;;; # Expander module
;;;
;;; The parse tree needs to be expanded into an abstract syntax tree before it
;;; can be further processed. In the expansion process, keywords/syntactic
;;; keywords (macros) are invoked until the result reaches a core form.
;;;
;;; The expansion process is as follows:
;;;
;;; * macro use: The associated transformer is invoked, then the resulting form
;;;   is run through the expander again.
;;; * define-syntax form: The right-hand-side is evaluated into a transformer
;;;   and bound to the keyword.
;;; * define form: The identifier is marked as a variable, but the expansion of
;;;   its right-hand-side is deferred.
;;; * begin form: The subforms are spliced into the list of body forms it is
;;;   processing.
;;; * let-syntax or letrec-syntax form: Its inner body is spliced into the list
;;;   of (outer) body forms it is processing, arranging for the keywords bound
;;;   to be visible only in the inner body forms.
;;; * expression, i.e., nondefinition: The expression is expanded, then the
;;;   deferred right-hand-side expressions are expanded. The remaining
;;;   expressions in the body are expanded and an equivalent of a letrec* is
;;;   formed from the defined variables, expanded right-hand-side expressions
;;;   and expanded body expressions.
;;;
;;; ## Top-level body
;;;
;;; The expansion of a top-level body behaves mostly as described above with
;;; the following exceptions:
;;;
;;; * nondefinition: Its expansion is deferred and the expander continues with
;;;   the rest of the items.
;;; * once the end is reached: The deferred right-hand-side and body
;;;   expressions are expanded and the equivalent of a letrec* is formed from
;;;   the defined variables, expanded right-hand-side expressions, and expanded
;;;   body expressions. For each expression that appears before a variable
;;;   definition, a dummy binding is created at the corresponding place within
;;;   the set of letrec* bindings, with a fresh temporary variable on the
;;;   left-hand side and the equivalent of (begin <expression> <unspecified>).
(library
  (syntax expander)
  (export expand-parse-tree)
  (import (rnrs base)
	  (only (rnrs control)
		when)
	  (only (rnrs records syntactic)
		define-record-type)
	  (only (conifer)
		conifer-syntax-kind
		conifer-red-offset
		conifer-syntax-kind
		conifer-text-length)
	  (only (syntax parse-tree)
		pt-root-sexps
		pt-sexp?
		pt-atom?
		pt-boolean?
		pt-boolean-value
		pt-char?
		pt-string?
		pt-list?
		pt-vector?
		pt-bytevector?
		pt-abbreviation?)
	  (only (syntax ast)
		make-ast-root
		make-ast-expr)
	  (only (env)
		make-root-env
		make-child-env))

  (define-record-type
    expander
    (fields
      (mutable state)
      (mutable saved-states)
      (mutable errors)))

  ;; State that needs to be preserved when entering scopes.
  (define-record-type
    state
    (fields
      ;; Deferred data that still need to be expanded.
      (mutable deferred)
      ;; Keyword and value bindings encountered so far.
      (mutable bindings)))

  (define make-span cons)

  ;; Parses a top-level program from a red-tree.
  (define expand-parse-tree
    (lambda (pt)
      (define expander (make-expander
			 (make-state
			   '()
			   (make-root-env))
			 '()
			 '()))

      ; expand all top-level data. Libraries are collected and all other
      ; data are deferred.
      (define libraries (let loop ([pt-children (pt-root-sexps pt)]
				   [acc '()])
			  (cond
			    [(null? pt-children)
			     (reverse acc)]
			    [(pt-sexp? (car pt-children))
			     (cond
			       [(expand-top-level-datum expander
							(car pt-children))
				; only libraries are really returned from expand-top-level-datum,
				; all other data is deferred.
				=> (lambda (item) (loop (cdr pt-children)
							(cons item acc)))]
			       [else (loop (cdr pt-children)
					   acc)])]
			    [else (error 'expand-parse-tree
					 "shouldn't have arrived here")])))
      (define items (expand-deferred-top-level-items expander))

      (cons
	(make-ast-root items)
	(expander-errors expander))))



  ;; Expands `datum` in a <top-level> context.
  (define expand-top-level-datum
    (lambda (e datum)
      (cond
	; All expressions in the top-level are deferred until all data has
	; been seen.
	[(pt-atom? datum) => (lambda (atom) (defer-expr e atom))]
	[else (error 'expand-top-level-datum
		     "unknown datum kind: ~a"
		     (conifer-syntax-kind datum))])))

  (define expand-deferred-top-level-items
    (lambda (e)
      (let loop ([deferred-items (reverse (take-current-deferred! e))]
		 [acc '()])
	(if (null? deferred-items)
	  (reverse acc)
	  (cond
	    [(eq? 'expr (caar deferred-items))
	     (loop (cdr deferred-items)
		   (cons (expand-atom e
				      (cdar deferred-items))
			 acc))]
	    [else (error 'expand-deferred-top-level-items
			 "")])))))

  (define expand-sexp
    (lambda (e sexp)
      (cond
	[(pt-atom? sexp) => (lambda (atom) (expand-atom e atom))]
	[else (error 'expand-sexp
		     "unknown sexp kind: ~a"
		     (conifer-syntax-kind sexp))])))

  (define expand-atom
    (lambda (e atom)
      (cond
	[(pt-boolean? atom)
	 (make-ast-expr (make-span (conifer-red-offset atom)
				   (conifer-text-length atom))
			(pt-boolean-value atom))]
	[(pt-char? atom)
	 => (lambda (c) (make-ast-expr (make-span (conifer-red-offset atom)
						  (conifer-text-length atom))
				       c))]
	[(pt-string? atom)
	 => (lambda (c) (make-ast-expr (make-span (conifer-red-offset atom)
						  (conifer-text-length atom))
				       c))]
	[else (error 'expand-atom
		     "unknown atom kind: ~a"
		     (conifer-syntax-kind atom))])))

  ;; Pushes `expr` into the deferred list.
  (define defer-expr
    (lambda (e expr)
      (let ([state (expander-state e)])
	(state-deferred-set!
	  state
	  (cons (cons 'expr expr)
		(state-deferred state))))))

  ;; Returns the current active environment.
  (define current-env
    (lambda (e)
      (state-bindings (expander-state e))))

  ;; Removes and returns the current deferred data.
  (define take-current-deferred!
    (lambda (e)
      (let* ([state (expander-state e)]
	     [d (state-deferred state)])
	(state-deferred-set!
	  state
	  '())
	d)))

  ;; Saves the current state and creates a new one for entering a lambda.
  (define enter-scope
    (lambda (e)
      (expander-saved-states-set!
	e
	(cons (expander-state e)
	      (expander-saved-states e)))
      (expander-state-set!
	e
	(make-state
	  '()
	  (make-child-env
	    (current-env e))))))

  ;; Discards the current state, pops the last saved state and sets it as the
  ;; active one.
  (define exit-scope
    (lambda (e)
      (when (null? (expander-saved-states e))
	(assertion-violation
	  'exit-scope
	  "tried to exit from the root scope"))
      (expander-state-set!
	e
	(car (expander-saved-states e)))
      (expander-saved-states-set!
	e
	(cdr (expander-saved-states e))))))
