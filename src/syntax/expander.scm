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
  (export expand-parse-tree
	  (rename (enter-scope expand-enter-scope)
		  (exit-scope expand-exit-scope)))
  (import (rnrs base)
	  (only (rnrs control)
		when)
	  (only (rnrs lists)
		assq)
	  (only (rnrs records syntactic)
		define-record-type)
	  (only (conifer)
		conifer-syntax-kind
		conifer-red-parent
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
		pt-identifier?
		pt-list?
		pt-vector?
		pt-bytevector?
		pt-abbreviation?)
	  (only (syntax ast)
		make-ast-root
		make-ast-expr
		make-ast-proc-call)
	  (only (env)
		make-root-env
		make-child-env
		env-lookup)
	  (common))

  (define-record-type
    expander
    (fields
      (mutable state)
      (mutable saved-states)
      importer
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

  ;; Parses a top-level program from a red-tree `pt`.
  ;; (: expand-parse-tree (-> ParseTree [ConfigArgs] Pair[AST, List[Error]]))
  ;;
  ;; ConfigArgs is an optional association list to modify the behaviour of the
  ;; expander:
  ;; * importer: `(: importer
  ;;		   (-> ModuleName
  ;;		       Either[Env[Symbol, Binding] | Error]))` - The importer is
  ;;   a function that receives a module name and returns and environment with
  ;;   the exported bindings from the module (we only really care about
  ;;   keywords), or an Error, if the module doesn't exist.
  ;; * intrinsic-env: `(: intrinsic-env Env[Symbol, Binding])` - An environment
  ;;   with the intrinsic keywords every file starts with (e.g. `import` and
  ;;   `library`). See the `(syntax expander intrinsics)` module to see the
  ;;   structure of intrinsic keywords.
  (define expand-parse-tree
    (lambda (pt . extra-args)
      (define expander
	(let ([importer (fmap cdr (assq 'importer extra-args))]
	      [intrinsic-env (fmap cdr (assq 'intrinsic-env extra-args))])
	  (make-expander
	    (make-state
	      '()
	      (if intrinsic-env
		(make-child-env
		  intrinsic-env)
		(make-root-env)))
	    '()
	    (or importer
		(lambda _ (err "no importer was provided")))
	    '())))

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
      (define items (expand-deferred-items expander))

      (cons
	(make-ast-root items)
	(expander-errors expander))))

  ;; Expands `datum` in a <top-level> context.
  (define expand-top-level-datum
    (lambda (e datum)
      (cond
	; All expressions in the top-level are deferred until all data has
	; been seen.
	[(pt-atom? datum) => (lambda (atom) (defer-datum e 'atom atom))]
	[(pt-list? datum) => (lambda (data) (expand-top-level-list e datum data))]
	[else (error 'expand-top-level-datum
		     "unknown datum kind: ~a"
		     (conifer-syntax-kind datum))])))

  (define expand-top-level-list
    (lambda (e parent data)
      (cond
	[(null? data)
	 (emit-error-with-hint
	   e
	   (parse-tree->span parent)
	   "empty lists aren't allowed"
	   (make-hint "try '()"))]
	[(fmap
	   (lambda (id) (lookup e id))
	   (pt-identifier? (car data)))
	 => (lambda (binding)
	      (error 'expand-list "unknown binding ~a" binding))]
	[else (defer-datum e 'list data)])))

  (define expand-deferred-items
    (lambda (e)
      (let loop ([deferred-items (reverse (take-current-deferred! e))]
		 [acc '()])
	(if (null? deferred-items)
	  (reverse acc)
	  (cond
	    [(eq? 'atom (caar deferred-items))
	     (loop (cdr deferred-items)
		   (cons (expand-deferred-atom e
				      (cdar deferred-items))
			 acc))]
	    [(eq? 'list (caar deferred-items))
	     (loop (cdr deferred-items)
		   (cons (expand-deferred-list e
					       #f
					       (cdar deferred-items))
			 acc))]
	    [else (error 'expand-deferred-items
			 "unknown datum to defer ~a"
			 (caar deferred-items))])))))

  (define expand-deferred-sexp
    (lambda (e sexp)
      (cond
	[(pt-atom? sexp) => (lambda (atom) (expand-deferred-atom e atom))]
	[(pt-list? sexp) => (lambda (data) (expand-deferred-list e sexp data))]
	[else (error 'expand-sexp
		     "unknown sexp kind: ~a"
		     (conifer-syntax-kind sexp))])))

  ;; Expands the inner child of an atom red-tree.
  (define expand-deferred-atom
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
	 => (lambda (s) (make-ast-expr (make-span (conifer-red-offset atom)
						  (conifer-text-length atom))
				       s))]
	[(pt-identifier? atom)
	 => (lambda (v) (make-ast-expr (make-span (conifer-red-offset atom)
						  (conifer-text-length atom))
				       v))]
	[else (error 'expand-atom
		     "unknown atom kind: ~a"
		     (conifer-syntax-kind atom))])))

  (define expand-deferred-list
    (lambda (e parent data)
      (cond
	[(null? data)
	 (when (not parent)
	   (error 'expand-deferred-list
		  "shouldn't defer an empty list"))
	 (emit-error-with-hint
	   e
	   (parse-tree->span parent)
	   "empty lists aren't allowed"
	   (make-hint "try '()"))
	 (make-ast-expr
	   (parse-tree->span parent)
	   '())]
	; if the first atom of the list is a keyword, expand it.
	[(and-then
	   (car data)
	   pt-atom?
	   pt-identifier?
	   [-> (id) (lookup e id)])
	 => (lambda (binding)
	      (error 'expand-deferred-list "unknown binding ~a" binding))]
	[else (let ([parent (conifer-red-parent (car data))])
		(make-ast-proc-call
		  (cons (conifer-red-offset parent)
			(conifer-text-length parent))
		  (expand-deferred-sexp e (car data))
		  (map
		    (lambda (d) (expand-deferred-sexp e d))
		    (cdr data))))])))

  ;; Pushes `expr` into the deferred list.
  (define defer-datum
    (lambda (e kind expr)
      (let ([state (expander-state e)])
	(state-deferred-set!
	  state
	  (cons (cons kind expr)
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

  (define emit-error
    (lambda (e span msg)
      (expander-errors-set!
	e
	(cons
	  (list span msg)
	  (expander-errors e)))))

  (define emit-error-with-hint
    (lambda (e span msg hint)
      (expander-errors-set!
	e
	(cons
	  (list span msg hint)
	  (expander-errors e)))))

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
	(cdr (expander-saved-states e)))))

  (define parse-tree->span
    (lambda (t)
      (cons (conifer-red-offset t)
	    (conifer-text-length t))))

  ;; Looks up `id` in the current environment (recursively) and returns its
  ;; binding or `#f`.
  (define lookup
    (lambda (e id)
      (env-lookup
	(current-env e)
	id))))
