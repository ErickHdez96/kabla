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
		assq
		find)
	  (only (rnrs records syntactic)
		define-record-type)
	  (only (conifer)
		conifer-syntax-kind
		conifer-red-parent
		conifer-red-offset
		conifer-red-children
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
		pt-dot?
		pt-vector?
		pt-bytevector?
		pt-abbreviation?)
	  (only (syntax ast)
		make-ast-root
		make-ast-expr
		make-ast-list
		make-ast-identifier)
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
      (mutable items)
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
	      ; items
	      '()
	      ; deferred
	      '()
	      ; bindings
	      (if intrinsic-env
		(make-child-env
		  intrinsic-env)
		(make-root-env)))
	    ; saved-states
	    '()
	    ; importer
	    (or importer
		(lambda _ (err "no importer was provided")))
	    ; errors
	    '())))

      (for-each
	(lambda (d) (expand-datum expander d 'top-level))
	(pt-root-sexps pt))
      (expand-deferred-items expander)

      (cons
	(make-ast-root (take-items! expander))
	(reverse (expander-errors expander)))))

  (define expand-datum
    (lambda (e datum ctx)
      (cond
	[(pt-atom? datum) => (lambda (atom)
			       (case ctx
				 [(top-level) (defer-datum e 'datum datum)]
				 [(any) (expand-atom e atom ctx)]
				 [else (error
					 'expand-datum
					 "unexpected context ~a"
					 ctx)]))]
	[(pt-list? datum) => (lambda (lst) (expand-list e datum lst ctx))]
	[else (error
		'expand-datum
		"unexpected datum ~a: ~a"
		(conifer-syntax-kind datum)
		datum)])))

  (define expand-deferred-items
    (lambda (e)
      (for-each
	(lambda (d)
	  (case (car d)
	    [(datum) (and-then
		       (expand-datum e (cdr d) 'any)
		       [-> i (push-item! e i)])]
	    [else (error
		    'expand-deferred-items
		    "unknown deferred item ~a: ~a"
		    (car d)
		    d)]))
	(take-current-deferred! e))))

  (define expand-deferred-sexp
    (lambda (e sexp)
      (cond
	[(pt-atom? sexp) => (lambda (atom) (expand-deferred-atom e atom))]
	;[(pt-list? sexp) => (lambda (data) (expand-deferred-list e sexp data))]
	[else (error 'expand-sexp
		     "unknown sexp kind: ~a"
		     (conifer-syntax-kind sexp))])))

  ;; Expands the inner child of an atom red-tree.
  (define expand-atom
    (lambda (e atom ctx)
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
	 => (lambda (v) (make-ast-identifier (make-span (conifer-red-offset atom)
						  (conifer-text-length atom))
				       v))]
	[else (error 'expand-atom
		     "unknown atom kind: ~a"
		     (conifer-syntax-kind atom))])))

  (define expand-list
    (lambda (e parent lst ctx)
      (let ([before-dot (car lst)]
	    [after-dot (cdr lst)]
	    [span (parse-tree->span parent)])
	(cond
	  [(null? before-dot)
	   (case ctx
	     [(any)
	      ; only emit the error if the list _is_ empty, errors
	      ; for invalid characters, extra dots or the like
	      ; are generated elsewhere.
	      (when (length (conifer-red-children parent))
		(emit-error-with-hint
		  e
		  span
		  "empty lists not allowed"
		  (make-hint "try '()")))

	      ; Expand data after the `.` to generate error messages
	      (for-each
		(lambda (ad-datum) (expand-datum e ad-datum 'any))
		after-dot)
	      (make-ast-expr
		span
		'())]
	     [else (defer-datum e 'datum parent)])]

	  [else (case ctx
		  [(top-level) (defer-datum e 'datum parent)]
		  [(any)
		   (let ([elems (map (lambda (d) (expand-datum e d 'any))
				     before-dot)])
		     (when (not (null? after-dot))
		       (emit-error
			 e
			 (parse-tree->span
			   (find
			     pt-dot?
			     (conifer-red-children parent)))
			 "dot '.' not allowed in this context")
		       (for-each
			 (lambda (ad-datum) (expand-datum e ad-datum 'any))
			 after-dot))
		     (make-ast-list
		       span
		       elems))]
		  [else (error 'expand-list
			       "can't expand list ~a"
			       parent)])]))))

  ;; Pushes `expr` into the deferred list.
  (define defer-datum
    (lambda (e kind datum)
      (let ([state (expander-state e)])
	(state-deferred-set!
	  state
	  (cons (cons kind datum)
		(state-deferred state)))
	#f)))

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

  ;; Removes and returns the current items
  (define take-items!
    (lambda (e)
      (let* ([state (expander-state e)]
	     [i (state-items state)])
	(state-items-set!
	  state
	  '())
	i)))

  ;; Pushes an expanded item to the expander
  (define push-item!
    (lambda (e item)
      (let ([state (expander-state e)])
	(state-items-set!
	  state
	  (cons item
		(state-items state))))))

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
