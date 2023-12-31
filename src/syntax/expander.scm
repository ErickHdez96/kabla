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
	  expand-datum
	  expand-deferred-items
	  (rename (enter-scope expand-enter-scope)
		  (exit-scope expand-exit-scope)
		  (emit-error expand-emit-error)
		  (take-items! expand-take-items!)
		  (push-item! expand-push-item!)))
  (import (rnrs base)
	  (only (rnrs control)
		when)
	  (only (rnrs lists)
		assq
		find
		cons*)
	  (only (rnrs records syntactic)
		define-record-type)
	  (only (srfi srfi-28)
		format)
	  (only (conifer)
		conifer-green-node-builder
		conifer-tree->string
		conifer-syntax-kind
		conifer-red-tree?
		conifer-red-parent
		conifer-red-offset
		conifer-red-children
		conifer-red-tree-green
		conifer-make-view
		conifer-syntax-kind
		conifer-text-length
		conifer-push-node
		conifer-push-token
		conifer-start-node
		conifer-finish-node
		conifer-finish-builder)
	  (only (syntax parse-tree)
		pt-root-sexps
		pt-abbreviation?
		pt-sexp?
		pt-atom?
		pt-boolean?
		pt-boolean-value
		pt-char?
		pt-string?
		pt-identifier?
		pt-list?
		pt-vector?
		pt-dot?
		pt-vector?
		pt-bytevector?
		pt-abbreviation?
		pt-offset
		pt-span)
	  (only (syntax ast)
		make-ast-root
		make-ast-boolean
		make-ast-char
		make-ast-string
		make-ast-null
		make-ast-list
		make-ast-vector
		make-ast-proc-call
		make-ast-var
		make-ast-unspecified)
	  (only (env)
		make-root-env
		make-child-env
		env-lookup
		env-insert!)
	  (common))

  (define-record-type
    expander
    (fields
      (mutable state)
      (mutable saved-states)
      green-node-builder
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
	(let ([importer (and-then (assq 'importer extra-args)
				  cdr)]
	      [intrinsic-env (and-then (assq 'intrinsic-env extra-args)
				       cdr)]
	      [green-node-builder (and-then (assq 'green-node-builder extra-args)
					    cdr)])
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
	    ; green node builder
	    (or green-node-builder
		(conifer-green-node-builder))
	    ; importer
	    (or importer
		(lambda _ (err "no importer was provided")))
	    ; errors
	    '())))

      (for-each
	(lambda (d) (expand-datum expander d #f 'body))
	(pt-root-sexps pt))
      (expand-deferred-items expander)

      (cons
	(make-ast-root (take-items! expander) pt)
	(reverse (expander-errors expander)))))

  (define expand-datum
    (lambda (e datum source-datum ctx)
      (cond
	[(pt-atom? datum) => (lambda (atom)
			       (case ctx
				 [(body) (defer-datum e 'datum datum source-datum)]
				 [(expr) (expand-atom e atom source-datum)]
				 [else (error
					 'expand-datum
					 "unexpected context ~a"
					 ctx)]))]
	[(pt-list? datum) => (lambda (lst) (expand-list
					     e
					     datum
					     source-datum
					     lst
					     ctx))]
	[(pt-vector? datum) => (lambda (elems)
				 (case ctx
				   [(body) (defer-datum e 'datum datum source-datum)]
				   [(expr) (expand-vector
					     e
					     datum
					     source-datum
					     elems)]
				   [else (error
					   'expand-datum
					   "unexpected context ~a"
					   ctx)]))]
	[(pt-abbreviation? datum)
	 => (lambda (abb)
	      (let* ([builder (expander-green-node-builder e)]
		     [new-parent (begin
				   (conifer-start-node 
				     builder
				     'list)
				   (conifer-push-token builder 'open-delim "(")
				   (conifer-start-node builder 'atom)
				   (case (conifer-syntax-kind (car abb))
				     [(quote) (conifer-push-token builder 'identifier "quote")]
				     [else (error 'expand-datum
						  "unexpected abbreviation ~a - ~a"
						  (conifer-syntax-kind datum)
						  datum)])
				   (conifer-finish-node builder)
				   (conifer-push-token builder 'whitespace " ")
				   (if (cdr abb)
				     (conifer-push-node builder (conifer-red-tree-green (cdr abb)))
				     (conifer-push-token builder 'identifier (format
									      "<error - ~a>"
									      (conifer-tree->string datum))))
				   (conifer-push-token builder 'close-delim ")")
				   (conifer-finish-node builder)
				   (conifer-make-view (conifer-finish-builder builder)
						      (conifer-red-offset datum)))])
		(expand-datum e new-parent (conifer-red-tree-green datum) ctx)))]
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
	    [(datum) (and-then (expand-datum e (cadr d) (cddr d) 'expr)
			       [-> i (push-item! e i)])]
	    [(def)
	     (let ([elems (pt-list? (cadr d))])
	       (cond
		 [(and elems
		       (keyword-def-list? e (car elems)))
		  => (lambda (transformer)
		       (and-then (transformer
				   e
				   (cadr d)
				   (cddr d)
				   (car elems)
				   (and (not (null? (cdr elems)))
					(cadr elems)))
				 [-> i (push-item! e i)]))]
		 [else (error 'expand-deferred-items
			      "invalid deferred definition ~a"
			      d)]))]
	    [else (error
		    'expand-deferred-items
		    "unknown deferred item ~a: ~a"
		    (car d)
		    d)]))
	(take-current-deferred! e))))

  ;; Expands the inner child of an atom red-tree.
  (define expand-atom
    (lambda (e atom source-datum)
      (let ([offset (pt-offset atom)])
	(cond
	  [(pt-boolean? atom) (make-ast-boolean offset atom (pt-boolean-value atom) source-datum)]
	  [(pt-char? atom) => (lambda (c) (make-ast-char offset atom c source-datum))]
	  [(pt-string? atom) => (lambda (s) (make-ast-string offset atom s source-datum))]
	  [(pt-identifier? atom) => (lambda (v) (make-ast-var offset atom v source-datum))]
	  [else (error 'expand-atom
		       "unknown atom kind: ~a"
		       (conifer-syntax-kind atom))]))))

  (define expand-list
    (lambda (e parent source-datum lst ctx)
      (let ([before-dot (car lst)]
	    [after-dot (cdr lst)]
	    [offset (pt-offset parent)])
	(cond
	  [(null? before-dot)
	   (case ctx
	     [(expr)
	      ; only emit the error if the list _is_ empty, errors
	      ; for invalid characters, extra dots or the like
	      ; are generated elsewhere.
	      (when (vector-length (conifer-red-children parent))
		(emit-error-with-hint
		  e
		  (pt-span parent)
		  "empty lists not allowed"
		  (make-hint "try '()")))

	      (make-ast-null offset parent)]
	     [else (defer-datum e 'datum parent source-datum)])]

	  [else
	    (case ctx
	      ; everything gets deferred in the top level
	      ; for error recovery purposes, also in the body.
	      [(body)
	       (if (keyword-def-list? e before-dot)
		 (defer-define e parent before-dot source-datum)
		 (defer-datum e 'datum parent source-datum))]
	      ; in an expression context we expand everything
	      [(expr)
	       (cond
		 [(keyword-def-list? e before-dot)
		  (emit-error
		    e
		    (pt-span parent)
		    "definitions are not allowed in this context")
		  ; we still defer it for error recovery purposes
		  (defer-define e parent before-dot source-datum)
		  (make-ast-unspecified
		    (pt-offset parent)
		    parent
		    source-datum)]
		 [(keyword-expr-list? e before-dot)
		  => (lambda (transformer)
		       (transformer e
				    parent
				    source-datum
				    before-dot
				    (if (null? after-dot)
				      #f
				      (car after-dot))))]
		 [else
		   (let ([elems (map (lambda (d) (expand-datum e d #f 'expr))
				     before-dot)])
		     (when (not (null? after-dot))
		       (emit-error
			 e
			 (pt-span
			   (find
			     pt-dot?
			     (vector->list (conifer-red-children parent))))
			 "dot '.' not allowed in this context"))
		     (make-ast-proc-call
		       offset
		       parent
		       (car elems)
		       (cdr elems)
		       source-datum))])]
	      [else (error 'expand-list
			   "can't expand list ~a"
			   parent)])]))))

  (define expand-vector
    (lambda (e parent source-datum elems)
      (let ([expanded-elems (map
			      (lambda (d)
				(expand-datum
				  e
				  d
				  #f
				  'expr))
			      elems)])
	(make-ast-vector
	  (pt-offset parent)
	  parent
	  expanded-elems
	  source-datum))))

  ;; Returns the associated transformer if the first element of `elems` is an
  ;; expression keyword (e.g. `if`, `lamdba`, etc.)
  (define keyword-expr-list?
    (lambda (e elems)
      (and (not (null? elems))
	   (and-then (pt-atom? (car elems))
		     pt-identifier?
		     [-> id (lookup e id)]
		     [-> binding (and (eq? 'keyword-expr
					   (car binding))
				      (cdr binding))]))))

  ;; Returns the associated transformer if the first element of `elems` is a
  ;; definition keyword (i.e. `define`).
  (define keyword-def-list?
    (lambda (e elems)
      (and (not (null? elems))
	   (and-then (pt-atom? (car elems))
		     pt-identifier?
		     [-> id (lookup e id)]
		     [-> binding (and (eq? 'keyword-def
					   (car binding))
				      (cdr binding))]))))

  ;; Pushes `expr` into the deferred list.
  (define defer-datum
    (lambda (e kind datum source-datum)
      (let ([state (expander-state e)])
	(state-deferred-set!
	  state
	  (cons (cons* kind datum source-datum)
		(state-deferred state)))
	#f)))

  ;; Defers the define node `parent` to be lowered later.
  (define defer-define
    (lambda (e parent elems source-datum)
      (when (>= (length elems) 2)
	(cond
	  [(or (and-then (pt-atom? (cadr elems))
			 pt-identifier?)
	       (and-then (pt-list? (cadr elems))
			 [-> children (and (not (null? children))
					   (pt-atom? (car children)))]
			 pt-identifier?))
	   => (lambda (name)
		(insert! e
			 name
			 'value))]))
      (defer-datum e 'def parent source-datum)))

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
	(cdr (expander-saved-states e)))))

  (define emit-error
    (lambda (e span msg)
      (expander-errors-set!
	e
	(cons
	  (list span msg)
	  (expander-errors e)))
      #f))

  (define emit-error-with-hint
    (lambda (e span msg hint)
      (expander-errors-set!
	e
	(cons
	  (list span msg hint)
	  (expander-errors e)))
      #f))

  ;; Looks up `id` in the current environment (recursively) and returns its
  ;; binding or `#f`.
  (define lookup
    (lambda (e id)
      (env-lookup
	(current-env e)
	id)))

  ;; Inserts a new binding from `id` to `binding` in the current environment.
  (define insert!
    (lambda (e id binding)
      (env-insert!
	(current-env e)
	id
	binding))))
