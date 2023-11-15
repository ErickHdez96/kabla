(library
  (syntax expander base)
  (export RNRS-BASE-ENV)
  (import (rnrs base)
	  (only (rnrs control)
		when)
	  (only (rnrs lists)
		find)
	  (only (srfi srfi-28)
		format)
	  (only (conifer)
		conifer-token-text
		conifer-tree->string)
	  (only (syntax ast)
		make-ast-unspecified
		make-ast-if
		make-ast-define
		make-ast-identifier
		make-ast-lambda
		make-ast-let
		ast-define?)
	  (only (syntax parse-tree)
		pt-atom?
		pt-identifier?
		pt-list?
		pt-dot?
		pt-open-delim?
		pt-close-delim?
		pt-span
		pt-offset)
	  (only (syntax expander)
		expand-datum
		expand-emit-error
		expand-take-items!)
	  (only (syntax expander base lambda)
		keyword-lambda)
	  (only (syntax expander base quote)
		keyword-quote)
	  (only (syntax expander base common)
		close-delim-or-dot-span
		expected-closing-delim
		maybe-unexpected-dot)
	  (only (env)
		make-root-env
		env-insert!)
	  (common))

  ;; Expands a `define` definition.
  ;; <define> → ( define <var> <expr>? )
  ;;	      | ( define ( <var> <formals> ) <expr> )
  ;;	      | ( define ( <var> . <formal> ) <expr> )
  (define keyword-define
    (lambda (e node elems dot)
      (define last-span (close-delim-or-dot-span node))
      (define elems-length (length elems))

      (cond
	[(< elems-length 2)
	 (expand-emit-error
	   e
	   last-span
	   "expected a variable name or open delimiter")]
	[(and-then (pt-atom? (cadr elems))
		   pt-identifier?)
	 => (lambda (var)
	      (let ([expr (or (and (>= elems-length 3)
				   (expand-datum e (caddr elems) 'expr))
			      (make-ast-unspecified (pt-offset node) node))])
		(when (>= elems-length 4)
		  (expand-emit-error
		    e
		    (pt-span (cadddr elems))
		    (format "expected ~a, found ~a"
			    (expected-closing-delim node)
			    (conifer-tree->string (car (cdddr elems))))))
		(maybe-unexpected-dot e node dot)
		(make-ast-define
		  (pt-offset node)
		  node
		  (make-ast-identifier
		    (pt-offset (cadr elems))
		    (cadr elems)
		    var)
		  expr)))]
	[(pt-list? (cadr elems))
	 (expand-emit-error
	   e
	   (pt-span (cadr elems))
	   "function short-hand not supported yet")]
	[else
	  (expand-emit-error
	    e
	    (pt-span (cadr elems))
	    (format "expected an identifier or an open delimiter, found ~a"
		    (conifer-tree->string (cadr elems))))])))

  ;; Expands an `if` expression
  ;; ( if <expr> <expr> <expr>? )
  (define keyword-if
    (lambda (e node elems dot)
      ; the span of the r-paren, dot, or last element
      (let* ([last-span (close-delim-or-dot-span node)]
	     [elems-length (length elems)]
	     ; the first element is the keyword `if`
	     [conditional (cond
			    [(>= elems-length 2)
			     (expand-datum e (cadr elems) 'expr)]
			    [else (expand-emit-error
				    e
				    last-span
				    "expected a condition")
				  (make-ast-unspecified (pt-offset node) node)])]
	     [true (cond
		     [(>= elems-length 3)
		      (expand-datum e (caddr elems) 'expr)]
		     [else (expand-emit-error
			     e
			     last-span
			     "expected a true branch")
			   (make-ast-unspecified (pt-offset node) node)])]
	     [false (cond
		      [(>= elems-length 4)
		       (expand-datum e (cadddr elems) 'expr)]
		      [else (make-ast-unspecified (pt-offset node) node)])])

	(when (>= elems-length 5)
	  (expand-emit-error
	    e
	    (pt-span (car (cddddr elems)))
	    (format "expected ~a, found ~a"
		    (expected-closing-delim node)
		    (conifer-tree->string (car (cddddr elems))))))

	(maybe-unexpected-dot e node dot)

	(make-ast-if
	  (pt-offset node)
	  node
	  conditional
	  true
	  false))))

  ;; Environment with all the keywords from (rnrs base).
  (define RNRS-BASE-ENV
    (let ([env (make-root-env)])
      (env-insert!
	env
	'define
	(cons
	  'keyword-def
	  keyword-define))

      (env-insert!
	env
	'if
	(cons
	  'keyword-expr
	  keyword-if))     

      (env-insert!
	env
	'lambda
	(cons
	  'keyword-expr
	  keyword-lambda))     

      (env-insert!
	env
	'quote
	(cons
	  'keyword-expr
	  keyword-quote))
      env)))
