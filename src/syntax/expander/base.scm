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
		conifer-tree->string
		conifer-red-children)
	  (only (syntax ast)
		make-ast-unspecified
		make-ast-if
		make-ast-define
		make-ast-identifier)
	  (only (syntax parse-tree)
		pt-atom?
		pt-identifier?
		pt-list?
		pt-dot?
		pt-open-delim?
		pt-close-delim?
		pt-span)
	  (only (syntax expander)
		expand-datum
		expand-emit-error)
	  (only (env)
		make-root-env
		env-insert!)
	  (common))

  ;; Environment with all the keywords from (rnrs base).
  (define RNRS-BASE-ENV
    (let ([env (make-root-env)])
      (env-insert!
	env
	'if
	(cons
	  'keyword-expr
	  keyword-if))

      (env-insert!
	env
	'define
	(cons
	  'keyword-def
	  keyword-define))
      env))

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
	      (let ([expr (and (>= elems-length 3)
			       (expand-datum e (caddr elems) 'expr))])
		(when (>= elems-length 4)
		  (expand-emit-error
		    e
		    (pt-span (cadddr elems))
		    (format "expected ~a, found ~a"
			    (expected-closing-delim node)
			    (conifer-tree->string (car (cdddr elems))))))
		(maybe-unexpected-dot e node dot)
		(make-ast-define
		  (pt-span node)
		  (make-ast-identifier
		    (pt-span (cadr elems))
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
				  (make-ast-unspecified
				    (pt-span node))])]
	     [true (cond
		     [(>= elems-length 3)
		      (expand-datum e (caddr elems) 'expr)]
		     [else (expand-emit-error
			     e
			     last-span
			     "expected a true branch")
			   (make-ast-unspecified
			     (pt-span node))])]
	     [false (cond
		      [(>= elems-length 4)
		       (expand-datum e (cadddr elems) 'expr)]
		      [else (make-ast-unspecified
			      (pt-span node))])])

	(when (>= elems-length 5)
	  (expand-emit-error
	    e
	    (pt-span (car (cddddr elems)))
	    (format "expected ~a, found ~a"
		    (expected-closing-delim node)
		    (conifer-tree->string (car (cddddr elems))))))

	(maybe-unexpected-dot e node dot)

	(make-ast-if
	  (pt-span node)
	  conditional
	  true
	  false))))

  ;; Emits an error if `dot` is not `#f`. `.` is not allowed in base
  ;; keywords
  (define maybe-unexpected-dot
    (lambda (e node dot)
      (when dot
	(expand-emit-error
	  e
	  (pt-span (find pt-dot? (conifer-red-children node)))
	  "dot '.' not allowed in this context"))))

  ;; Returns the span of the left-most `.`, closing delimiter, or the
  ;; right-most child of `node`.
  (define close-delim-or-dot-span
    (lambda (node)
      (let search ([elems (conifer-red-children node)])
	(cond
	  [(null? elems)
	   (error 'rparen-or-dot
		  "should have returned a span by now")]
	  [(null? (cdr elems))
	   (pt-span (car elems))]
	  [(pt-close-delim? (car elems)) => pt-span]
	  [(pt-dot? (car elems)) => pt-span]
	  [else (search (cdr elems))]))))

  ;; Returns a character representing the expected closing delimiter for `node`
  ;; depending on its opening delimiter. For error purposes.
  (define expected-closing-delim
    (lambda (node)
      (let ([children (conifer-red-children node)])
	(assert (not (null? children)))
	(cond
	  [(pt-open-delim? (car children))
	   => (lambda (od)
		(let ([od (conifer-token-text od)])
		  (cond
		    [(string=? "(" od) #\)]
		    [(string=? "[" od) #\]]
		    [(string=? "{" od) #\}]
		    [else (error 'expected-closing-delim
				 "invalid open delimiter ~a"
				 od)])))]
	  [else (error 'expected-closing-delim
		       "invalid open delimiter ~a"
		       (car children))])))))
