(library
  (syntax expander base quote)
  (export keyword-quote)
  (import (rnrs base)
	  (only (rnrs control)
		when)
	  (only (syntax ast)
		make-ast-error
		make-ast-boolean
		make-ast-char
		make-ast-null
		make-ast-string
		make-ast-symbol)
	  (only (syntax parse-tree)
		pt-span
		pt-atom?
		pt-boolean?
		pt-boolean-value
		pt-char?
		pt-identifier?
		pt-list?
		pt-string?
		pt-syntax-kind)
	  (only (syntax expander)
		expand-emit-error)
	  (only (syntax expander base common)
		close-delim-or-dot-span
		expected-closing-delim
		maybe-unexpected-dot)
	  (common))

  (define keyword-quote
    (lambda (e node elems dot)
      (define last-span (close-delim-or-dot-span node))
      (define elems-length (length elems))

      (cond
	[(= elems-length 1)
	 (expand-emit-error
	   e
	   (pt-span node)
	   "expected a datum to quote")
	 (make-ast-error (pt-span node) node)]
	[else
	  (when (> elems-length 2)
	    (expand-emit-error
	      e
	      (pt-span node)
	      "expected only one datum to quote"))
	  (let ([q (quote-datum e node (cadr elems))])
	    (maybe-unexpected-dot e node dot)
	    q)])))

  (define quote-datum
    (lambda (e node elem)
      (cond
	[(pt-atom? elem)
	 => (lambda (atom)
	      (cond
		[(pt-identifier? atom) => (lambda (ident)
					    (make-ast-symbol
					      (pt-span node)
					      node
					      ident))]
		[(pt-boolean? atom)
		 (make-ast-boolean
		   (pt-span node)
		   node
		   (pt-boolean-value atom))]
		[(pt-char? atom) => (lambda (char)
				      (make-ast-char
					(pt-span node)
					node
					char))]
		[(pt-string? atom) => (lambda (str)
					(make-ast-string
					  (pt-span node)
					  node
					  str))]
		[else (error 'quote-datum
			     "unknown atom ~a - ~a"
			     (pt-syntax-kind elem)
			     elem)]))]
	[(pt-list? elem)
	 => (lambda (elems)
	      (cond
		[(and (null? (car elems))
		      (null? (cdr elems)))
		 (make-ast-null (pt-span node) node)]
		[else (error 'quote-datum
			     "cannot quote lists yet ~a"
			     elem)]))]
	[else (error 'quote-datum
		     "unknown datum kind ~a - ~a"
		     (pt-syntax-kind elem)
		     elem)]))))

