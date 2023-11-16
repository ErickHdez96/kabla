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
		pt-offset
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
    (lambda (e node source-datum elems dot)
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
	  (let ([q (quote-datum e node source-datum (cadr elems))])
	    (maybe-unexpected-dot e node dot)
	    q)])))

  (define quote-datum
    (lambda (e node source-datum elem)
      (cond
	[(pt-atom? elem)
	 => (lambda (atom)
	      (cond
		[(pt-identifier? atom) => (lambda (ident)
					    (make-ast-symbol
					      (pt-offset node)
					      node
					      ident
					      source-datum))]
		[(pt-boolean? atom)
		 (make-ast-boolean
		   (pt-offset node)
		   node
		   (pt-boolean-value atom)
		   source-datum)]
		[(pt-char? atom) => (lambda (char)
				      (make-ast-char
					(pt-offset node)
					node
					char
					source-datum))]
		[(pt-string? atom) => (lambda (str)
					(make-ast-string
					  (pt-offset node)
					  node
					  str
					  source-datum))]
		[else (error 'quote-datum
			     "unknown atom ~a - ~a"
			     (pt-syntax-kind elem)
			     elem)]))]
	[(pt-list? elem)
	 => (lambda (elems)
	      (cond
		[(and (null? (car elems))
		      (null? (cdr elems)))
		 (make-ast-null (pt-offset node) node source-datum)]
		[else (error 'quote-datum
			     "cannot quote lists yet ~a"
			     elem)]))]
	[else (error 'quote-datum
		     "unknown datum kind ~a - ~a"
		     (pt-syntax-kind elem)
		     elem)]))))

