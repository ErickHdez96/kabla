(library
  (syntax expander base set!)
  (export keyword-set!)
  (import (rnrs base)
	  (only (rnrs control)
		when)
	  (only (srfi srfi-28)
		format)
	  (only (conifer)
		conifer-tree->string)
	  (only (syntax ast)
		ast-expr?
		make-ast-set!
		make-ast-unspecified
		make-ast-identifier)
	  (only (syntax parse-tree)
		pt-atom?
		pt-identifier?
		pt-span
		pt-offset)
	  (only (syntax expander)
		expand-datum
		expand-emit-error)
	  (only (syntax expander base common)
		maybe-unexpected-dot
		close-delim-or-dot-span
		expected-closing-delim)
	  (common))

  ;; Expands an `set!` expression
  ;; ( set! <ident> <expr> )
  (define keyword-set!
    (lambda (e node source-datum elems dot)
      (define last-span (close-delim-or-dot-span node))
      (define elems-length (length elems))

      (cond
	[(< elems-length 2)
	 (expand-emit-error
	   e
	   last-span
	   "expected a variable")
	 (make-ast-unspecified (pt-offset node) node source-datum)]
	[(< elems-length 3)
	 (when (not (and-then (pt-atom? (cadr elems))
			      pt-identifier?))
	   (expand-emit-error
	     e
	     (pt-span (cadr elems))
	     (format
	       "expected a variable, found ~a"
	       (conifer-tree->string (cadr elems)))))
	 (expand-emit-error
	   e
	   last-span
	   "expected an expression")
	 (make-ast-unspecified (pt-offset node) node source-datum)]
	[else
	  (when (> elems-length 3)
	    (expand-emit-error
	      e
	      (pt-span (cadddr elems))
	      (format
		"expected ~a, found ~a"
		(expected-closing-delim node)
		(conifer-tree->string (cadddr elems)))))
	  (let ([var (cond
		       [(and-then (pt-atom? (cadr elems))
				  pt-identifier?)
			=> (lambda (var)
			     (make-ast-identifier
			       (pt-offset (cadr elems))
			       (cadr elems)
			       var))]
		       [else
			 (expand-emit-error
			   e
			   (pt-span (cadr elems))
			   (format
			     "expected a variable, found ~a"
			     (conifer-tree->string (cadr elems))))
			 (make-ast-identifier
			   (pt-offset (cadr elems))
			   (cadr elems)
			   (string->symbol
			     (format "|~a|"
				     (conifer-tree->string (cadr elems)))))])]
		[expr (expand-datum e (caddr elems) #f 'expr)])
	    (maybe-unexpected-dot e node dot)
	    (assert (ast-expr? expr))
	    (make-ast-set!
	      (pt-offset node)
	      node
	      var
	      expr
	      source-datum))]))))
