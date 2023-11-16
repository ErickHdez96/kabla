(library
  (syntax expander base assert)
  (export keyword-assert)
  (import (rnrs base)
          (only (rnrs control)
                when)
          (only (srfi srfi-28)
                format)
          (only (conifer)
                conifer-tree->string)
          (only (syntax ast)
                make-ast-assert
                make-ast-unspecified)
          (only (syntax parse-tree)
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

  ;; Expands an `assert` expression
  ;; ( assert <expr> )
  (define keyword-assert
    (lambda (e node source-datum elems dot)
      (define last-span (close-delim-or-dot-span node))
      (define elems-length (length elems))

      (cond
	[(< elems-length 2)
	 (expand-emit-error
	   e
	   last-span
	   "expected an expression")
	 (make-ast-unspecified (pt-offset node) node source-datum)]
	[else
	  (when (> elems-length 2)
	    (expand-emit-error
	      e
	      (pt-span (caddr elems))
	      (format
		"expected ~a, found ~a"
		(expected-closing-delim node)
		(conifer-tree->string (caddr elems)))))
	  (let ([expr (expand-datum e (cadr elems) #f 'expr)])
	    (maybe-unexpected-dot e node dot)
	    (make-ast-assert
	      (pt-offset node)
	      node
	      expr
	      source-datum))]))))

