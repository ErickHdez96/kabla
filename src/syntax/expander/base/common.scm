(library
  (syntax expander base common)
  (export maybe-unexpected-dot
          close-delim-or-dot-span
          expected-closing-delim)
  (import (rnrs base)
          (only (rnrs control)
                when)
          (only (rnrs lists)
                find)
          (only (conifer)
                conifer-red-children
                conifer-token-text)
          (only (syntax parse-tree)
                pt-span
                pt-dot?
                pt-close-delim?
                pt-open-delim?)
          (only (syntax expander)
                expand-emit-error))

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
