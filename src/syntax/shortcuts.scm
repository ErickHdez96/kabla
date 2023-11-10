(library
  (syntax shortcuts)
  (export parse-str
	  expand-str)
  (import (rnrs base)
	  (conifer)
	  (syntax scanner)
	  (syntax parser)
	  (syntax expander)
	  (syntax records))

  ;; Scans and parses a string into a parse tree.
  (define parse-str
    (lambda (s)
      (parse-tokens (scan-string s))))

  ;; Parses a string and expands the parse tree.
  (define expand-str
    (lambda (s)
      (let* ([parse-result (parse-str s)]
	     [expand-result (expand-parse-tree
			      (conifer-make-view (car parse-result)))])
	(cons
	  (car expand-result)
	  (append
	    (cdr expand-result)
	    (cdr parse-result)))))))
