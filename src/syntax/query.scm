(library
  (syntax query)
  (export register-queries)
  (import (rnrs base)
	  (only (rnrs records syntactic)
		define-record-type)
	  (only (quest)
		register-task
		fetch)
	  (only (conifer)
		conifer-make-view)
	  (only (syntax scanner)
		scan-string)
	  (only (syntax parser)
		parse-tokens)
	  (only (syntax records)
		make-parse-result)
	  (common))

  (define register-queries
    (lambda (bctx)
      (register-task
	bctx
	'module->parse-tree
	(lambda (qctx module)
	  (and-then
	    (fetch qctx 'module->filename module)
	    [-> fname (fetch qctx 'filename->string fname)]
	    [-> id-str (let ([p-result (parse-tokens (scan-string (cdr id-str)))])
			 (make-parse-result
			   (car id-str)
			   (conifer-make-view (car p-result))
			   (cdr p-result)))]))))))
