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
	  (let* ([filename (fetch qctx 'module->filename module)]
		 [id-str (fmap (lambda (f)
				 (fetch qctx 'filename->string f))
			       filename)]
		 [tokens (fmap (lambda (id-str) (scan-string (cdr id-str))) id-str)]
		 [p-result (fmap parse-tokens tokens)])
	    (fmap
	      (lambda (result)
		(make-parse-result
		  (car id-str)
		  (conifer-make-view (car result))
		  (cdr result)))
	      p-result)))))))
