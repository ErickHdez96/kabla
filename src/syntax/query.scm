(define-module (syntax query)
	       #:export (register-queries))

(use-modules (quest)
	     (conifer)
	     (syntax scanner)
	     (syntax parser)
	     (common))

(define register-queries
  (lambda (bctx)
    (register-task
      bctx
      'parse-module
      (lambda (qctx module)
	(let* ([filename (fetch qctx 'module->filename module)]
	       [str (fmap (lambda (f)
			    (fetch qctx 'filename->string f))
			  filename)]
	       [tokens (fmap scan-string str)]
	       [p-result (fmap parse-tokens tokens)])
	  (fmap
	    (lambda (result)
	      (cons
		(conifer-make-view (car result))
		(cdr result)))
	    p-result))))))
