(library
  (syntax expander)
  (export expand-parse-tree)
  (import (rnrs base)
	  (only (rnrs records syntactic)
		define-record-type)
	  (only (conifer)
		conifer-syntax-kind
		conifer-red-offset
		conifer-syntax-kind
		conifer-text-length)
	  (only (syntax parse-tree)
		pt-root-sexps
		pt-sexp?
		pt-atom?
		pt-boolean?
		pt-boolean-value
		pt-char?
		pt-list?
		pt-vector?
		pt-bytevector?
		pt-abbreviation?)
	  (env)
	  (common))

  (define-record-type
    expander
    (fields))

  ;; Parses a top-level program from a red-tree.
  (define expand-parse-tree
    (lambda (pt)
      (define expander (make-expander))
      (define val-env (make-root-env))

      (let loop ([pt-children (pt-root-sexps pt)]
		 [items '()])
	(cond
	  [(null? pt-children)
	   (reverse items)]
	  [(pt-sexp? (car pt-children))
	   (cond
	     [(expand-sexp expander
			   (car pt-children)
			   val-env)
	      => (lambda (item) (loop (cdr pt-children)
				      (cons item items)))]
	     [else (loop (cdr pt-children)
			 (cons item items))])]
	  [else (error 'expand-parse-tree
		       "shouldn't have arrived here")]))))

  (define expand-sexp
    (lambda (e sexp val-env)
      (cond
	[(pt-atom? sexp) => (lambda (atom) (expand-atom e atom val-env))]
	[else (error 'expand-sexp
		     "unknown sexp kind: ~a"
		     (conifer-syntax-kind sexp))])))

  (define expand-atom
    (lambda (e atom val-env)
      (cond
	[(pt-boolean? atom)
	 (cons (cons (conifer-red-offset atom)
		     (conifer-text-length atom))
	       (pt-boolean-value atom))]
	[(pt-char? atom)
	 => (lambda (c) (cons (cons (conifer-red-offset atom)
				    (conifer-text-length atom))
			      c))]
	[else (error 'expand-atom
		     "unknown atom kind: ~a"
		     (conifer-syntax-kind atom))]))))
