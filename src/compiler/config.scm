(library
  (compiler config)
  (export make-compiler-config
	  compiler-config-search-path
	  compiler-config?
	  assoc-list->compiler-config)
  (import (rnrs base)
	  (only (rnrs control)
		when)
	  (rnrs records syntactic)
	  (rnrs lists)
	  (common)
	  (only (system)
		canonicalize-path
		dirname))

  (define-record-type
    compiler-config
    (fields
      ; string - Absolute path of the base directory where the compiler
      ; should run.
      search-path))

  ;; Transforms an association list into a compiler-config record.
  ;;
  ;; Must-be present keys:
  ;; * search-path: Base directory where local dependencies will be sought.
  ;;
  ;; Optional keys:
  ;; * base-dir: Directory where the root file to compile is located. This
  ;;   directory will be added at the head of `search-path`.
  ;;
  ;; (: assoc-list->compiler-config (-> AssocList[Symbol, Any] CompilerConfig))
  (define assoc-list->compiler-config
    (lambda assl
      (cond
	; transform `assl` from List[AssocList; 1] -> AssocList
	[(and (not (null? assl))
	      (list? assl)
	      (list? (car assl)))
	 (apply assoc-list->compiler-config
		(car assl))]
	[else
	  (assert (or (null? assl)
		      (pair? (car assl))))

	  (let ([search-path (or (fmap (lambda (sp)
					 (when (not (list? (cdr sp)))
					   (assertion-violation
					     'assoc-list->compiler-config
					     "search-path: must be a list of strings"))
					 (when (and (not (null? (cdr sp)))
						    (not (for-all string? (cdr sp))))
					   (assertion-violation
					     'assoc-list->compiler-config
					     "search-path: must consist exclusively of strings ~A"
					     sp))
					 (cdr sp))
				       (assq 'search-path assl))
				 (assertion-violation
				   'assoc-list->compiler-config
				   "search-path: Missing required field"))]
		[base-dir (fmap (lambda (bd)
				  (when (not (string? (cdr bd)))
				    (assertion-violation
				      'assoc-list->compiler-config
				      "base-dir: must be a string"))
				  (cdr bd))
				(assq 'base-dir assl))])
	    (make-compiler-config
	      ; search-path
	      (if base-dir
		(cons base-dir search-path)
		search-path)))]))))
