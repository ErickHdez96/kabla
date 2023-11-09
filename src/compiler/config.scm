(define-module (compiler config)
	       #:export (make-compiler-config
			  compiler-config-base-dir
			  compiler-config-root-file
			  compiler-config?
			  assoc-list->compiler-config))

(use-modules (rnrs records syntactic)
	     (rnrs base)
	     (common))

(define-record-type
  compiler-config
  (fields
    ; string - Absolute path of the base directory where the compiler
    ; should run.
    base-dir
    ; string - Absolute path to the input file to be compiled.
    root-file))

;; Transforms an association list into a compiler-config record.
;;
;; Must-be present keys:
;; * base-dir: Base directory where local dependencies will be sought.
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

	(let ([root-file (fmap
			   (lambda (res) (canonicalize-path (cdr res)))
			   (assq 'input-file assl))])
	  (make-compiler-config
	    (or (fmap cdr (assq 'base-dir
				assl))
		(fmap dirname root-file)
		(getcwd))
	    root-file))])))
