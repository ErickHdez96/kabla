(library
  (query)
  (export make-query-context
	  fetch)
  (import (rnrs base)
	  (only (rnrs control)
		when)
	  (only (rnrs lists)
		cons*
		assoc)
	  (only (srfi srfi-13)
		string-prefix?)
	  (only (rnrs io simple)
		call-with-input-file)
	  (only (rnrs exceptions)
		guard)
	  (only (ice-9 exceptions)
		exception-kind)
	  (only (ice-9 textual-ports)
		get-string-all)
	  (only (quest)
		make-builder
		register-task
		fetch)
	  (only (compiler config)
		compiler-config?
		compiler-config-search-path)
	  (only (syntax)
		syntax-register-queries)
	  (common)
	  (system))

  ; Global state

  ;; Association list from file names to file-id.
  (define FILE-IDS '())

  ;; Creates a new query context with the given compiler config
  (define make-query-context
    (lambda (compiler-config)
      (assert (compiler-config? compiler-config))
      (let ([bctx (make-builder)])
	(register-task
	  bctx
	  'compiler-config
	  (lambda _ compiler-config))

	(register-queries bctx)
	(syntax-register-queries bctx)

	bctx)))

  (define register-queries
    (lambda (bctx)
      (register-task bctx 'module->filename q-module->filename)
      (register-task bctx 'filename->string q-filename->string)
      ))

  ;; Transforms a module into an absolute filename.
  ;; (: q-module->filename (-> QCtx ModuleName String))
  ;; 	where:
  ;;		ModuleName = List[Symbol..., Version?]
  ;;		Version = List[Integer]
  (define q-module->filename
    (lambda (qctx module-name)

      ; Turn a module name into the respective filename.
      ; For example:
      ; (rnrs) -> rnrs
      ; (rnrs base) -> rnrs/base
      ; (rnrs io simple) -> rnrs/io/simple
      (define module-name->filename
	(lambda (module)
	  (assert (and (list? module)
		       (not (null? module))))
	  (let loop ([module module]
		     [path '()])
	    (cond
	      [(null? module)
	       (apply string-append
		      (reverse path))]
	      [(symbol? (car module))
	       (loop (cdr module)
		     (cons* (symbol->string (car module))
			    file-name-separator-string
			    path))]
	      [(list? (car module))
	       (assertion-violation
		 'q-module->filename
		 "versions not supported yet")]
	      [else (assertion-violation
		      'q-module->filename
		      "module elements can only be symbols or a version list")]))))

      (let* ([compiler-config (fetch qctx 'compiler-config '())]
	     [search-path (compiler-config-search-path compiler-config)]
	     [module-filename-suffix (module-name->filename module-name)]
	     [file-extensions '("scm")])
	; Try to search the module in the search path, with different file
	; extensions.
	(let loop ([search-path search-path]
		   [fexts file-extensions])
	  (cond
	    [(null? search-path)
	     (err "could not resolve module ~A" module-name)]
	    [(null? fexts) (loop (cdr search-path)
				 file-extensions)]

	    [(guard
	       (x [(eq? (exception-kind x) 'system-error) #f])
	       (canonicalize-path
		 (string-append
		   (car search-path)
		   file-name-separator-string
		   module-filename-suffix
		   "."
		   (car fexts))))]
	    [else (loop search-path
			(cdr fexts))])))))

  ;; Returns the id and contents of the file with absolute path `filename`.
  (define q-filename->string
    (lambda (qctx filename)
      (when (not (string-prefix? "/" filename))
	(assertion-violation
	  'q-filename->content
	  "filename must be absolute"))

      (guard
	(x [(eq? (exception-kind x) 'system-error)
	    (err "could not read file: ~A" filename)])
	(let ([file-id (cond
			 [(assoc filename FILE-IDS) => (lambda (file-id) (cdr file-id))]
			 [else
			   (let ([id (length FILE-IDS)])
			     (set! FILE-IDS (cons (cons filename id)
						  FILE-IDS)))])])
	  (call-with-input-file
	    filename
	    (lambda (f)
	      (cons file-id
		    (get-string-all f)))))))))
