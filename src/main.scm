#!/usr/bin/env guile \
-e main --r6rs -s
!#

(add-to-load-path (dirname (current-filename)))

(use-modules (rnrs base)
	     (rnrs exceptions)
	     (rnrs io simple)
	     (ice-9 format)
	     (ice-9 exceptions)
	     (conifer)
	     (compiler config)
	     (syntax)
	     (query)
	     (common)
	     (atty))

(define HELP-MESSAGE
"The Kabla compiler

Usage: kabla [OPTIONS] FILE

Arguments:
  FILE  The file to compile

Options:
  -h, --help
	Prints this message and exits.
  --emit parse-tree
	Parses the file (without expanding macros) and prints the resulting tree.")

(define SEARCH-PATH '())
(define DEFAULT-COMPILER-CONFIG
  (list
    (cons 'search-path SEARCH-PATH)))

(define main
  (lambda (args)
    (let* ([parsed-args (parse-args args)]
	   [input-file (cdr (assq 'input-file parsed-args))]
	   [absolute-input-filename (guard
				      (x [(eq? 'system-error (exception-kind x))
					  (bail (format #f
							"~a ~? - ~a"
							(red-bold "error:")
							(exception-message x)
							(exception-irritants x)
							input-file))])
				      (canonicalize-path input-file))]
	   [compiler-config (assoc-list->compiler-config (append parsed-args
								 (cons
								   (cons 'base-dir
									 (dirname absolute-input-filename))
								   DEFAULT-COMPILER-CONFIG)))]
	   [qctx (make-query-context compiler-config)]
	   [root-module (input-file->module input-file)]
	   [result (fetch qctx 'module->parse-tree root-module)])

      (cond
	[(is-error? result) (bail (apply format #f "~a ~@?" (red "error:") (cdr result)))]
	[result (for-each (lambda (n)
			    (display (conifer-tree->debug-string n))
			    (newline))
			  (pt-root-sexps (parse-result-parse-tree result)))]
	[else (bail
		"query returned #f")]))))

;; Parses the arguments given to the program into an association list.
(define parse-args
  (lambda (args)
    (let loop ([args (cdr args)]
	       [assl '()])
      (if (null? args)
	(validate-parsed-args assl)
	(let ([arg (car args)]
	      [rest (cdr args)])
	  (cond
	    [(string-prefix? "--" arg)
	     (cond
	       [(string-ci=? "--emit" arg)
		(loop (if (null? rest)
			rest
			(cdr rest))
		      (cons (cons 'emit (if (null? rest)
					  #f
					  (car rest)))
			    assl))]
	       [(string-ci=? "--help") (bail HELP-MESSAGE 0)]
	       [else
		 (bail
		   (string-append
		     (red "error: ")
		     (format #f "unknown flag ~a" arg)))])]
	    [(string-prefix? "-" arg)
	     (cond
	       [(string-ci=? "-h" arg) (bail HELP-MESSAGE 0)]
	       [else
		 (bail
		   (string-append
		     (red "error: ")
		     (format #f "unknown flag ~a" arg)))])]
	    ; input file
	    [else
	      (if (assq 'input-file assl)
		(assertion-violation 'parse-args
				     "only one INPUT_FILE accepted")
		(loop rest
		      (cons (cons 'input-file
				  arg)
			    assl)))]))))))

;; Validates the parsed arguments.
(define validate-parsed-args
  (lambda (args)
    (let check-duplicates ([args args]
			   [seen-config '()])
      (when (not (null? args))
	(when (memq (caar args) seen-config)
	  (bail
	    (string-append
	      (red "error: ")
	      (format #f "duplicate flag ~a" (caar args)))))
	(check-duplicates (cdr args)
			  (cons (caar args)
				seen-config))))

    (when (not (assq 'input-file args))
      (bail HELP-MESSAGE))
    (cond
      [(assq 'emit args)
       => (lambda (el)
	    (case (string->symbol (cdr el))
	      [(parse-tree) #t]
	      [else (bail
		      (string-append
			(red "error: ")
			(format
			  #f
			  "invalid argument ~a to flag --~a"
			  (if (cdr el)
			    (cdr el)
			    "<none>") (car el))))]))])
    args))

;; Prints `msg` and exits the program.
(define bail
  (lambda (msg . exit-code)
    (let* ([exit-code (if (null? exit-code)
			1
			(car exit-code))]
	   [out-port (if (= exit-code 0)
		       (current-output-port)
		       (current-error-port))])
      (display msg out-port)
      (newline out-port)
      (exit exit-code))))

;; Turns the input file to a module name.
;;
;; # Examples
;;
;; "src/main.scm" -> '(main)
;; "kabla.scm" -> '(kabla)
(define input-file->module
  (lambda (absolute-path)
    (let* ([fn (basename absolute-path)]
	   [last-period (or (string-rindex fn #\.) (string-length fn))]
	   [without-extension (substring fn 0 last-period)])
      (list (string->symbol
	      without-extension)))))
