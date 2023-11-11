(library
  (syntax ast)
  (export make-ast-root
	  ast-root?
	  ast-root-items
	  make-ast-expr
	  ast-expr?
	  ast-expr-span
	  ast-expr-value
	  ast-boolean?
	  ast-char?
	  ast-string?
	  ast-variable?
	  make-ast-proc-call
	  ast-proc-call?
	  ast-proc-call-span
	  ast-proc-call-fn
	  ast-proc-call-args)
  (import (rnrs base)
	  (only (rnrs records syntactic)
		define-record-type)
	  (only (rnrs lists)
		cons*))

  (define-record-type
    ast-root
    (fields
      ; vector of items parsed from the file.
      items))

  ;; Import directive `(import <import spec> ... )`
  (define-record-type
    ast-import
    (fields
      ;; Vector of libraries being imported
      library-names))

  ;; <expr> → <literal> | <procedure call> | <vector> | <lambda>
  ;;	    | <if> | <quote> | <set!>
  ;; <literal> → <number> | <boolean> | <character> | <string> | <bytevector>
  ;;	       | <variable>
  (define-record-type
    ast-expr
    (fields
      ; Pair of [start, end) into the parsed text.
      span
      ; The value of the expresson, literals are stored as primitives
      ; (i.e. char, boolean, number, identifier (symbol), string).
      value))

  ;; Returns `#t` if `e` is a boolean expr.
  (define ast-boolean?
    (lambda (e)
      (and (ast-expr? e)
	   (boolean? (ast-expr-value e)))))

  ;; Returns `#t` if `e` is a char expr.
  (define ast-char?
    (lambda (e)
      (and (ast-expr? e)
	   (char? (ast-expr-value e)))))

  ;; Returns `#t` if `e` is a string expr.
  (define ast-string?
    (lambda (e)
      (and (ast-expr? e)
	   (string? (ast-expr-value e)))))

  ;; Returns `#t` if `e` is a variable expr.
  (define ast-variable?
    (lambda (e)
      (and (ast-expr? e)
	   (symbol? (ast-expr-value e)))))

  (define make-ast-proc-call
    (lambda (span fn args)
      (make-ast-expr
	span
	(cons* 'proc-call
	       fn
	       (if (list? args)
		 (list->vector args)
		 args)))))

  (define ast-proc-call?
    (lambda (e)
      (and (ast-expr? e)
	   (eq? 'proc-call (car (ast-expr-value e))))))

  (define ast-proc-call-fn
    (lambda (e)
      (assert (ast-proc-call? e))
      (cadr (ast-expr-value e))))

  (define ast-proc-call-args
    (lambda (e)
      (assert (ast-proc-call? e))
      (cddr (ast-expr-value e)))))
