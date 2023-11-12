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
	  ast-identifier?
	  ast-symbol?
	  ast-null?
	  make-ast-identifier
	  make-ast-list
	  ast-list?
	  ast-list-span
	  ast-list-fn
	  ast-list-args)
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

  ;; <expr> → <literal> | <list> | <vector> | <lambda> | <if> | <quote>
  ;;	    | <set!> | <pair>
  ;; <literal> → <number> | <boolean> | <character> | <string> | <bytevector>
  ;;	       | <variable> | <symbol>
  (define-record-type
    ast-expr
    (fields
      ; Pair of [start, end) into the parsed text.
      span
      ; The value of the expresson, literals are stored as primitives
      ; (i.e. char, boolean, number, identifier (symbol), string).
      value)
    (protocol
      (lambda (new)
	(lambda (span value)
	  (cond
	    [(char? value) (new span (cons 'char value))]
	    [(string? value) (new span (cons 'string value))]
	    [(boolean? value) (new span (cons 'boolean value))]
	    [(symbol? value) (new span (cons 'symbol value))]
	    [(null? value) (new span (cons 'null '()))]
	    [else (new span value)])))))

  ;; Returns `#t` if `e` is a boolean expr.
  (define ast-boolean?
    (lambda (e)
      (and (ast-expr? e)
	   (eq? 'boolean (car (ast-expr-value e))))))

  ;; Returns `#t` if `e` is a char expr.
  (define ast-char?
    (lambda (e)
      (and (ast-expr? e)
	   (eq? 'char (car (ast-expr-value e))))))

  ;; Returns `#t` if `e` is a string expr.
  (define ast-string?
    (lambda (e)
      (and (ast-expr? e)
	   (eq? 'string (car (ast-expr-value e))))))

  ;; Returns `#t` if `e` is an identifier expr.
  (define ast-identifier?
    (lambda (e)
      (and (ast-expr? e)
	   (eq? 'identifier (car (ast-expr-value e))))))

  ;; Returns `#t` if `e` is a symbol expr.
  (define ast-symbol?
    (lambda (e)
      (and (ast-expr? e)
	   (eq? 'symbol (car (ast-expr-value e))))))

  ;; Returns `#t` if `e` is a null expr.
  (define ast-null?
    (lambda (e)
      (and (ast-expr? e)
	   (eq? 'null (car (ast-expr-value e))))))

  ;; Returns a new identifier expression 
  (define make-ast-identifier
    (lambda (span ident)
      (make-ast-expr
	span
	(cons 'identifier
	       ident))))

  ;; Returns a new list 
  (define make-ast-list
    (lambda (span elems)
      (make-ast-expr
	span
	(cons* 'list
	       (if (list? elems)
		 (list->vector elems)
		 elems)))))

  ;; Returns a new list 
  (define make-ast-list
    (lambda (span elems)
      (make-ast-expr
	span
	(cons* 'list
	       (if (list? elems)
		 (list->vector elems)
		 elems)))))

  ;; Returns `#t` if `e` is a list expression.
  (define ast-list?
    (lambda (e)
      (and (ast-expr? e)
	   (eq? 'list (car (ast-expr-value e))))))

  (define ast-list-fn
    (lambda (e)
      (assert (ast-list? e))
      (cadr (ast-expr-value e))))

  (define ast-list-args
    (lambda (e)
      (assert (ast-list? e))
      (cddr (ast-expr-value e)))))
