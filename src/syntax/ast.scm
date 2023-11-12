(library
  (syntax ast)
  (export make-ast-root
	  ast-root?
	  ast-root-items
	  make-ast-expr
	  ast-expr?
	  ast-expr-span
	  ast-expr-kind
	  ast-expr-value
	  make-ast-boolean
	  ast-boolean?
	  make-ast-char
	  ast-char?
	  make-ast-string
	  ast-string?
	  make-ast-identifier
	  ast-identifier?
	  make-ast-symbol
	  ast-symbol?
	  make-ast-null
	  ast-null?
	  make-ast-unspecified
	  ast-unspecified?
	  make-ast-list
	  ast-list?
	  make-ast-if
	  ast-if?)
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
      ; A symbol representing the kind of the expression.
      kind
      ; The value of the expresson, literals are stored as primitives
      ; (i.e. char, boolean, number, identifier (symbol), string).
      value))

  ;; Returns a new boolean node.
  (define make-ast-boolean
    (lambda (span b)
      (make-ast-expr span 'boolean b)))

  ;; Returns `#t` if `e` is a boolean expr.
  (define ast-boolean?
    (lambda (e)
      (and (ast-expr? e)
	   (eq? 'boolean (ast-expr-kind e)))))

  ;; Returns a new char node.
  (define make-ast-char
    (lambda (span c)
      (make-ast-expr span 'char c)))

  ;; Returns `#t` if `e` is a char expr.
  (define ast-char?
    (lambda (e)
      (and (ast-expr? e)
	   (eq? 'char (ast-expr-kind e)))))

  ;; Returns a new string node.
  (define make-ast-string
    (lambda (span s)
      (make-ast-expr span 'string s)))

  ;; Returns `#t` if `e` is a string expr.
  (define ast-string?
    (lambda (e)
      (and (ast-expr? e)
	   (eq? 'string (ast-expr-kind e)))))

  ;; Returns a new identifier node.
  (define make-ast-identifier
    (lambda (span id)
      (make-ast-expr span 'identifier id)))

  ;; Returns `#t` if `e` is an identifier expr.
  (define ast-identifier?
    (lambda (e)
      (and (ast-expr? e)
	   (eq? 'identifier (ast-expr-kind e)))))

  ;; Returns a new symbol node.
  (define make-ast-symbol
    (lambda (span sy)
      (make-ast-expr span 'symbol sy)))

  ;; Returns `#t` if `e` is a symbol expr.
  (define ast-symbol?
    (lambda (e)
      (and (ast-expr? e)
	   (eq? 'symbol (ast-expr-kind e)))))

  ;; Returns a new unspecified node.
  (define make-ast-unspecified
    (lambda (span)
      (make-ast-expr span 'unspecified #f)))

  ;; Returns `#t` if `e` is a unspecified expr.
  (define ast-unspecified?
    (lambda (e)
      (and (ast-expr? e)
	   (eq? 'unspecified (ast-expr-kind e)))))

  ;; Returns a new null node.
  (define make-ast-null
    (lambda (span)
      (make-ast-expr span 'null #f)))

  ;; Returns `#t` if `e` is a null expr.
  (define ast-null?
    (lambda (e)
      (and (ast-expr? e)
	   (eq? 'null (car (ast-expr-value e))))))

  ;; Returns a new list 
  (define make-ast-list
    (lambda (span elems)
      (make-ast-expr
	span
	'list
	(if (list? elems)
	  (list->vector elems)
	  elems))))

  ;; Returns `#t` if `e` is a list expression.
  (define ast-list?
    (lambda (e)
      (and (ast-expr? e)
	   (eq? 'list (ast-expr-kind e)))))

  ;; Returns a new if node 
  (define make-ast-if
    (lambda (span cond true . false)
      (make-ast-expr
	span
	'if
	(cons* cond
	       true
	       (if (null? false)
		 #f
		 (car false))))))

  ;; Returns `#t` if `e` is an if expression.
  (define ast-if?
    (lambda (e)
      (and (ast-expr? e)
	   (eq? 'if (ast-expr-kind e))))))