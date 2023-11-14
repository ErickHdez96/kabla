(library
  (syntax ast)
  (export make-ast-root
	  ast-root?
	  ast-root-items
	  make-ast-define
	  ast-define?
	  ast-define-span
	  ast-define-variable
	  ast-define-expr
	  ast-span
	  ast-variable
	  ast-expr
	  make-ast-expr
	  ast-expr?
	  ast-expr-span
	  ast-expr-kind
	  ast-expr-value
	  make-ast-error
	  ast-error?
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
	  ast-if?
	  make-ast-lambda
	  ast-lambda?
	  make-ast-let
	  ast-let?)
  (import (rnrs base)
	  (only (conifer)
		conifer-red-tree?
		conifer-green-tree?
		conifer-red-tree-green)
	  (only (rnrs records syntactic)
		define-record-type)
	  (only (rnrs lists)
		cons*))

  (define-record-type
    ast-root
    (fields
      ; vector of items parsed from the file.
      items
      green)
    (protocol
      (lambda (new)
	(lambda (items tree)
	  (new
	    items
	    (cond
	      [(conifer-red-tree? tree) (conifer-red-tree-green tree)]
	      [(conifer-green-tree? tree) tree]
	      [else (assertion-violation 'ast-root
					 "expected a red/green node, found ~a"
					 tree)]))))))

  ;; Import directive `(import <import spec> ... )`
  (define-record-type
    ast-import
    (fields
      ;; Vector of libraries being imported
      library-names))

  ;; <define> → ( define <var> <expr>? )
  ;;	      | ( define ( <var> <formals> ) <expr> )
  ;;	      | ( define ( <var> . <formal> ) <expr> )
  (define-record-type
    ast-define
    (fields
      ; Pair of [start, end) into the parsed text.
      span
      ; The name of the variable being defined (as an ast-expr.)
      variable
      ; ast-expr used to initialize `variable`, `#f` if none.
      expr
      green)
    (protocol
      (lambda (new)
	(lambda (span tree variable expr)
	  (new
	    span
	    variable
	    expr
	    (cond
	      [(conifer-red-tree? tree) (conifer-red-tree-green tree)]
	      [(conifer-green-tree? tree) tree]
	      [else (assertion-violation 'ast-root
					 "expected a red/green node, found ~a"
					 tree)]))))))

  ;; <expr> → <literal> | <list> | <vector> | <lambda> | <if> | <quote>
  ;;	    | <set!> | <pair>
  ;; <literal> → <number> | <boolean> | <character> | <string> | <bytevector>
  ;;	       | <variable> | <symbol>
  (define-record-type
    ast-expr
    (fields
      ; Pair of [start, length] into the parsed text.
      span
      ; A symbol representing the kind of the expression.
      kind
      ; The value of the expresson, literals are stored as primitives
      ; (i.e. char, boolean, number, identifier (symbol), string).
      value
      green)
    (protocol
      (lambda (new)
	(lambda (span kind value tree)
	  (new
	    span
	    kind
	    value
	    (cond
	      [(conifer-red-tree? tree) (conifer-red-tree-green tree)]
	      [(conifer-green-tree? tree) tree]
	      [else (assertion-violation 'ast-root
					 "expected a red/green node, found ~a"
					 tree)]))))))

  ;; Creates a new error expression. When an expression is expected, but the
  ;; datum couldn't be correctly expanded, an error is generated to preserve
  ;; the original tree.
  (define make-ast-error
    (lambda (span green)
      (make-ast-expr span 'error #f green)))

  ;; Returns `#t` if `e` is an error expr.
  (define ast-error?
    (lambda (e)
      (and (ast-expr? e)
	   (eq? 'error (ast-expr-kind e)))))

  ;; Returns a new boolean node.
  (define make-ast-boolean
    (lambda (span green b)
      (make-ast-expr span 'boolean b green)))

  ;; Returns `#t` if `e` is a boolean expr.
  (define ast-boolean?
    (lambda (e)
      (and (ast-expr? e)
	   (eq? 'boolean (ast-expr-kind e)))))

  ;; Returns a new char node.
  (define make-ast-char
    (lambda (span green c)
      (make-ast-expr span 'char c green)))

  ;; Returns `#t` if `e` is a char expr.
  (define ast-char?
    (lambda (e)
      (and (ast-expr? e)
	   (eq? 'char (ast-expr-kind e)))))

  ;; Returns a new string node.
  (define make-ast-string
    (lambda (span green s)
      (make-ast-expr span 'string s green)))

  ;; Returns `#t` if `e` is a string expr.
  (define ast-string?
    (lambda (e)
      (and (ast-expr? e)
	   (eq? 'string (ast-expr-kind e)))))

  ;; Returns a new identifier node.
  (define make-ast-identifier
    (lambda (span green id)
      (make-ast-expr span 'identifier id green)))

  ;; Returns `#t` if `e` is an identifier expr.
  (define ast-identifier?
    (lambda (e)
      (and (ast-expr? e)
	   (eq? 'identifier (ast-expr-kind e)))))

  ;; Returns a new symbol node.
  (define make-ast-symbol
    (lambda (span green sy)
      (make-ast-expr span 'symbol sy green)))

  ;; Returns `#t` if `e` is a symbol expr.
  (define ast-symbol?
    (lambda (e)
      (and (ast-expr? e)
	   (eq? 'symbol (ast-expr-kind e)))))

  ;; Returns a new unspecified node.
  (define make-ast-unspecified
    (lambda (span green)
      (make-ast-expr span 'unspecified #f green)))

  ;; Returns `#t` if `e` is a unspecified expr.
  (define ast-unspecified?
    (lambda (e)
      (and (ast-expr? e)
	   (eq? 'unspecified (ast-expr-kind e)))))

  ;; Returns a new null node.
  (define make-ast-null
    (lambda (span green)
      (make-ast-expr span 'null #f green)))

  ;; Returns `#t` if `e` is a null expr.
  (define ast-null?
    (lambda (e)
      (and (ast-expr? e)
	   (eq? 'null (car (ast-expr-value e))))))

  ;; Returns a new list 
  (define make-ast-list
    (lambda (span green elems)
      (make-ast-expr
	span
	'list
	(if (list? elems)
	  (list->vector elems)
	  elems)
	green)))

  ;; Returns `#t` if `e` is a list expression.
  (define ast-list?
    (lambda (e)
      (and (ast-expr? e)
	   (eq? 'list (ast-expr-kind e)))))

  ;; Returns a new if node 
  (define make-ast-if
    (lambda (span green cond true . false)
      (make-ast-expr
	span
	'if
	(cons* cond
	       true
	       (if (null? false)
		 #f
		 (car false)))
	green)))

  ;; Returns `#t` if `e` is an if expression.
  (define ast-if?
    (lambda (e)
      (and (ast-expr? e)
	   (eq? 'if (ast-expr-kind e)))))

  ;; Returns a new `lambda` node.
  (define make-ast-lambda
    (lambda (span green vars rest body)
      (assert (list? vars))
      (for-each
	(lambda (v)
	  (assert (ast-identifier? v)))
	vars)
      (assert (or (ast-identifier? rest)
		  (not rest)))
      (assert (and (ast-let? body)
		   (eq? 'letrec* (ast-expr-kind body))))

      (make-ast-expr
	span
	'lambda
	(cons* vars
	       rest
	       body)
	green)))

  ;; Returns `#t` if `e` is a lambda expression.
  (define ast-lambda?
    (lambda (e)
      (and (ast-expr? e)
	   (eq? 'lambda (ast-expr-kind e)))))

  ;; Returns a new `lambda` node.
  (define make-ast-let
    (lambda (span green kind vars exprs)
      (assert (or (eq? 'let kind)
		  (eq? 'let* kind)
		  (eq? 'letrec kind)
		  (eq? 'letrec* kind)))
      (assert (list? vars))
      (for-each
	(lambda (var)
	  (assert (and (pair? var)
		       (ast-identifier? (car var))
		       (ast-expr? (cdr var)))))
	vars)
      (assert (list? exprs))
      (for-each
	(lambda (e)
	  (assert (ast-expr? e)))
	exprs)

      (make-ast-expr
	span
	kind
	(cons* vars
	       exprs)
	green)))

  ;; Returns `#t` if `e` is a let/let*/letrec/letrec* expression.
  (define ast-let?
    (lambda (e)
      (and (ast-expr? e)
	   (case (ast-expr-kind e)
	     [(let let* letrec letrec*) #t]
	     [else #f])))))
