(library
  (syntax ast)
  (export make-ast-root
	  ast-span
	  ast-root?
	  ast-root-items
	  make-ast-define
	  ast-define?
	  ast-define-offset
	  ast-define-variable
	  ast-define-expr
	  ast-offset
	  ast-variable
	  ast-expr
	  make-ast-expr
	  ast-expr?
	  ast-expr-offset
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
		conifer-red-tree-green
		conifer-text-length)
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
      offset
      ; The name of the variable being defined (as an ast-expr.)
      variable
      ; ast-expr used to initialize `variable`, `#f` if none.
      expr
      green)
    (protocol
      (lambda (new)
	(lambda (offset tree variable expr)
	  (assert (integer? offset))
	  (new
	    offset
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
      offset
      ; A symbol representing the kind of the expression.
      kind
      ; The value of the expresson, literals are stored as primitives
      ; (i.e. char, boolean, number, identifier (symbol), string).
      value
      green)
    (protocol
      (lambda (new)
	(lambda (offset kind value tree)
	  (assert (integer? offset))
	  (new
	    offset
	    kind
	    value
	    (cond
	      [(conifer-red-tree? tree) (conifer-red-tree-green tree)]
	      [(conifer-green-tree? tree) tree]
	      [else (assertion-violation 'ast-root
					 "expected a red/green node, found ~a"
					 tree)]))))))

  ;; Returns the `(offset . length)` span of `node.`
  (define ast-span
    (lambda (node)
      (cond
	[(ast-expr? node) (cons (ast-expr-offset node)
				(conifer-text-length (ast-expr-green node)))]
	[(ast-define? node) (cons (ast-define-offset node)
				  (conifer-text-length (ast-define-green node)))]
	[else (error 'ast-span
		     "invalid node: ~a"
		     node)])))

  ;; Creates a new error expression. When an expression is expected, but the
  ;; datum couldn't be correctly expanded, an error is generated to preserve
  ;; the original tree.
  (define make-ast-error
    (lambda (offset green)
      (make-ast-expr offset 'error #f green)))

  ;; Returns `#t` if `e` is an error expr.
  (define ast-error?
    (lambda (e)
      (and (ast-expr? e)
	   (eq? 'error (ast-expr-kind e)))))

  ;; Returns a new boolean node.
  (define make-ast-boolean
    (lambda (offset green b)
      (make-ast-expr offset 'boolean b green)))

  ;; Returns `#t` if `e` is a boolean expr.
  (define ast-boolean?
    (lambda (e)
      (and (ast-expr? e)
	   (eq? 'boolean (ast-expr-kind e)))))

  ;; Returns a new char node.
  (define make-ast-char
    (lambda (offset green c)
      (make-ast-expr offset 'char c green)))

  ;; Returns `#t` if `e` is a char expr.
  (define ast-char?
    (lambda (e)
      (and (ast-expr? e)
	   (eq? 'char (ast-expr-kind e)))))

  ;; Returns a new string node.
  (define make-ast-string
    (lambda (offset green s)
      (make-ast-expr offset 'string s green)))

  ;; Returns `#t` if `e` is a string expr.
  (define ast-string?
    (lambda (e)
      (and (ast-expr? e)
	   (eq? 'string (ast-expr-kind e)))))

  ;; Returns a new identifier node.
  (define make-ast-identifier
    (lambda (offset green id)
      (make-ast-expr offset 'identifier id green)))

  ;; Returns `#t` if `e` is an identifier expr.
  (define ast-identifier?
    (lambda (e)
      (and (ast-expr? e)
	   (eq? 'identifier (ast-expr-kind e)))))

  ;; Returns a new symbol node.
  (define make-ast-symbol
    (lambda (offset green sy)
      (make-ast-expr offset 'symbol sy green)))

  ;; Returns `#t` if `e` is a symbol expr.
  (define ast-symbol?
    (lambda (e)
      (and (ast-expr? e)
	   (eq? 'symbol (ast-expr-kind e)))))

  ;; Returns a new unspecified node.
  (define make-ast-unspecified
    (lambda (offset green)
      (make-ast-expr offset 'unspecified #f green)))

  ;; Returns `#t` if `e` is a unspecified expr.
  (define ast-unspecified?
    (lambda (e)
      (and (ast-expr? e)
	   (eq? 'unspecified (ast-expr-kind e)))))

  ;; Returns a new null node.
  (define make-ast-null
    (lambda (offset green)
      (make-ast-expr offset 'null #f green)))

  ;; Returns `#t` if `e` is a null expr.
  (define ast-null?
    (lambda (e)
      (and (ast-expr? e)
	   (eq? 'null (car (ast-expr-value e))))))

  ;; Returns a new list 
  (define make-ast-list
    (lambda (offset green elems)
      (make-ast-expr
	offset
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
    (lambda (offset green cond true . false)
      (make-ast-expr
	offset
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
    (lambda (offset green vars rest body)
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
	offset
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
    (lambda (offset green kind vars exprs)
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
	offset
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
