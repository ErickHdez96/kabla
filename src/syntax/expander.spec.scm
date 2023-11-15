(import (rnrs base)
	(srfi srfi-64)
	(conifer)
	(syntax shortcuts)
	(syntax ast))

(test-runner-current (test-runner-simple))

(define check-expression
  (lambda (expected actual)
    (cond
      [(or (boolean? expected)
	   (char? expected)
	   (string? expected)
	   (symbol? expected))
       (test-equal expected actual)]
      [(vector? expected)
       (vector-for-each
	 check-node
	 expected
	 actual)]
      [(pair? expected)
       (assert (pair? actual))
       (let loop ([e expected]
		  [a actual])
	 (check-node (car e)
		     (car a))
	 (cond
	   [(pair? (cdr e))
	    (assert (pair? (cdr a)))
	    (loop (cdr e)
		  (cdr a))]
	   [else
	     (assert (not (pair? (cdr a))))
	     (check-node (cdr e)
			 (cdr a))]))]
      [else
	(test-equal expected actual)])))

(define check-node
  (lambda (expected actual)
    (cond
      [(ast-expr? expected)
       (cond
	 [(or (not (ast-expr? actual)) 
	      (not (equal? (ast-expr-span expected) (ast-expr-span actual)))
	      (not (equal? (ast-expr-kind expected) (ast-expr-kind actual))))
	  (test-equal expected actual)]
	 [else
	   (check-expression (ast-expr-value expected)
			     (ast-expr-value actual))])]
      [(ast-define? expected)
       (cond
	 [(or (not (ast-define? actual)) 
	      (not (equal? (ast-define-span expected) (ast-define-span actual))))
	  (test-equal expected actual)]
	 [else
	   (check-node (ast-define-variable expected)
		       (ast-define-variable actual))
	   (check-node (ast-define-expr expected)
		       (ast-define-expr actual))])])))

(define check
  (lambda (str expected-ast . expected-errors)
    (let ([result (expand-str str)])
      (cond
	[(or (ast-expr? expected-ast)
	     (ast-define? expected-ast))
	 (test-eqv
	   1
	   (length (ast-root-items (car result))))
	 (check-node
	   expected-ast
	   (car (ast-root-items (car result))))]
	[else
	  (for-each
	    (lambda (e a)
	      (check-node e a))
	    expected-ast
	    (ast-root-items (car result)))])

      (test-equal
	(if (null? expected-errors)
	  '()
	  (car expected-errors))
	(cadr result)))))

(define gn
  (conifer-make-green-token
    'dummy
    ""))

(test-group
  "expander boolean"
  (check
    "#t"
    (make-ast-boolean '(0 . 2) gn #t))

  (check
    "#T"
    (make-ast-boolean '(0 . 2) gn #t))

  (check
    "#f"
    (make-ast-boolean '(0 . 2) gn #f))

  (check
    "#F"
    (make-ast-boolean '(0 . 2) gn #f)))

(test-group
  "expander char"
  (check
    "#\\a"
    (make-ast-char '(0 . 3) gn #\a))

  (check
    "#\\λ"
    (make-ast-char '(0 . 3) gn #\λ))

  (check
    "#\\nul"
    (make-ast-char '(0 . 5) gn #\nul))

  (check
    "#\\alarm"
    (make-ast-char '(0 . 7) gn #\alarm))

  (check
    "#\\backspace"
    (make-ast-char '(0 . 11) gn #\backspace))

  (check
    "#\\tab"
    (make-ast-char '(0 . 5) gn #\tab))

  (check
    "#\\linefeed"
    (make-ast-char '(0 . 10) gn #\linefeed))

  (check
    "#\\newline"
    (make-ast-char '(0 . 9) gn #\newline))

  (check
    "#\\vtab"
    (make-ast-char '(0 . 6) gn #\vtab))

  (check
    "#\\page"
    (make-ast-char '(0 . 6) gn #\page))

  (check
    "#\\return"
    (make-ast-char '(0 . 8) gn #\return))

  (check
    "#\\esc"
    (make-ast-char '(0 . 5) gn #\esc))

  (check
    "#\\space"
    (make-ast-char '(0 . 7) gn #\space))

  (check
    "#\\delete"
    (make-ast-char '(0 . 8) gn #\delete))

  (check
    "#\\x3bb"
    (make-ast-char '(0 . 6) gn #\λ))

  (check
    "#\\x3c0"
    (make-ast-char '(0 . 6) gn #\π))

  (check
    "#\\x2654"
    (make-ast-char '(0 . 7) gn #\♔)))

(test-group
  "expander char error"
  (check
    "#\\Space"
    (make-ast-char '(0 . 7) gn #\xFFFD)
    '(((0 . 7) "invalid character name: Space")))

  (check
    "#\\xDAAA"
    (make-ast-char '(0 . 7) gn #\xFFFD)
    '(((0 . 7)
       "hex scalar value must be in range [#x0, #xD7FF] ∪ [#xE000, #x10FFFF]"))))

(test-group
  "expander string"
  (check
    (list->string '(#\" #\a #\"))
    (make-ast-string '(0 . 3) gn "a"))

  (check
    (list->string '(#\" #\\ #\a #\"))
    (make-ast-string '(0 . 4) gn "\a"))

  (check
    (list->string '(#\" #\\ #\b #\"))
    (make-ast-string '(0 . 4) gn "\b"))

  (check
    (list->string '(#\" #\\ #\t #\"))
    (make-ast-string '(0 . 4) gn "\t"))

  (check
    (list->string '(#\" #\\ #\n #\"))
    (make-ast-string '(0 . 4) gn "\n"))

  (check
    (list->string '(#\" #\\ #\v #\"))
    (make-ast-string '(0 . 4) gn "\v"))

  (check
    (list->string '(#\" #\\ #\f #\"))
    (make-ast-string '(0 . 4) gn "\f"))

  (check
    (list->string '(#\" #\\ #\r #\"))
    (make-ast-string '(0 . 4) gn "\r"))

  (check
    (list->string '(#\" #\\ #\" #\"))
    (make-ast-string '(0 . 4) gn "\""))

  (check
    (list->string '(#\" #\\ #\\ #\"))
    (make-ast-string '(0 . 4) gn "\\")))

(test-group
  "expander string error"

  (check
    (list->string '(#\"))
    (make-ast-string '(0 . 1) gn "")
    '(((0 . 1) "unterminated string")))

  (check
    (list->string '(#\" #\\ #\q))
    (make-ast-string '(0 . 3) gn "\xFFFD;")
    '(((1 . 2) "invalid escape sequence \\q")
      ((0 . 3) "unterminated string"))))

(test-group
  "expander identifiers"
  (check
    "a"
    (make-ast-identifier
      '(0 . 1)
      gn
      'a))

  (check
    "set!"
    (make-ast-identifier
      '(0 . 4)
      gn
      'set!))

  (check
    "hello-world"
    (make-ast-identifier
      '(0 . 11)
      gn
      'hello-world)))

(test-group
  "expander procedure calls"
  (check
    "(f)"
    (make-ast-list
      '(0 . 3)
      gn
      (list
	(make-ast-identifier
	  '(1 . 1)
	  gn
	  'f))))

  (check
    "(char=? #\\a #\\a)"
    (make-ast-list
      '(0 . 16)
      gn
      (list
	(make-ast-identifier
	  '(1 . 6)
	  gn
	  'char=?)
	(make-ast-char
	  '(8 . 3)
	  gn
	  #\a)
	(make-ast-char
	  '(12 . 3)
	  gn
	  #\a)))))

(test-group
  "expander simple error recovery"

  (check
    "()"
    (make-ast-null '(0 . 2) gn)
    '(((0 . 2) "empty lists not allowed" (#f . "try '()"))))

  (check
    "(a . ())"
    (make-ast-list
      '(0 . 8)
      gn
      (list
	(make-ast-identifier
	  '(1 . 1)
	  gn
	  'a)))
    '(((3 . 1) "dot '.' not allowed in this context")))

  (check
    "(id ())"
    (make-ast-list
      '(0 . 7)
      gn
      (list
	(make-ast-identifier
	  '(1 . 2)
	  gn
	  'id)
	(make-ast-null '(4 . 2) gn)))
    '(((4 . 2) "empty lists not allowed" (#f . "try '()")))))

(test-group
  "expander if"
  (check
    "(if #t #\\a #f)"
    (make-ast-if
      '(0 . 14)
      gn
      (make-ast-boolean
	'(4 . 2)
	gn
	#t)
      (make-ast-char
	'(7 . 3)
	gn
	#\a)
      (make-ast-boolean
	'(11 . 2)
	gn
	#f)))

  (check
    "(if #f a)"
    (make-ast-if
      '(0 . 9)
      gn
      (make-ast-boolean
	'(4 . 2)
	gn
	#f)
      (make-ast-identifier
	'(7 . 1)
	gn
	'a)
      (make-ast-unspecified '(0 . 9) gn))))

(test-group
  "expander if error"
  (check
    "(if)"
    (make-ast-if
      '(0 . 4)
      gn
      (make-ast-unspecified '(0 . 4) gn)
      (make-ast-unspecified '(0 . 4) gn)
      (make-ast-unspecified '(0 . 4) gn))
    '(((3 . 1) "expected a condition")
      ((3 . 1) "expected a true branch")))

  (check
    "(if #t)"
    (make-ast-if
      '(0 . 7)
      gn
      (make-ast-boolean '(4 . 2) gn #t)
      (make-ast-unspecified '(0 . 7) gn)
      (make-ast-unspecified '(0 . 7) gn))
    '(((6 . 1) "expected a true branch")))

  (check
    "(if #t . #t)"
    (make-ast-if
      '(0 . 12)
      gn
      (make-ast-boolean '(4 . 2) gn #t)
      (make-ast-unspecified '(0 . 12) gn)
      (make-ast-unspecified '(0 . 12) gn))
    '(((7 . 1) "expected a true branch")
      ((7 . 1) "dot '.' not allowed in this context")))

  (check
    "(if #t #t #t #t)"
    (make-ast-if
      '(0 . 16)
      gn
      (make-ast-boolean '(4 . 2) gn #t)
      (make-ast-boolean '(7 . 2) gn #t)
      (make-ast-boolean '(10 . 2) gn #t))
    '(((13 . 2) "expected ), found #t")))

  (check
    "[if #t #t #t #f]"
    (make-ast-if
      '(0 . 16)
      gn
      (make-ast-boolean '(4 . 2) gn #t)
      (make-ast-boolean '(7 . 2) gn #t)
      (make-ast-boolean '(10 . 2) gn #t))
    '(((13 . 2) "expected ], found #f"))))

(test-group
  "expander define"
  (check
    "(define v)"
    (make-ast-define
      '(0 . 10)
      gn
      (make-ast-identifier '(8 . 1) gn 'v)
      (make-ast-unspecified '(0 . 10) gn)))

  (check
    "(define b #t)"
    (make-ast-define
      '(0 . 13)
      gn
      (make-ast-identifier '(8 . 1) gn 'b)
      (make-ast-boolean '(10 . 2) gn #t)))

  (check
    "(define a #t)
     (define b a)"
    (list
      (make-ast-define
	'(0 . 13)
	gn
	(make-ast-identifier '(8 . 1) gn 'a)
	(make-ast-boolean '(10 . 2) gn #t))
      (make-ast-define
	'(19 . 12)
	gn
	(make-ast-identifier '(27 . 1) gn 'b)
	(make-ast-identifier '(29 . 1) gn 'a))))

  (check
    "(define)"
    '()
    '(((7 . 1) "expected a variable name or open delimiter")))

  (check
    "(define #t)"
    '()
    '(((8 . 2) "expected an identifier or an open delimiter, found #t")))

  (check
    "(define a #t #f)"
    (make-ast-define
      '(0 . 16)
      gn
      (make-ast-identifier
	'(8 . 1)
	gn
	'a)
      (make-ast-boolean
	'(10 . 2)
	gn
	#t))
    '(((13 . 2) "expected ), found #f")))

  (check
    "[define a #t #f]"
    (make-ast-define
      '(0 . 16)
      gn
      (make-ast-identifier
	'(8 . 1)
	gn
	'a)
      (make-ast-boolean
	'(10 . 2)
	gn
	#t))
    '(((13 . 2) "expected ], found #f")))

  (check
    "(define a . #f)"
    (make-ast-define
      '(0 . 15)
      gn
      (make-ast-identifier
	'(8 . 1)
	gn
	'a)
      (make-ast-unspecified '(0 . 15) gn))
    '(((10 . 1) "dot '.' not allowed in this context"))))

(test-group
  "expander lambda"
  (check
    "(lambda (x) x)"
    (make-ast-lambda
      '(0 . 14)
      gn
      (list
	(make-ast-identifier '(9 . 1) gn 'x))
      #f
      (make-ast-let
	'(0 . 14)
	gn
	'letrec*
	'()
	(list
	  (make-ast-identifier '(12 . 1) gn 'x)))))

  (check
    "(lambda (x y) y)"
    (make-ast-lambda
      '(0 . 16)
      gn
      (list
	(make-ast-identifier '(9 . 1) gn 'x)
	(make-ast-identifier '(11 . 1) gn 'y))
      #f
      (make-ast-let
	'(0 . 16)
	gn
	'letrec*
	'()
	(list
	  (make-ast-identifier '(14 . 1) gn 'y)))))

  (check
    "(lambda (x . rest) rest)"
    (make-ast-lambda
      '(0 . 24)
      gn
      (list
	(make-ast-identifier '(9 . 1) gn 'x))
      (make-ast-identifier '(13 . 4) gn 'rest)
      (make-ast-let
	'(0 . 24)
	gn
	'letrec*
	'()
	(list
	  (make-ast-identifier '(19 . 4) gn 'rest)))))

  (check
    "(lambda x x)"
    (make-ast-lambda
      '(0 . 12)
      gn
      '()
      (make-ast-identifier '(8 . 1) gn 'x)
      (make-ast-let
	'(0 . 12)
	gn
	'letrec*
	'()
	(list
	  (make-ast-identifier '(10 . 1) gn 'x)))))

  (check
    "(lambda x (x) (x))"
    (make-ast-lambda
      '(0 . 18)
      gn
      '()
      (make-ast-identifier '(8 . 1) gn 'x)
      (make-ast-let
	'(0 . 18)
	gn
	'letrec*
	'()
	(list
	  (make-ast-list
	    '(10 . 3)
	    gn
	    (list
	      (make-ast-identifier '(11 . 1) gn 'x)))
	  (make-ast-list
	    '(14 . 3)
	    gn
	    (list
	      (make-ast-identifier '(15 . 1) gn 'x)))))))

  (check
    "(lambda (x) (define a x) a)"
    (make-ast-lambda
      '(0 . 27)
      gn
      (list (make-ast-identifier '(9 . 1) gn 'x))
      #f
      (make-ast-let
	'(0 . 27)
	gn
	'letrec*
	(list
	  (cons (make-ast-identifier '(20 . 1) gn 'a)
		(make-ast-identifier '(22 . 1) gn 'x)))
	(list
	  (make-ast-identifier '(25 . 1) gn 'a)))))

  (check
    "(lambda (x) x (define a x))"
    (make-ast-lambda
      '(0 . 27)
      gn
      (list (make-ast-identifier '(9 . 1) gn 'x))
      #f
      (make-ast-let
	'(0 . 27)
	gn
	'letrec*
	(list
	  (cons (make-ast-identifier '(22 . 1) gn 'a)
		(make-ast-identifier '(24 . 1) gn 'x)))
	(list
	  (make-ast-identifier '(12 . 1) gn 'x))))
    '(((14 . 12) "definitions are not allowed after the first expression")))

  (check
    "(lambda (#f . #t) x)"
    (make-ast-lambda
      '(0 . 20)
      gn
      '()
      (make-ast-identifier
	'(14 . 2)
	gn
	(string->symbol "|#t|"))
      (make-ast-let
	'(0 . 20)
	gn
	'letrec*
	'()
	(list
	  (make-ast-identifier '(18 . 1) gn 'x))))
    '(((9 . 2) "expected an identifier, found #f")
      ((14 . 2) "expected an identifier, found #t")))

  (check
    "(lambda #t x)"
    (make-ast-lambda
      '(0 . 13)
      gn
      '()
      (make-ast-identifier
	'(8 . 2)
	gn
	(string->symbol "|#t|"))
      (make-ast-let
	'(0 . 13)
	gn
	'letrec*
	'()
	(list
	  (make-ast-identifier '(11 . 1) gn 'x))))
    '(((8 . 2) "expected an identifier or an open delimiter, found #t"))))

(test-group
  "expander quote"
  (check
    "(quote a)"
    (make-ast-symbol
      '(0 . 9)
      gn
      'a))

  (check
    "(quote #t)"
    (make-ast-boolean
      '(0 . 10)
      gn
      #t))

  (check
    "(quote #f)"
    (make-ast-boolean
      '(0 . 10)
      gn
      #f))

  (check
    "(quote #\\a)"
    (make-ast-char
      '(0 . 11)
      gn
      #\a))

  (check
    "(quote ())"
    (make-ast-null '(0 . 10) gn))

  (check
    "(quote \"hi\")"
    (make-ast-string '(0 . 12) gn "hi")))

(test-group
  "expander multiple atoms"
  (check
    "#t #f"
    (list
      (make-ast-boolean '(0 . 2) gn #t)
      (make-ast-boolean '(3 . 2) gn #f))))
