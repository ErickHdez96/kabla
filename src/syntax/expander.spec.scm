(import (rnrs base)
	(srfi srfi-64)
	(conifer)
	(syntax shortcuts)
	(syntax ast)
	(common))

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
       (assert (= (vector-length expected)
		  (vector-length actual)))
       (vector-for-each
	 check-expression
	 expected
	 actual)]
      [(ast-expr? expected)
       (cond
	 [(or (not (ast-expr? actual)) 
	      (not (equal? (ast-expr-offset expected) (ast-expr-offset actual)))
	      (not (equal? (ast-expr-kind expected) (ast-expr-kind actual))))
	  (test-equal expected actual)]
	 [else
	   (check-expression (ast-expr-value expected)
			     (ast-expr-value actual))])]
      [(ast-define? expected)
       (cond
	 [(or (not (ast-define? actual)) 
	      (not (equal? (ast-define-offset expected) (ast-define-offset actual))))
	  (test-equal expected actual)]
	 [else
	   (check-expression (ast-define-variable expected)
			     (ast-define-variable actual))
	   (check-expression (ast-define-expr expected)
			     (ast-define-expr actual))])]
      [(pair? expected)
       (assert (pair? actual))
       (check-expression (car expected) (car actual))
       (check-expression (cdr expected) (cdr actual))]
      [(null? expected) (assert (null? actual))]
      [else (error 'check-expression
		   "unknown kind: ~a - ~a"
		   expected
		   actual)])))

(define check-node
  (lambda (expected actual)
    (cond
      [(ast-expr? expected)
       (cond
	 [(or (not (ast-expr? actual)) 
	      (not (equal? (ast-expr-offset expected) (ast-expr-offset actual)))
	      (not (equal? (ast-expr-kind expected) (ast-expr-kind actual))))
	  (test-equal expected actual)]
	 [else
	   (check-expression (ast-expr-value expected)
			     (ast-expr-value actual))])]
      [(ast-define? expected)
       (cond
	 [(or (not (ast-define? actual)) 
	      (not (equal? (ast-define-offset expected) (ast-define-offset actual))))
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
	 (check-expression
	   expected-ast
	   (car (ast-root-items (car result))))]
	[else
	  (for-each
	    (lambda (e a)
	      (check-expression e a))
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
    (make-ast-boolean 0 gn #t))

  (check
    "#T"
    (make-ast-boolean 0 gn #t))

  (check
    "#f"
    (make-ast-boolean 0 gn #f))

  (check
    "#F"
    (make-ast-boolean 0 gn #f)))

(test-group
  "expander char"
  (check
    "#\\a"
    (make-ast-char 0 gn #\a))

  (check
    "#\\λ"
    (make-ast-char 0 gn #\λ))

  (check
    "#\\nul"
    (make-ast-char 0 gn #\nul))

  (check
    "#\\alarm"
    (make-ast-char 0 gn #\alarm))

  (check
    "#\\backspace"
    (make-ast-char 0 gn #\backspace))

  (check
    "#\\tab"
    (make-ast-char 0 gn #\tab))

  (check
    "#\\linefeed"
    (make-ast-char 0 gn #\linefeed))

  (check
    "#\\newline"
    (make-ast-char 0 gn #\newline))

  (check
    "#\\vtab"
    (make-ast-char 0 gn #\vtab))

  (check
    "#\\page"
    (make-ast-char 0 gn #\page))

  (check
    "#\\return"
    (make-ast-char 0 gn #\return))

  (check
    "#\\esc"
    (make-ast-char 0 gn #\esc))

  (check
    "#\\space"
    (make-ast-char 0 gn #\space))

  (check
    "#\\delete"
    (make-ast-char 0 gn #\delete))

  (check
    "#\\x3bb"
    (make-ast-char 0 gn #\λ))

  (check
    "#\\x3c0"
    (make-ast-char 0 gn #\π))

  (check
    "#\\x2654"
    (make-ast-char 0 gn #\♔)))

(test-group
  "expander char error"
  (check
    "#\\Space"
    (make-ast-char 0 gn #\xFFFD)
    '(((0 . 7) "invalid character name: Space")))

  (check
    "#\\xDAAA"
    (make-ast-char 0 gn #\xFFFD)
    '(((0 . 7)
       "hex scalar value must be in range [#x0, #xD7FF] ∪ [#xE000, #x10FFFF]"))))

(test-group
  "expander string"
  (check
    (list->string '(#\" #\a #\"))
    (make-ast-string 0 gn "a"))

  (check
    (list->string '(#\" #\\ #\a #\"))
    (make-ast-string 0 gn "\a"))

  (check
    (list->string '(#\" #\\ #\b #\"))
    (make-ast-string 0 gn "\b"))

  (check
    (list->string '(#\" #\\ #\t #\"))
    (make-ast-string 0 gn "\t"))

  (check
    (list->string '(#\" #\\ #\n #\"))
    (make-ast-string 0 gn "\n"))

  (check
    (list->string '(#\" #\\ #\v #\"))
    (make-ast-string 0 gn "\v"))

  (check
    (list->string '(#\" #\\ #\f #\"))
    (make-ast-string 0 gn "\f"))

  (check
    (list->string '(#\" #\\ #\r #\"))
    (make-ast-string 0 gn "\r"))

  (check
    (list->string '(#\" #\\ #\" #\"))
    (make-ast-string 0 gn "\""))

  (check
    (list->string '(#\" #\\ #\\ #\"))
    (make-ast-string 0 gn "\\")))

(test-group
  "expander string error"

  (check
    (list->string '(#\"))
    (make-ast-string 0 gn "")
    '(((0 . 1) "unterminated string")))

  (check
    (list->string '(#\" #\\ #\q))
    (make-ast-string 0 gn "\xFFFD;")
    '(((1 . 2) "invalid escape sequence \\q")
      ((0 . 3) "unterminated string"))))

(test-group
  "expander variables"
  (check
    "a"
    (make-ast-var
      0
      gn
      'a))

  (check
    "set!"
    (make-ast-var
      0
      gn
      'set!))

  (check
    "hello-world"
    (make-ast-var
      0
      gn
      'hello-world)))

(test-group
  "expander procedure calls"
  (check
    "(f)"
    (make-ast-proc-call
      0
      gn
      (make-ast-var 1 gn 'f)
      (list)))

  (check
    "(char=? #\\a #\\a)"
    (make-ast-proc-call
      0
      gn
      (make-ast-var 1 gn 'char=?)
      (list
	(make-ast-char
	  8
	  gn
	  #\a)
	(make-ast-char
	  12
	  gn
	  #\a)))))

(test-group
  "expander simple error recovery"

  (check
    "()"
    (make-ast-null 0 gn)
    '(((0 . 2) "empty lists not allowed" (#f . "try '()"))))

  (check
    "(a . ())"
    (make-ast-proc-call
      0
      gn
      (make-ast-var 1 gn 'a)
      (list))
    '(((3 . 1) "dot '.' not allowed in this context")))

  (check
    "(id ())"
    (make-ast-proc-call
      0
      gn
      (make-ast-var 1 gn 'id)
      (list
	(make-ast-null 4 gn)))
    '(((4 . 2) "empty lists not allowed" (#f . "try '()")))))

(test-group
  "expander if"
  (check
    "(if #t #\\a #f)"
    (make-ast-if
      0
      gn
      (make-ast-boolean
	4
	gn
	#t)
      (make-ast-char
	7
	gn
	#\a)
      (make-ast-boolean
	11
	gn
	#f)))

  (check
    "(if #f a)"
    (make-ast-if
      0
      gn
      (make-ast-boolean
	4
	gn
	#f)
      (make-ast-var
	7
	gn
	'a)
      (make-ast-unspecified 0 gn))))

(test-group
  "expander if error"
  (check
    "(if)"
    (make-ast-if
      0
      gn
      (make-ast-unspecified 0 gn)
      (make-ast-unspecified 0 gn)
      (make-ast-unspecified 0 gn))
    '(((3 . 1) "expected a condition")
      ((3 . 1) "expected a true branch")))

  (check
    "(if #t)"
    (make-ast-if
      0
      gn
      (make-ast-boolean 4 gn #t)
      (make-ast-unspecified 0 gn)
      (make-ast-unspecified 0 gn))
    '(((6 . 1) "expected a true branch")))

  (check
    "(if #t . #t)"
    (make-ast-if
      0
      gn
      (make-ast-boolean 4 gn #t)
      (make-ast-unspecified 0 gn)
      (make-ast-unspecified 0 gn))
    '(((7 . 1) "expected a true branch")
      ((7 . 1) "dot '.' not allowed in this context")))

  (check
    "(if #t #t #t #t)"
    (make-ast-if
      0
      gn
      (make-ast-boolean 4 gn #t)
      (make-ast-boolean 7 gn #t)
      (make-ast-boolean 10 gn #t))
    '(((13 . 2) "expected ), found #t")))

  (check
    "[if #t #t #t #f]"
    (make-ast-if
      0
      gn
      (make-ast-boolean 4 gn #t)
      (make-ast-boolean 7 gn #t)
      (make-ast-boolean 10 gn #t))
    '(((13 . 2) "expected ], found #f"))))

(test-group
  "expander define"
  (check
    "(define v)"
    (make-ast-define
      0
      gn
      (make-ast-var 8 gn 'v)
      (make-ast-unspecified 0 gn)))

  (check
    "(define b #t)"
    (make-ast-define
      0
      gn
      (make-ast-var 8 gn 'b)
      (make-ast-boolean 10 gn #t)))

  (check
    "(define a #t)
     (define b a)"
    (list
      (make-ast-define
	0
	gn
	(make-ast-var 8 gn 'a)
	(make-ast-boolean 10 gn #t))
      (make-ast-define
	19
	gn
	(make-ast-var 27 gn 'b)
	(make-ast-var 29 gn 'a)))))

(test-group
  "expander define error"
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
      0
      gn
      (make-ast-var
	8
	gn
	'a)
      (make-ast-boolean
	10
	gn
	#t))
    '(((13 . 2) "expected ), found #f")))

  (check
    "[define a #t #f]"
    (make-ast-define
      0
      gn
      (make-ast-var
	8
	gn
	'a)
      (make-ast-boolean
	10
	gn
	#t))
    '(((13 . 2) "expected ], found #f")))

  (check
    "(define a . #f)"
    (make-ast-define
      0
      gn
      (make-ast-var
	8
	gn
	'a)
      (make-ast-unspecified 0 gn))
    '(((10 . 1) "dot '.' not allowed in this context")))

  (check
    "((define a 2))"
    (make-ast-proc-call
      0
      gn
      (make-ast-unspecified 1 gn)
      (list))
    '(((1 . 12) "definitions are not allowed in this context"))))

(test-group
  "expander lambda"
  (check
    "(lambda (x) x)"
    (make-ast-lambda
      0
      gn
      (list
	(make-ast-var 9 gn 'x))
      #f
      (make-ast-let
	0
	gn
	'letrec*
	'()
	(list
	  (make-ast-var 12 gn 'x)))))

  (check
    "(lambda (x y) y)"
    (make-ast-lambda
      0
      gn
      (list
	(make-ast-var 9 gn 'x)
	(make-ast-var 11 gn 'y))
      #f
      (make-ast-let
	0
	gn
	'letrec*
	'()
	(list
	  (make-ast-var 14 gn 'y)))))

  (check
    "(lambda (x . rest) rest)"
    (make-ast-lambda
      0
      gn
      (list
	(make-ast-var 9 gn 'x))
      (make-ast-var 13 gn 'rest)
      (make-ast-let
	0
	gn
	'letrec*
	'()
	(list
	  (make-ast-var 19 gn 'rest)))))

  (check
    "(lambda x x)"
    (make-ast-lambda
      0
      gn
      '()
      (make-ast-var 8 gn 'x)
      (make-ast-let
	0
	gn
	'letrec*
	'()
	(list
	  (make-ast-var 10 gn 'x)))))

  (check
    "(lambda x (x) (x))"
    (make-ast-lambda
      0
      gn
      '()
      (make-ast-var 8 gn 'x)
      (make-ast-let
	0
	gn
	'letrec*
	'()
	(list
	  (make-ast-proc-call
	    10
	    gn
	    (make-ast-var 11 gn 'x)
	    (list))
	  (make-ast-proc-call
	    14
	    gn
	    (make-ast-var 15 gn 'x)
	    (list))))))

  (check
    "(lambda (x) (define a x) a)"
    (make-ast-lambda
      0
      gn
      (list (make-ast-var 9 gn 'x))
      #f
      (make-ast-let
	0
	gn
	'letrec*
	(list
	  (cons (make-ast-var 20 gn 'a)
		(make-ast-var 22 gn 'x)))
	(list
	  (make-ast-var 25 gn 'a)))))

  (check
    "(lambda (x) x (define a x))"
    (make-ast-lambda
      0
      gn
      (list (make-ast-var 9 gn 'x))
      #f
      (make-ast-let
	0
	gn
	'letrec*
	(list
	  (cons (make-ast-var 22 gn 'a)
		(make-ast-var 24 gn 'x)))
	(list
	  (make-ast-var 12 gn 'x))))
    '(((14 . 12) "definitions are not allowed after the first expression")))

  (check
    "(lambda (#f . #t) x)"
    (make-ast-lambda
      0
      gn
      '()
      (make-ast-var
	14
	gn
	(string->symbol "|#t|"))
      (make-ast-let
	0
	gn
	'letrec*
	'()
	(list
	  (make-ast-var 18 gn 'x))))
    '(((9 . 2) "expected an identifier, found #f")
      ((14 . 2) "expected an identifier, found #t")))

  (check
    "(lambda #t x)"
    (make-ast-lambda
      0
      gn
      '()
      (make-ast-var
	8
	gn
	(string->symbol "|#t|"))
      (make-ast-let
	0
	gn
	'letrec*
	'()
	(list
	  (make-ast-var 11 gn 'x))))
    '(((8 . 2) "expected an identifier or an open delimiter, found #t"))))

(test-group
  "expander quote"
  (check "(quote a)" (make-ast-symbol 0 gn 'a))
  (check "(quote #t)" (make-ast-boolean 0 gn #t))
  (check "(quote #f)" (make-ast-boolean 0 gn #f))
  (check "(quote #\\a)" (make-ast-char 0 gn #\a))
  (check "(quote ())" (make-ast-null 0 gn))

  (check
    "(quote (a b))"
    (make-ast-list
      0
      gn
      (list
	(make-ast-symbol 8 gn 'a)
	(make-ast-symbol 10 gn 'b))
      #t))

  (check
    "(quote (#t #f a b))"
    (make-ast-list
      0
      gn
      (list
	(make-ast-boolean 8 gn #t)
	(make-ast-boolean 11 gn #f)
	(make-ast-symbol 14 gn 'a)
	(make-ast-symbol 16 gn 'b))
      #t))

  (check
    "(quote (a . b))"
    (make-ast-list
      0
      gn
      (list
	(make-ast-symbol 8 gn 'a)
	(make-ast-symbol 12 gn 'b))
      #f))

  (check
    "(quote (a b . #t))"
    (make-ast-list
      0
      gn
      (list
	(make-ast-symbol 8 gn 'a)
	(make-ast-symbol 10 gn 'b)
	(make-ast-boolean 14 gn #t))
      #f))

  (check
    "(quote \"hi\")"
    (make-ast-string 0 gn "hi")))

(test-group
  "expander quote error"
  (check
    "[quote (a . b c)]"
    (make-ast-list
      0
      gn
      (list
	(make-ast-symbol 8 gn 'a)
	(make-ast-symbol 12 gn 'b))
      #f)
    '(((14 . 1) "expected ), found c")))

  (check
    "(quote [a . b #t])"
    (make-ast-list
      0
      gn
      (list
	(make-ast-symbol 8 gn 'a)
	(make-ast-symbol 12 gn 'b))
      #f)
    '(((14 . 2) "expected ], found #t"))))

(test-group
  "expander abbreviations"
  (check
    "'a"
    (make-ast-symbol
      0
      gn
      'a)))

(test-group
  "expander multiple atoms"
  (check
    "#t #f"
    (list
      (make-ast-boolean 0 gn #t)
      (make-ast-boolean 3 gn #f))))
