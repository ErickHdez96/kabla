(import (srfi srfi-64)
	(syntax shortcuts)
	(syntax ast))

(test-runner-current (test-runner-simple))

(define any->item
  (lambda (p)
    (cond
      [(ast-expr? p) p]
      [(ast-define? p) p]
      [else (error 'any->item
		   "expected a valid expression pair: ~a"
		   p)])))

(define-syntax chek
  (syntax-rules ()
    [(_ str expected-item)
     (let ([result (expand-str str)])
       (test-eqv
	 1
	 (length (ast-root-items (car result))))
       (test-equal
	 '()
	 (cdr result))
       (test-equal
	 expected-item
	 (car (ast-root-items (car result)))))]))

(define check
  (lambda (str expected-ast . expected-errors)
    (let ([result (expand-str str)])
      (cond
	[(or (ast-expr? expected-ast)
	     (ast-define? expected-ast))
	 (test-eqv
	   1
	   (length (ast-root-items (car result))))
	 (test-equal
	   (any->item expected-ast)
	   (car (ast-root-items (car result))))]
	[else
	  (test-equal
	    (map any->item expected-ast)
	    (ast-root-items (car result)))])

      (test-equal
	(if (null? expected-errors)
	  '()
	  (car expected-errors))
	(cdr result)))))

(test-group
  "expander boolean"
  (check
    "#t"
    (make-ast-boolean '(0 . 2) #t))

  (check
    "#T"
    (make-ast-boolean '(0 . 2) #t))

  (check
    "#f"
    (make-ast-boolean '(0 . 2) #f))

  (check
    "#F"
    (make-ast-boolean '(0 . 2) #f)))

(test-group
  "expander char"
  (check
    "#\\a"
    (make-ast-char '(0 . 3) #\a))

  (check
    "#\\λ"
    (make-ast-char '(0 . 3) #\λ))

  (check
    "#\\nul"
    (make-ast-char '(0 . 5) #\nul))

  (check
    "#\\alarm"
    (make-ast-char '(0 . 7) #\alarm))

  (check
    "#\\backspace"
    (make-ast-char '(0 . 11) #\backspace))

  (check
    "#\\tab"
    (make-ast-char '(0 . 5) #\tab))

  (check
    "#\\linefeed"
    (make-ast-char '(0 . 10) #\linefeed))

  (check
    "#\\newline"
    (make-ast-char '(0 . 9) #\newline))

  (check
    "#\\vtab"
    (make-ast-char '(0 . 6) #\vtab))

  (check
    "#\\page"
    (make-ast-char '(0 . 6) #\page))

  (check
    "#\\return"
    (make-ast-char '(0 . 8) #\return))

  (check
    "#\\esc"
    (make-ast-char '(0 . 5) #\esc))

  (check
    "#\\space"
    (make-ast-char '(0 . 7) #\space))

  (check
    "#\\delete"
    (make-ast-char '(0 . 8) #\delete))

  (check
    "#\\x3bb"
    (make-ast-char '(0 . 6) #\λ))

  (check
    "#\\x3c0"
    (make-ast-char '(0 . 6) #\π))

  (check
    "#\\x2654"
    (make-ast-char '(0 . 7) #\♔)))

(test-group
  "expander char error"
  (check
    "#\\Space"
    (make-ast-char '(0 . 7) #\xFFFD)
    '(((0 . 7) "invalid character name: Space")))

  (check
    "#\\xDAAA"
    (make-ast-char '(0 . 7) #\xFFFD)
    '(((0 . 7)
       "hex scalar value must be in range [#x0, #xD7FF] ∪ [#xE000, #x10FFFF]"))))

(test-group
  "expander string"
  (check
    (list->string '(#\" #\a #\"))
    (make-ast-string '(0 . 3) "a"))

  (check
    (list->string '(#\" #\\ #\a #\"))
    (make-ast-string '(0 . 4) "\a"))

  (check
    (list->string '(#\" #\\ #\b #\"))
    (make-ast-string '(0 . 4) "\b"))

  (check
    (list->string '(#\" #\\ #\t #\"))
    (make-ast-string '(0 . 4) "\t"))

  (check
    (list->string '(#\" #\\ #\n #\"))
    (make-ast-string '(0 . 4) "\n"))

  (check
    (list->string '(#\" #\\ #\v #\"))
    (make-ast-string '(0 . 4) "\v"))

  (check
    (list->string '(#\" #\\ #\f #\"))
    (make-ast-string '(0 . 4) "\f"))

  (check
    (list->string '(#\" #\\ #\r #\"))
    (make-ast-string '(0 . 4) "\r"))

  (check
    (list->string '(#\" #\\ #\" #\"))
    (make-ast-string '(0 . 4) "\""))

  (check
    (list->string '(#\" #\\ #\\ #\"))
    (make-ast-string '(0 . 4) "\\")))

(test-group
  "expander string error"

  (check
    (list->string '(#\"))
    (make-ast-string '(0 . 1) "")
    '(((0 . 1) "unterminated string")))

  (check
    (list->string '(#\" #\\ #\q))
    (make-ast-string '(0 . 3) "\xFFFD;")
    '(((1 . 2) "invalid escape sequence \\q")
      ((0 . 3) "unterminated string"))))

(test-group
  "expander identifiers"
  (check
    "a"
    (make-ast-identifier
      '(0 . 1)
      'a))

  (check
    "set!"
    (make-ast-identifier
      '(0 . 4)
      'set!))

  (check
    "hello-world"
    (make-ast-identifier
      '(0 . 11)
      'hello-world)))

(test-group
  "expander procedure calls"
  (check
    "(f)"
    (make-ast-list
      '(0 . 3)
      (list
	(make-ast-identifier
	  '(1 . 1)
	  'f))))

  (check
    "(char=? #\\a #\\a)"
    (make-ast-list
      '(0 . 16)
      (list
	(make-ast-identifier
	  '(1 . 6)
	  'char=?)
	(make-ast-char
	  '(8 . 3)
	  #\a)
	(make-ast-char
	  '(12 . 3)
	  #\a)))))

(test-group
  "expander simple error recovery"

  (check
    "()"
    (make-ast-null '(0 . 2))
    '(((0 . 2) "empty lists not allowed" (#f . "try '()"))))

  (check
    "(a . ())"
    (make-ast-list
      '(0 . 8)
      (list
	(make-ast-identifier
	  '(1 . 1)
	  'a)))
    '(((3 . 1) "dot '.' not allowed in this context")))

  (check
    "(id ())"
    (make-ast-list
      '(0 . 7)
      (list
	(make-ast-identifier
	  '(1 . 2)
	  'id)
	(make-ast-null '(4 . 2))))
    '(((4 . 2) "empty lists not allowed" (#f . "try '()")))))

(test-group
  "expander if"
  (check
    "(if #t #\\a #f)"
    (make-ast-if
      '(0 . 14)
      (make-ast-boolean
	'(4 . 2)
	#t)
      (make-ast-char
	'(7 . 3)
	#\a)
      (make-ast-boolean
	'(11 . 2)
	#f)))

  (check
    "(if #f a)"
    (make-ast-if
      '(0 . 9)
      (make-ast-boolean
	'(4 . 2)
	#f)
      (make-ast-identifier
	'(7 . 1)
	'a)
      (make-ast-unspecified
	'(0 . 9)))))

(test-group
  "expander if error"
  (check
    "(if)"
    (make-ast-if
      '(0 . 4)
      (make-ast-unspecified
	'(0 . 4))
      (make-ast-unspecified
	'(0 . 4))
      (make-ast-unspecified
	'(0 . 4)))
    '(((3 . 1) "expected a condition")
      ((3 . 1) "expected a true branch")))

  (check
    "(if #t)"
    (make-ast-if
      '(0 . 7)
      (make-ast-boolean
	'(4 . 2) #t)
      (make-ast-unspecified
	'(0 . 7))
      (make-ast-unspecified
	'(0 . 7)))
    '(((6 . 1) "expected a true branch")))

  (check
    "(if #t . #t)"
    (make-ast-if
      '(0 . 12)
      (make-ast-boolean
	'(4 . 2) #t)
      (make-ast-unspecified
	'(0 . 12))
      (make-ast-unspecified
	'(0 . 12)))
    '(((7 . 1) "expected a true branch")
      ((7 . 1) "dot '.' not allowed in this context")))

  (check
    "(if #t #t #t #t)"
    (make-ast-if
      '(0 . 16)
      (make-ast-boolean
	'(4 . 2) #t)
      (make-ast-boolean
	'(7 . 2) #t)
      (make-ast-boolean
	'(10 . 2) #t))
    '(((13 . 2) "expected ), found #t")))

  (check
    "[if #t #t #t #f]"
    (make-ast-if
      '(0 . 16)
      (make-ast-boolean
	'(4 . 2) #t)
      (make-ast-boolean
	'(7 . 2) #t)
      (make-ast-boolean
	'(10 . 2) #t))
    '(((13 . 2) "expected ], found #f"))))

(test-group
  "expander define"
  (check
    "(define v)"
    (make-ast-define
      '(0 . 10)
      (make-ast-identifier '(8 . 1) 'v)
      (make-ast-unspecified '(0 . 10))))

  (check
    "(define b #t)"
    (make-ast-define
      '(0 . 13)
      (make-ast-identifier '(8 . 1) 'b)
      (make-ast-boolean '(10 . 2) #t)))

  (check
    "(define a #t)
     (define b a)"
    (list
      (make-ast-define
	'(0 . 13)
	(make-ast-identifier '(8 . 1) 'a)
	(make-ast-boolean '(10 . 2) #t))
      (make-ast-define
	'(19 . 12)
	(make-ast-identifier '(27 . 1) 'b)
	(make-ast-identifier '(29 . 1) 'a))))

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
      (make-ast-identifier
	'(8 . 1)
	'a)
      (make-ast-boolean
	'(10 . 2)
	#t))
    '(((13 . 2) "expected ), found #f")))

  (check
    "[define a #t #f]"
    (make-ast-define
      '(0 . 16)
      (make-ast-identifier
	'(8 . 1)
	'a)
      (make-ast-boolean
	'(10 . 2)
	#t))
    '(((13 . 2) "expected ], found #f")))

  (check
    "(define a . #f)"
    (make-ast-define
      '(0 . 15)
      (make-ast-identifier
	'(8 . 1)
	'a)
      (make-ast-unspecified '(0 . 15)))
    '(((10 . 1) "dot '.' not allowed in this context"))))

(test-group
  "expander lambda"

  (check
    "(lambda (x) x)"
    (make-ast-lambda
      '(0 . 14)
      (list
	(make-ast-identifier '(9 . 1) 'x))
      #f
      (make-ast-let
	'(0 . 14)
	'letrec*
	'()
	(list
	  (make-ast-identifier '(12 . 1) 'x)))))

  (check
    "(lambda (x y) y)"
    (make-ast-lambda
      '(0 . 16)
      (list
	(make-ast-identifier '(9 . 1) 'x)
	(make-ast-identifier '(11 . 1) 'y))
      #f
      (make-ast-let
	'(0 . 16)
	'letrec*
	'()
	(list
	  (make-ast-identifier '(14 . 1) 'y)))))

  (check
    "(lambda (x . rest) rest)"
    (make-ast-lambda
      '(0 . 24)
      (list
	(make-ast-identifier '(9 . 1) 'x))
      (make-ast-identifier '(13 . 4) 'rest)
      (make-ast-let
	'(0 . 24)
	'letrec*
	'()
	(list
	  (make-ast-identifier '(19 . 4) 'rest)))))

  (check
    "(lambda x x)"
    (make-ast-lambda
      '(0 . 12)
      '()
      (make-ast-identifier '(8 . 1) 'x)
      (make-ast-let
	'(0 . 12)
	'letrec*
	'()
	(list
	  (make-ast-identifier '(10 . 1) 'x)))))

  (check
    "(lambda x (x) (x))"
    (make-ast-lambda
      '(0 . 18)
      '()
      (make-ast-identifier '(8 . 1) 'x)
      (make-ast-let
	'(0 . 18)
	'letrec*
	'()
	(list
	  (make-ast-list
	    '(10 . 3)
	    (list
	      (make-ast-identifier '(11 . 1) 'x)))
	  (make-ast-list
	    '(14 . 3)
	    (list
	      (make-ast-identifier '(15 . 1) 'x)))))))

  (check
    "(lambda (x) (define a x) a)"
    (make-ast-lambda
      '(0 . 27)
      (list (make-ast-identifier '(9 . 1) 'x))
      #f
      (make-ast-let
	'(0 . 27)
	'letrec*
	(list
	  (cons (make-ast-identifier '(20 . 1) 'a)
		(make-ast-identifier '(22 . 1) 'x)))
	(list
	  (make-ast-identifier '(25 . 1) 'a)))))

  (check
    "(lambda (x) x (define a x))"
    (make-ast-lambda
      '(0 . 27)
      (list (make-ast-identifier '(9 . 1) 'x))
      #f
      (make-ast-let
	'(0 . 27)
	'letrec*
	(list
	  (cons (make-ast-identifier '(22 . 1) 'a)
		(make-ast-identifier '(24 . 1) 'x)))
	(list
	  (make-ast-identifier '(12 . 1) 'x))))
    '(((14 . 12) "definitions are not allowed after the first expression")))

  (check
    "(lambda (#f . #t) x)"
    (make-ast-lambda
      '(0 . 20)
      '()
      (make-ast-identifier
	'(14 . 2)
	(string->symbol "|#t|"))
      (make-ast-let
	'(0 . 20)
	'letrec*
	'()
	(list
	  (make-ast-identifier '(18 . 1) 'x))))
    '(((9 . 2) "expected an identifier, found #f")
      ((14 . 2) "expected an identifier, found #t")))

  (check
    "(lambda #t x)"
    (make-ast-lambda
      '(0 . 13)
      '()
      (make-ast-identifier
	'(8 . 2)
	(string->symbol "|#t|"))
      (make-ast-let
	'(0 . 13)
	'letrec*
	'()
	(list
	  (make-ast-identifier '(11 . 1) 'x))))
    '(((8 . 2) "expected an identifier or an open delimiter, found #t"))))

(test-group
  "expander multiple atoms"
  (check
    "#t #f"
    (list
      (make-ast-boolean '(0 . 2) #t)
      (make-ast-boolean '(3 . 2) #f))))
