(import (srfi srfi-64)
	(syntax shortcuts)
	(except (syntax ast)
		make-ast-expr)
	(rename (syntax ast)
		(make-ast-expr expr)))

(test-runner-current (test-runner-simple))

(define pair->item
  (lambda (p)
    (cond
      [(or (char? (cdr p))
	   (boolean? (cdr p))
	   (string=? (cdr p)))
       (expr (car p) (cdr p))]
      [else (error 'pair->item
		   "expected a valid expression pair: ~a"
		   p)])))

(define check
  (lambda (str expected-ast . expected-errors)
    (let ([result (expand-str str)])
      (cond
	[(and (pair? expected-ast)
	      (not (pair? (cdr expected-ast))))
	 (test-eqv
	   1
	   (length (ast-root-items (car result))))
	 (test-equal
	   (pair->item expected-ast)
	   (car (ast-root-items (car result))))]
	[else
	  (test-equal
	    (map pair->item expected-ast)
	    (ast-root-items (car result)))])

      (test-equal
	(if (null? expected-errors)
	  '()
	  (car expected-errors))
	(cdr result)))))

(test-group
  "expander simple atoms"
  ; booleans
  (begin
    (check
      "#t"
      '((0 . 2) . #t))

    (check
      "#T"
      '((0 . 2) . #t))

    (check
      "#f"
      '((0 . 2) . #f))

    (check
      "#F"
      '((0 . 2) . #f)))

  ; chars
  (begin
    (check
      "#\\a"
      '((0 . 3) . #\a))

    (check
      "#\\λ"
      '((0 . 3) . #\λ))

    (check
      "#\\nul"
      '((0 . 5) . #\nul))

    (check
      "#\\alarm"
      '((0 . 7) . #\alarm))

    (check
      "#\\backspace"
      '((0 . 11) . #\backspace))

    (check
      "#\\tab"
      '((0 . 5) . #\tab))

    (check
      "#\\linefeed"
      '((0 . 10) . #\linefeed))

    (check
      "#\\newline"
      '((0 . 9) . #\newline))

    (check
      "#\\vtab"
      '((0 . 6) . #\vtab))

    (check
      "#\\page"
      '((0 . 6) . #\page))

    (check
      "#\\return"
      '((0 . 8) . #\return))

    (check
      "#\\esc"
      '((0 . 5) . #\esc))

    (check
      "#\\space"
      '((0 . 7) . #\space))

    (check
      "#\\delete"
      '((0 . 8) . #\delete))

    (check
      "#\\x3bb"
      '((0 . 6) . #\λ))

    (check
      "#\\x3c0"
      '((0 . 6) . #\π))

    (check
      "#\\x2654"
      '((0 . 7) . #\♔)))

  ; strings
  (begin
    (check
      (list->string '(#\" #\a #\"))
      '((0 . 3) . "a"))

    (check
      (list->string '(#\" #\\ #\a #\"))
      '((0 . 4) . "\a"))

    (check
      (list->string '(#\" #\\ #\b #\"))
      '((0 . 4) . "\b"))

    (check
      (list->string '(#\" #\\ #\t #\"))
      '((0 . 4) . "\t"))

    (check
      (list->string '(#\" #\\ #\n #\"))
      '((0 . 4) . "\n"))

    (check
      (list->string '(#\" #\\ #\v #\"))
      '((0 . 4) . "\v"))

    (check
      (list->string '(#\" #\\ #\f #\"))
      '((0 . 4) . "\f"))

    (check
      (list->string '(#\" #\\ #\r #\"))
      '((0 . 4) . "\r"))

    (check
      (list->string '(#\" #\\ #\" #\"))
      '((0 . 4) . "\""))

    (check
      (list->string '(#\" #\\ #\\ #\"))
      '((0 . 4) . "\\"))))

(test-group
  "expander multiple atoms"
  (check
    "#t #f"
    '(((0 . 2) . #t)
      ((3 . 2) . #f))))

(test-group
  "expander simple error recovery"
  (check
    "#\\Space"
    '((0 . 7) . #\xFFFD)
    '(((0 . 7) "invalid character name: Space")))

  (check
    "#\\xDAAA"
    '((0 . 7) . #\xFFFD)
    '(((0 . 7) "hex scalar value must be in range [#x0, #xD7FF] ∪ [#xE000, #x10FFFF]")))

  (check
    (list->string '(#\"))
    '((0 . 1) . "")
    '(((0 . 1) "unterminated string")))

  (check
    (list->string '(#\" #\\ #\q))
    '((0 . 3) . "\xFFFD;")
    '(((1 . 2) "invalid escape sequence \\q")
      ((0 . 3) "unterminated string"))))
