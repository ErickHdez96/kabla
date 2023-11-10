(import (srfi srfi-64)
	(syntax shortcuts))

(test-runner-current (test-runner-simple))

(define check
  (lambda (str expected-ast . expected-errors)
    (let ([result (expand-str str)])
      (test-equal
	expected-ast
	(car result))

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
      '((0 . 7) . #\♔))
    ; error chars

    (check
      "#\\Space"
      '((0 . 7) . #\xFFFD)
      '(((0 . 7) "invalid character name: Space")))

    (check
      "#\\xDAAA"
      '((0 . 7) . #\xFFFD)
      '(((0 . 7) "hex scalar value must be in range [#x0, #xD7FF] or [#xE000, #x10FFFF]"))))

    )
