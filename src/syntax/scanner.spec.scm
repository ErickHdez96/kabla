(use-modules (srfi srfi-64)
	     (syntax scanner))

(test-runner-current (test-runner-simple))

(test-group
  "scanner whole strings"
  ; tests that the whole string scans into a single token.
  (define test-complete-tokens
    (lambda (kind str)
      (test-equal
	(list (cons kind str))
	(scan-string str))))

  (test-complete-tokens 'whitespace " ")
  (test-complete-tokens 'whitespace "     ")
  (test-complete-tokens 'whitespace "	")
  (test-complete-tokens 'whitespace "	 \t ")
  (test-complete-tokens 'whitespace "\n\n\t")

  (test-complete-tokens 'open-delim "(")
  (test-complete-tokens 'close-delim ")")
  (test-complete-tokens 'open-delim "[")
  (test-complete-tokens 'close-delim "]")
  (test-complete-tokens 'quote "'")
  (test-complete-tokens 'backtick "`")
  (test-complete-tokens 'dot ".")

  ; multitokens
  (test-complete-tokens 'comma ",")
  (test-complete-tokens 'comma-at ",@")

  (begin ; tokens starting with #
    (test-complete-tokens 'open-vector "#(")
    (test-complete-tokens 'hash-quote "#'")
    (test-complete-tokens 'hash-backtick "#`")
    (test-complete-tokens 'hash-comma "#,")
    (test-complete-tokens 'hash-comma-at "#,@")

    ; case insensitive booleans
    (test-complete-tokens 'true "#t")
    (test-complete-tokens 'false "#f")
    (test-complete-tokens 'true "#T")
    (test-complete-tokens 'false "#F")

    ; characters
    (test-complete-tokens 'char "#\\a")
    (test-complete-tokens 'char "#\\λ")
    (test-complete-tokens 'char "#\\\\") ; #\\

    ; character names
    (test-complete-tokens 'char "#\\nul")
    (test-complete-tokens 'char "#\\alarm")
    (test-complete-tokens 'char "#\\backspace")
    (test-complete-tokens 'char "#\\tab")
    (test-complete-tokens 'char "#\\linefeed")
    (test-complete-tokens 'char "#\\newline")
    (test-complete-tokens 'char "#\\vtab")
    (test-complete-tokens 'char "#\\page")
    (test-complete-tokens 'char "#\\return")
    (test-complete-tokens 'char "#\\esc")
    (test-complete-tokens 'char "#\\space")
    (test-complete-tokens 'char "#\\delete"))

  (test-complete-tokens 'identifier "a")
  (test-complete-tokens 'identifier "_")
  (test-complete-tokens 'identifier "+")
  (test-complete-tokens 'identifier "-")
  (test-complete-tokens 'identifier "hello-world$4")
  (test-complete-tokens 'identifier "!$%&*/:<=>?^_~")

  ; simple integers

  (test-complete-tokens 'int-number "3")
  (test-complete-tokens 'int-number "1000")

  ; strings
  (test-complete-tokens 'string "\"Hello, world!\"")

  ; erroneous tokens
  ; errors must be generated by the parser

  (test-complete-tokens 'char "#\\dElEte")
  (test-complete-tokens 'char "#\\Space")
  (test-complete-tokens 'char "#\\NUL")
  (test-complete-tokens 'char "#\\aa")
  (test-complete-tokens 'char "#\\x3BB")

  (test-complete-tokens 'open-bytevector "#vu8(")
  (test-complete-tokens 'open-bytevector "#Vu8(")
  (test-complete-tokens 'open-bytevector "#VU8(")
  (test-complete-tokens 'open-bytevector "#vU8(")

  (test-complete-tokens 'identifier ".a"))
