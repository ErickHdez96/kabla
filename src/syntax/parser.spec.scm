(use-modules (srfi srfi-64)
             (syntax parser)
             (syntax scanner)
             (conifer))

(test-runner-current (test-runner-simple))

(define remove-indent
  (lambda (s)
    (define lines
      (lambda (chars)
        (let loop ([chars chars]
                   [lines '()]
                   [line '()])
          (if (null? chars)
            (reverse (cons (reverse (cons #\linefeed line))
                           lines))
            (if (char=? #\linefeed (car chars))
              (loop (cdr chars)
                    (cons (reverse (cons #\linefeed line))
                          lines)
                    '())
              (loop (cdr chars)
                    lines
                    (cons (car chars)
                          line)))))))

    (let* ([chars (string->list s)]
           [lines (lines
                    (if (char=? #\linefeed (car chars))
                      (cdr chars)
                      chars))]
           [common-indent
             (let loop ([line (car lines)]
                        [count 0])
               (if (char=? #\space (car line))
                 (loop (cdr line) (+ count 1))
                 count))])
      (let loop ([lines (cdr lines)]
                 [line (car lines)]
                 [n 0]
                 [acc '()])
        (if (= n common-indent)
          (if (null? lines)
            (apply string-append
                   (reverse (map list->string (cons line acc))))
            (loop (cdr lines)
                  (car lines)
                  0
                  (cons line acc)))
          (loop lines
                (cdr line)
                (+ n 1)
                acc))))))

(define check
  (lambda (input expected . errors)
    (let ([parse-result (let ([tokens (scan-string input)])
                          (parse-tokens tokens))])
      (test-equal
        errors
        (cdr parse-result))
      (test-equal
        (remove-indent expected)
        (conifer-tree->debug-string (car parse-result))))))

(test-group
  "parser whole strings"
  (check
    "1"
    "
    root@0..1
      atom@0..1
        int-number@0..1 \"1\""))

(test-group
  "lists"
  (check
    "()"
    "
    root@0..2
      list@0..2
        open-delim@0..1 \"(\"
        close-delim@1..2 \")\"")

  (check
    "[]"
    "
    root@0..2
      list@0..2
        open-delim@0..1 \"[\"
        close-delim@1..2 \"]\"")

  (check
    "(1)"
    "
    root@0..3
      list@0..3
        open-delim@0..1 \"(\"
        atom@1..2
          int-number@1..2 \"1\"
        close-delim@2..3 \")\"")

  (check
    "[1]"
    "
    root@0..3
      list@0..3
        open-delim@0..1 \"[\"
        atom@1..2
          int-number@1..2 \"1\"
        close-delim@2..3 \"]\"")

  (check
    "(1 . 2)"
    "
    root@0..7
      list@0..7
        open-delim@0..1 \"(\"
        atom@1..2
          int-number@1..2 \"1\"
        whitespace@2..3 \" \"
        dot@3..4 \".\"
        whitespace@4..5 \" \"
        atom@5..6
          int-number@5..6 \"2\"
        close-delim@6..7 \")\"")

  (check
    "[1 . 2]"
    "
    root@0..7
      list@0..7
        open-delim@0..1 \"[\"
        atom@1..2
          int-number@1..2 \"1\"
        whitespace@2..3 \" \"
        dot@3..4 \".\"
        whitespace@4..5 \" \"
        atom@5..6
          int-number@5..6 \"2\"
        close-delim@6..7 \"]\"")

  (check
    "(+ 1 2)"
    "
    root@0..7
      list@0..7
        open-delim@0..1 \"(\"
        atom@1..2
          identifier@1..2 \"+\"
        whitespace@2..3 \" \"
        atom@3..4
          int-number@3..4 \"1\"
        whitespace@4..5 \" \"
        atom@5..6
          int-number@5..6 \"2\"
        close-delim@6..7 \")\""))

(test-group
  "recursive lists"
  (check
    "(let ([x 1]
           [y 2])
       (+ x y))"
    "
    root@0..45
      list@0..45
        open-delim@0..1 \"(\"
        atom@1..4
          identifier@1..4 \"let\"
        whitespace@4..5 \" \"
        list@5..29
          open-delim@5..6 \"(\"
          list@6..11
            open-delim@6..7 \"[\"
            atom@7..8
              identifier@7..8 \"x\"
            whitespace@8..9 \" \"
            atom@9..10
              int-number@9..10 \"1\"
            close-delim@10..11 \"]\"
          whitespace@11..23 \"\n               \"
          list@23..28
            open-delim@23..24 \"[\"
            atom@24..25
              identifier@24..25 \"y\"
            whitespace@25..26 \" \"
            atom@26..27
              int-number@26..27 \"2\"
            close-delim@27..28 \"]\"
          close-delim@28..29 \")\"
        whitespace@29..37 \"\n           \"
        list@37..44
          open-delim@37..38 \"(\"
          atom@38..39
            identifier@38..39 \"+\"
          whitespace@39..40 \" \"
          atom@40..41
            identifier@40..41 \"x\"
          whitespace@41..42 \" \"
          atom@42..43
            identifier@42..43 \"y\"
          close-delim@43..44 \")\"
        close-delim@44..45 \")\""))

(test-group
  "vectors"
  (check
    "#(1)"
    "
    root@0..4
      vector@0..4
        open-vector@0..2 \"#(\"
        atom@2..3
          int-number@2..3 \"1\"
        close-delim@3..4 \")\""))

(test-group
  "bytevectors"
  (check
    "#vu8(1)"
    "
    root@0..7
      bytevector@0..7
        open-bytevector@0..5 \"#vu8(\"
        int-number@5..6 \"1\"
        close-delim@6..7 \")\""))

(test-group
  "abbreviations"
  (check
    "'a"
    "
    root@0..2
      abbreviation@0..2
        quote@0..1 \"'\"
        atom@1..2
          identifier@1..2 \"a\"")

  (check
    "`()"
    "
    root@0..3
      abbreviation@0..3
        backtick@0..1 \"`\"
        list@1..3
          open-delim@1..2 \"(\"
          close-delim@2..3 \")\"")

  (check
    ",a"
    "
    root@0..2
      abbreviation@0..2
        comma@0..1 \",\"
        atom@1..2
          identifier@1..2 \"a\"")

  (check
    ",@a"
    "
    root@0..3
      abbreviation@0..3
        comma-at@0..2 \",@\"
        atom@2..3
          identifier@2..3 \"a\"")

  (check
    "#'()"
    "
    root@0..4
      abbreviation@0..4
        hash-quote@0..2 \"#'\"
        list@2..4
          open-delim@2..3 \"(\"
          close-delim@3..4 \")\"")

  (check
    "#`()"
    "
    root@0..4
      abbreviation@0..4
        hash-backtick@0..2 \"#`\"
        list@2..4
          open-delim@2..3 \"(\"
          close-delim@3..4 \")\"")

  (check
    "#,a"
    "
    root@0..3
      abbreviation@0..3
        hash-comma@0..2 \"#,\"
        atom@2..3
          identifier@2..3 \"a\"")

  (check
    "#,@a"
    "
    root@0..4
      abbreviation@0..4
        hash-comma-at@0..3 \"#,@\"
        atom@3..4
          identifier@3..4 \"a\""))

(test-group
  "error recovery"
  (check
    "(. 2)"
    "
    root@0..5
      list@0..5
        open-delim@0..1 \"(\"
        dot@1..2 \".\"
        whitespace@2..3 \" \"
        atom@3..4
          int-number@3..4 \"2\"
        close-delim@4..5 \")\""
    '((1 . 2) "expected at least one expression before dot '.'"))

  (check
    "(1 | 2)"
    "
    root@0..7
      list@0..7
        open-delim@0..1 \"(\"
        atom@1..2
          int-number@1..2 \"1\"
        whitespace@2..3 \" \"
        error@3..4 \"|\"
        whitespace@4..5 \" \"
        atom@5..6
          int-number@5..6 \"2\"
        close-delim@6..7 \")\""
    '((3 . 4) "expected an opening delimiter or an atom"))


  (check
    "(1 . . 2)"
    "
    root@0..9
      list@0..9
        open-delim@0..1 \"(\"
        atom@1..2
          int-number@1..2 \"1\"
        whitespace@2..3 \" \"
        dot@3..4 \".\"
        whitespace@4..5 \" \"
        dot@5..6 \".\"
        whitespace@6..7 \" \"
        atom@7..8
          int-number@7..8 \"2\"
        close-delim@8..9 \")\""
    '((5 . 6) "multiple dots '.' not allowed inside list"))

  (check
    "#(1 . 2)"
    "
    root@0..8
      vector@0..8
        open-vector@0..2 \"#(\"
        atom@2..3
          int-number@2..3 \"1\"
        whitespace@3..4 \" \"
        dot@4..5 \".\"
        whitespace@5..6 \" \"
        atom@6..7
          int-number@6..7 \"2\"
        close-delim@7..8 \")\""
    '((4 . 5) "dot '.' not allowed inside vector"))

  (check
    "#vu8(1 . 2)"
    "
    root@0..11
      bytevector@0..11
        open-bytevector@0..5 \"#vu8(\"
        int-number@5..6 \"1\"
        whitespace@6..7 \" \"
        dot@7..8 \".\"
        whitespace@8..9 \" \"
        int-number@9..10 \"2\"
        close-delim@10..11 \")\""
    '((7 . 8) "dot '.' not allowed inside bytevector")))

(test-group
  "invalid tokens"

  (check
    "#vU8()"
    "
    root@0..6
      bytevector@0..6
        open-bytevector@0..5 \"#vU8(\"
        close-delim@5..6 \")\""
    '((0 . 5) "#vu8( must be lowercase")) ; )
  )