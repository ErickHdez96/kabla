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
  (lambda (input expected)
    (let ([parse-result (let ([tokens (scan-string input)])
                          (parse-tokens tokens))])
      (test-equal
        '()
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
    "(1)"
    "
    root@0..3
      list@0..3
        open-delim@0..1 \"(\"
        atom@1..2
          int-number@1..2 \"1\"
        close-delim@2..3 \")\"")

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
