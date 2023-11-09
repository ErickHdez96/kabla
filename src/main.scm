#!/usr/bin/env guile \
-e main -s
!#
(use-modules (syntax scanner))

(define (main args)
  (define test-complete-tokens
    (lambda (kind str)
      (display (scan-string str))
      (newline)))

  (test-complete-tokens 'string "\"Hello, world!\"")
  )
