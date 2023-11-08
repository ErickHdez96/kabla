#!/usr/bin/env guile \
-e main -s
!#
(use-modules (conifer)
	     (quest))

(define (main args)
  (define t
    (let ([b (conifer-green-node-builder)])
      (conifer-start-node b 'root)

      (conifer-start-node b 'bin-expr)

      (conifer-start-node b 'literal)
      (conifer-push-token b 'int-number "5")
      (conifer-finish-node b)

      (conifer-push-token b 'whitespace " ")
      (conifer-push-token b 'plus "+")
      (conifer-push-token b 'whitespace " ")

      (conifer-start-node b 'literal)
      (conifer-push-token b 'int-number "10")
      (conifer-finish-node b)

      (conifer-finish-node b)

      (conifer-finish-node b)
      (conifer-make-view (conifer-finish-builder b))))
  (display (conifer-tree->string t))
  (newline))
