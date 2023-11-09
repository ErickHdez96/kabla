(define-module (atty)
	       #:export (red))

(use-modules (ice-9 format)
	     (common))

(define red
  (lambda (msg . rest)
    (colored-output "31" (apply format #f msg rest))))

(define colored-output
  (lambda (color msg)
    (format #f "\x1b;[~am~a\x1b;[m" color msg)))
