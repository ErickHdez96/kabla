(library
  (atty)
  (export red
	  red-bold
	  bold)
  (import (rnrs base)
	  (ice-9 format)
	  (common))

  (define red
    (lambda (msg . rest)
      (colored-output "31" (apply format #f msg rest))))

  (define red-bold
    (lambda (msg . rest)
      (colored-output "91" (apply format #f msg rest))))

  (define bold
    (lambda (msg . rest)
      (colored-output "97" (apply format #f msg rest))))

  (define colored-output
    (lambda (color msg)
      (format #f "\x1b;[~am~a\x1b;[m" color msg))))
