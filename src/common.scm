(define-module (common)
	       #:export (char-utf8-len))

; Returns the length in bytes the character takes in utf8.
(define (char-utf8-len c)
  (let ([n (char->integer c)])
    (cond
      [(<= n #x7F) 1]
      [(<= n #x7FF) 2]
      [(<= n #xFFFF) 3]
      [else 4])))
