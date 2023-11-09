(define-module (common)
	       #:export (char-utf8-len
			  fmap
			  ok
			  err
			  dbg))

; Returns the length in bytes the character takes in utf8.
(define (char-utf8-len c)
  (let ([n (char->integer c)])
    (cond
      [(<= n #x7F) 1]
      [(<= n #x7FF) 2]
      [(<= n #xFFFF) 3]
      [else 4])))

(define ok
  (lambda (v . extra)
    (cons 'ok
	  (if (null? extra)
	    v
	    (cons v extra)))))

(define err
  (lambda (e . extra)
    (cons 'error
	  (if (null? extra)
	    e
	    (cons e extra)))))

(define is-error?
  (lambda (x)
    (and (pair? x)
	 (eq? 'error (car x)))))

(define fmap
  (lambda (f m)
    (cond
      [(and (pair? m)
	    (eq? 'ok (car m)))
       (cons 'ok
	     (f (cdr m)))]
      [(and (pair? m)
	    (eq? 'error (car m)))
       m]
      [m (f m)]
      [else m])))

(define dbg
  (lambda (x)
    (display x)
    (newline)
    x))
