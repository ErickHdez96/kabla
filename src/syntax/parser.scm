(define-module (syntax parser)
	       #:export (parse-tokens))

(use-modules (rnrs records syntactic)
	     (conifer))

(define (dbg x)
  (display x)
  (newline)
  x)

(define-record-type
  parser
  (fields
    builder
    (mutable offset)
    (mutable tokens)
    (mutable errors)
    (mutable delimiter-stack)))

(define parse-tokens
  (lambda (tokens)
    (let ([p (make-parser (conifer-green-node-builder)
			  0
			  tokens
			  '()
			  '())])
      (start-node p 'root)

      (let loop ()
	(when (not (at-eof? p))
	  (parse-datum p)))

      (finish-node p)
      (let ([t (conifer-finish-builder (parser-builder p))])
	(cons t
	      (reverse (parser-errors p)))))))

(define parse-datum
  (lambda (p)
    (let* ([peek-t (peek p)]
	   [peek-sk (car peek-t)])
      (cond
	[(case peek-sk
	   [(char int-number true false identifier) #t]
	   [else #f])
	 (start-node p 'atom)
	 (bump p)
	 (finish-node p)]
	[(eq? 'open-delim peek-sk)
	 (start-node p 'list)
	 (push-delimiter p (cdr peek-t))
	 (let loop ()
	   (let* ([peek-t (peek p)]
		  [peek-sk (car peek-t)])
	     (when (and (not (eq? 'close-delim
				  peek-sk))
			(not (eq? 'eof
				  peek-sk)))
	       (parse-datum p)
	       (loop))))
	 (expect-close-delimiter p (cond
				     [(string=? "(" (cdr peek-t)) ")"]
				     [(string=? "[" (cdr peek-t)) "]"]))
	 (finish-node p)]))))

(define start-node
  (lambda (p syntax-kind)
    (conifer-start-node (parser-builder p)
			syntax-kind)))

(define finish-node
  (lambda (p)
    (conifer-finish-node (parser-builder p))))

(define bump
  (lambda (p)
    (let ([tok (next p)])
      (conifer-push-token
	(parser-builder p)
	(car tok)
	(cdr tok)))))

(define bump-raw
  (lambda (p)
    (let ([tok (next-raw p)])
      (conifer-push-token
	(parser-builder p)
	(car tok)
	(cdr tok)))))

(define push-delimiter
  (lambda (p d)
    (cond
      [(string=? "(" d)
       (parser-delimiter-stack-set!
	 p
	 (cons 'lparen
	       (parser-delimiter-stack p)))
       (bump p)]
      [(string=? "[" d)
       (parser-delimiter-stack-set!
	 p
	 (cons 'lbracket
	       (parser-delimiter-stack p)))
       (bump p)]
      [else (violation-assertion
	      'push-delimiter
	      "tried to push an invalid delimiter: ~a"
	      d)])))

(define expect-close-delimiter
  (lambda (p cd)
    (let ([peek-t (peek p)])
      (if (eq? 'close-delim (car peek-t))
	; TODO: Improve error recovery
	(begin
	  (pop-delimiter p (cdr peek-t))
	  (bump p))
	(emit-error p "expecting a ~A" cd)))))

(define pop-delimiter
  (lambda (p d)
    (if (null? (parser-delimiter-stack p))
      (emit-error
	p
	"closing delimiter ~S was never opened")
      (let ([top-delim (car (parser-delimiter-stack p))])
	(cond
	  [(or (and (string=? ")" d)
		    (eq? top-delim 'lparen))
	       (and (string=? "]" d)
		    (eq? top-delim 'lbracket)))
	   (parser-delimiter-stack-set!
	     p
	     (cdr (parser-delimiter-stack p)))])))))

(define emit-error
  (lambda (p msg . fargs)
    (parser-errors-set!
      p
      (cons (cons (cons (parser-offset p)
			(+ (parser-offset p)
			   (string-length (cdr (peek p)))))
		  (cons msg fargs))
	    (parser-errors p)))))

(define at-eof?
  (lambda (p)
    (eq? (car (peek-raw p))
	 'eof)))

(define next
  (lambda (p)
    (eat-trivia p)
    (next-raw p)))

(define peek
  (lambda (p)
    (eat-trivia p)
    (peek-raw p)))

(define next-raw
  (lambda (p)
    (cond
      [(null? (parser-tokens p)) '(eof . "")]
      [else (let ([t (car (parser-tokens p))])
	      (parser-tokens-set! p
				  (cdr (parser-tokens p)))
	      (parser-offset-set!
		p
		(+ 1 (parser-offset p)))
	      t)])))

(define peek-raw
  (lambda (p)
    (if (null? (parser-tokens p))
      '(eof . "")
      (car (parser-tokens p)))))

(define eat-trivia
  (lambda (p)
    (define at-trivia?
      (lambda ()
	(case (car (peek-raw p))
	  [(whitespace) #t]
	  [else #f])))

    (let loop ()
      (when (at-trivia?)
	(bump-raw p)
	(loop)))))
