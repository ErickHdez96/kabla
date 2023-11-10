(library
  (syntax parser)
  (export parse-tokens
	  parse-char)
  (import (rnrs base)
	  (only (rnrs control)
		when)
	  (only (rnrs records syntactic)
		define-record-type)
	  (only (rnrs unicode)
		string-ci=?
		char-downcase)
	  (only (srfi srfi-13)
		string-prefix?)
	  (only (ice-9 format)
		format)
	  (only (conifer)
		conifer-green-node-builder
		conifer-start-node
		conifer-finish-node
		conifer-finish-builder
		conifer-push-token))

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
	    (parse-datum p)
	    (loop)))

	(finish-node p)
	(let ([t (conifer-finish-builder (parser-builder p))])
	  (cons t
		(reverse (parser-errors p)))))))

  (define parse-datum
    (lambda (p)
      (let* ([peek-t (peek p)]
	     [peek-sk (car peek-t)]
	     [start (parser-offset p)])
	(validate-token p peek-t)
	(cond
	  ; parses a single atom
	  ; <boolean> | <number> | <character> | <string> | <identifier>
	  [(case peek-sk
	     [(char true false int-number identifier string) #t]
	     [else #f])
	   (start-node p 'atom)
	   (bump p)
	   (finish-node p)
	   (cons start (parser-offset p))]

	  ; parses a list starting with an open delimiter
	  ; (<datum*>) | [<datum>*] | (<datum>+ . <datum>) | [<datum>+ . <datum>]
	  [(eq? 'open-delim peek-sk)
	   (start-node p 'list)
	   (push-delimiter p (cdr peek-t))
	   (let loop ([parsed-expr #f]
		      [parsed-dot #f])
	     (let* ([peek-t (peek p)]
		    [peek-sk (car peek-t)])
	       (cond
		 [(or (eq? 'close-delim peek-sk)
		      (eq? 'eof peek-sk))
		  ; Simply do nothing
		  #f]
		 [(eq? 'dot peek-sk)
		  (when parsed-dot
		    (emit-error p "multiple dots '.' not allowed inside list"))
		  (when (not parsed-expr)
		    (emit-error p "expected at least one expression before dot '.'"))
		  (bump p)
		  (loop parsed-expr #t)]
		 [else (parse-datum p)
		       (loop #t parsed-dot)])))
	   (expect-close-delimiter p (cond
				       [(string=? "(" (cdr peek-t)) ")"]
				       [(string=? "[" (cdr peek-t)) "]"]))
	   (finish-node p)
	   (cons start (parser-offset p))]

	  [(token-abbrev? peek-t)
	   (start-node p 'abbreviation)
	   (bump p)
	   (parse-datum p)
	   (finish-node p)
	   (cons start (parser-offset p))]

	  [(eq? 'open-vector peek-sk)
	   (start-node p 'vector)
	   (push-delimiter p (cdr peek-t))

	   (let loop ()
	     (let* ([peek-t (peek p)]
		    [peek-sk (car peek-t)])
	       (cond
		 [(or (eq? 'close-delim peek-sk)
		      (eq? 'eof peek-sk))
		  ; Simply do nothing
		  #f]
		 [(eq? 'dot peek-sk)
		  (emit-error-and-bump p "dot '.' not allowed inside vector")
		  (loop)]
		 [else (parse-datum p)
		       (loop)])))

	   (expect-close-delimiter p ")")
	   (finish-node p)
	   (cons start (parser-offset p))]

	  [(eq? 'open-bytevector peek-sk)
	   (start-node p 'bytevector)
	   (push-delimiter p (cdr peek-t))

	   (let loop ()
	     (let* ([peek-t (peek p)]
		    [peek-sk (car peek-t)])
	       (cond
		 [(or (eq? 'close-delim peek-sk)
		      (eq? 'eof peek-sk))
		  ; Simply do nothing
		  #f]
		 [(eq? 'dot peek-sk)
		  (emit-error-and-bump p "dot '.' not allowed inside bytevector")
		  (loop)]
		 [(eq? 'int-number peek-sk)
		  ; TODO: Check it is within 0-255
		  (bump p)
		  (loop)]
		 [(parse-datum p) => (lambda (span)
				       (emit-error-span p span "only integers in the range [0..255] allowed inside bytevectors")
				       (loop))]
		 [else (loop)])))

	   (expect-close-delimiter p ")")
	   (finish-node p)
	   (cons start (parser-offset p))]

	  [else (emit-error-and-bump
		  p
		  "expected an opening delimiter or an atom")
		#f]))))

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
	[(string=? "#(" d)
	 (parser-delimiter-stack-set!
	   p
	   (cons 'vector
		 (parser-delimiter-stack p)))
	 (bump p)]
	[(string-ci=? "#vu8(" d)
	 (parser-delimiter-stack-set!
	   p
	   (cons 'bytevector
		 (parser-delimiter-stack p)))
	 (bump p)]
	[else (assertion-violation
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
		      (or (eq? top-delim 'lparen)
			  (eq? top-delim 'vector)
			  (eq? top-delim 'bytevector)))
		 (and (string=? "]" d)
		      (eq? top-delim 'lbracket)))
	     (parser-delimiter-stack-set!
	       p
	       (cdr (parser-delimiter-stack p)))])))))

  (define emit-error-span
    (lambda (p span msg . fargs)
      (parser-errors-set!
	p
	(cons (cons span
		    (cons msg fargs))
	      (parser-errors p)))))

  (define emit-error
    (lambda (p msg . fargs)
      (emit-error-span
	p
	(cons (parser-offset p)
	      (+ (parser-offset p)
		 (string-length (cdr (peek p)))))
	msg)))

  (define emit-error-and-bump
    (lambda (p msg . fargs)
      (apply emit-error p msg fargs)
      (bump p)))

  (define validate-token
    (lambda (p t)
      (let ([sk (car t)]
	    [text (cdr t)])
	(case sk
	  [(open-bytevector)
	   (when (not (string=? "#vu8(" text))
	     (emit-error p "#vu8( must be lowercase"))]
	  [(identifier)
	   (when (string-prefix? "#" text)
	     (emit-error p "identifiers cannot begin with '#'"))
	   ]
	  [(char)
	   (let ([e (parse-char text)])
	     (when (string? e)
	       (emit-error p e)))]
	  [(string)
	   ; TODO: Improve string validation
	   (when (or (= 1 (string-length text))
		     (not (char=? #\"
				  (string-ref text
					      (- (string-length text)
						 1))))
		     (and (char=? #\"
				  (string-ref text
					      (- (string-length text)
						 1)))
			  (char=? #\\
				  (string-ref text
					      (- (string-length text)
						 2)))))
	     (emit-error p "unterminated string"))]))))

  (define at-eof?
    (lambda (p)
      (eq? (car (peek p))
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
		  (+ (string-length (cdr t)) (parser-offset p)))
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

  (define token-abbrev?
    (lambda (t)
      (case (car t)
	[(quote backtick comma comma-at hash-quote hash-backtick hash-comma hash-comma-at) #t]
	[else #f])))

  ;; Parses string `s` into a character or returns a string with an
  ;; error message.
  (define parse-char
    (lambda (s)
      ; scanner must only have returned a character token if it begins
      ; with #\
      (assert (string-prefix? "#\\" s))
      (cond
	[(= 2 (string-length s)) "expecting a character"]
	[(= 3 (string-length s))
	 ; if there are only 3 characters in the string,
	 ; the third one is simply the character
	 (string-ref s 2)]
	[(char=? #\X (string-ref s 2))
	 "invalid hex character, try #\\x"]
	[(char=? #\x (string-ref s 2))
	 (let ([hex (parse-hex-scalar-value
		      (substring s 3))])
	   (cond
	     [(string? hex) hex]
	     [(or (<= 0 hex #xD7FF)
		  (<= #xE000 hex #x10FFFF))
	      (integer->char hex)]
	     [else
	       "hex scalar value must be in range [#x0, #xD7FF] ∪ [#xE000, #x10FFFF]"]))]
	[else (let ([char-name (substring s 2)])
		(cond
		  [(string=? "nul" char-name) #\nul]
		  [(string=? "alarm" char-name) #\alarm]
		  [(string=? "backspace" char-name) #\backspace]
		  [(string=? "tab" char-name) #\tab]
		  [(string=? "linefeed" char-name) #\linefeed]
		  [(string=? "newline" char-name) #\newline]
		  [(string=? "vtab" char-name) #\vtab]
		  [(string=? "page" char-name) #\page]
		  [(string=? "return" char-name) #\return]
		  [(string=? "esc" char-name) #\esc]
		  [(string=? "space" char-name) #\space]
		  [(string=? "delete" char-name) #\delete]
		  [else (format
			  #f
			  "invalid character name: ~a"
			  char-name)]))])))

  ;; Parses a hex number and returns an integer or a string with
  ;; an error message.
  (define parse-hex-scalar-value
    (lambda (s)
      (if (zero? (string-length s))
	#f
	(let loop ([n 0]
		   [parsed 0])
	  (if (= n (string-length s))
	    parsed
	    (let ([c (char-downcase (string-ref s n))])
	      (cond
		[(and (char>=? c #\0)
		      (char<=? c #\9))
		 (loop (+ n 1)
		       (+ (* parsed 16)
			  (- (char->integer c)
			     (char->integer #\0))))]
		[(and (char>=? c #\a)
		      (char<=? c #\f))
		 (loop (+ n 1)
		       (+ (* parsed 16)
			  (- (char->integer c)
			     (char->integer #\a))
			  10))]
		[else "invalid hex scalar value"]))))))))
