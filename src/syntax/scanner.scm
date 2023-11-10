#|

# Scanner
Transforms an input string into a list of tokens in the form

```text
List[Pair[Kind, Lexeme]]
where:
        Kind = symbol
        Lexeme = string
```

The scanner is based on R6RS's formal specification, however it is designed
to be extremely forgiving and will produce valid tokens in places where an
error should be specified. The parser is responsible for detecting such
errors and emitting the corresponding error message.

The following is a (hopefully) extensive list of all the erroneous tokens that can be produced:

* Tokens that have characters are parsed case-insensitive (e.g. #vu8())
* Most tokens consume everything until a delimiter (e.g. characters (#\xlwl), identifiers (+a, -b), numbers (3a)).
|#

(library
  (syntax scanner)
  (export scan-string)
  (import (rnrs base)
	  (only (rnrs lists)
		cons*)
	  (only (rnrs unicode)
		char-numeric?
		char-whitespace?
		char-alphabetic?)
	  (common))

  ;; Transforms a string to a list of tokens in the form
  ;; List[Pair[TokenKind, Lexeme]]
  ;; 	where:
  ;;		TokenKind = Symbol
  ;;		Lexeme = String
  (define scan-string
    (lambda (str)
      (let loop ([chars (string->list str)]
		 [acc '()])
	(if (null? chars)
	  (reverse acc)
	  (let ([c (car chars)]
		[rest (cdr chars)]
		[peek-c (peek (cdr chars))])
	    (cond
	      ; Try to parse characters that form tokens by themselves.
	      [(char=? #\( c) (loop rest (cons (cons 'open-delim "(") acc))]
	      [(char=? #\) c) (loop rest (cons (cons 'close-delim ")") acc))]
	      [(char=? #\[ c) (loop rest (cons (cons 'open-delim "[") acc))]
	      [(char=? #\] c) (loop rest (cons (cons 'close-delim "]") acc))]
	      [(char=? #\' c) (loop rest (cons (cons 'quote "'") acc))]
	      [(char=? #\` c) (loop rest (cons (cons 'backtick "`") acc))]
	      [(char=? #\. c)
	       (if (char-ident? peek-c)
		 (let-values ([(ident rest) (take-while chars char-ident?)])
		   (loop rest (cons (cons 'identifier (list->string ident))
				    acc)))
		 (loop rest (cons (cons 'dot ".") acc)))]

	      [(char=? #\, c)
	       (if (eqv? #\@ peek-c)
		 (loop (cdr rest) (cons (cons 'comma-at ",@") acc))
		 (loop rest (cons (cons 'comma ",") acc)))]

	      ; Try to parse a token starting with #
	      [(char=? #\# c)
	       (cond
		 [(eqv? #\( peek-c) (loop (cdr rest)
					  (cons (cons 'open-vector "#(")
						acc))]
		 [(eqv? #\' peek-c) (loop (cdr rest)
					  (cons (cons 'hash-quote "#'")
						acc))]
		 [(eqv? #\` peek-c) (loop (cdr rest)
					  (cons (cons 'hash-backtick "#`")
						acc))]
		 [(eqv? #\, peek-c)
		  (if (eqv? #\@ (peek-nth rest 1))
		    (loop (cddr rest)
			  (cons (cons 'hash-comma-at "#,@")
				acc))
		    (loop (cdr rest)
			  (cons (cons 'hash-comma "#,")
				acc)))]
		 ; Allow case insensitive #vu8(), an error **must** be emitted
		 ; later by the parser.
		 [(and (or (eqv? #\v (peek-nth rest 0))
			   (eqv? #\V (peek-nth rest 0)))
		       (or (eqv? #\u (peek-nth rest 1))
			   (eqv? #\U (peek-nth rest 1)))
		       (eqv? #\8 (peek-nth rest 2))
		       (eqv? #\( (peek-nth rest 3)))
		  (loop (skip chars 5)
			(cons (cons 'open-bytevector
				    (list->string
				      (take chars 5)))
			      acc))]
		 [(or (eqv? #\t peek-c)
		      (eqv? #\T peek-c))
		  (loop (cdr rest) (cons (cons 'true (string c peek-c))
					 acc))]
		 [(or (eqv? #\f peek-c)
		      (eqv? #\F peek-c))
		  (loop (cdr rest) (cons (cons 'false (string c peek-c))
					 acc))]
		 [(eqv? #\\ peek-c)
		  ; Take everything until we hit a delimiter, the parser
		  ; should generate the error for invalid characters.
		  (let-values ([(char rest) (take-til
					      ; skip the #, it is a delimiter
					      (cdr chars)
					      char-delimiter?)])
		    (loop rest
			  (cons (cons 'char
				      (list->string
					; push the skipped # back
					(cons #\# char)))
				acc)))]
		 [(not (char-delimiter? peek-c))
		  ; Take everything until we hit a delimiter, the parser
		  ; should generate the error for the invalid token.
		  (let-values ([(char rest) (take-til
					      ; skip the #, it is a delimiter
					      (cdr chars)
					      char-delimiter?)])
		    (loop rest
			  (cons (cons 'identifier
				      (list->string
					; push the skipped # back
					(cons #\# char)))
				acc)))]
		 [else (loop rest (cons (cons 'error "#") acc))])]
	      ; end #

	      [(char-dec-digit? c)
	       (let-values([(number rest) (take-while
					    chars
					    char-dec-digit?)])
		 (loop rest
		       (cons (cons 'int-number (list->string number))
			     acc)))]

	      [(char-ident? c)
	       (let-values ([(ident rest) (take-while
					    chars
					    char-ident?)])
		 (loop rest
		       (cons (cons 'identifier
				   (list->string ident))
			     acc)))]

	      [(char-whitespace? c)
	       (let-values ([(ws rest) (take-while
					 chars
					 char-whitespace?)])
		 (loop rest
		       (cons (cons 'whitespace (list->string ws))
			     acc)))]

	      [(char=? #\" c)
	       (let-values ([(str rest) (eat-string chars)])
		 (loop rest
		       (cons (cons 'string (list->string str))
			     acc)))]

	      [else (loop rest
			  (cons (cons 'error (string c))
				acc))]))))))

  ;; Returns `true` if `c` counts as a delimiter character.
  (define char-delimiter?
    (lambda (c)
      (and (char? c)
	   (case c
	     [(#\( #\) #\[ #\] #\{ #\} #\" #\; #\#) #t]
	     [else (char-whitespace? c)]))))

  ;; Returns `true` if `c` is an ascii-digit (0-9).
  (define char-dec-digit?
    (lambda (c)
      (and (char? c)
	   (char>=? c #\0)
	   (char<=? c #\9))))

  ;; Returns `true` if `c` is an ascii-digit (0-9).
  (define char-ident?
    (lambda (c)
      (and (char? c)
	   (or (char-alphabetic? c)
	       (char-numeric? c)
	       (case c
		 [(#\! #\$ #\% #\& #\* #\/ #\: #\< #\= #\> #\? #\^ #\_ #\~ #\+ #\- #\. #\@) #t]
		 [else #f])))))

  ;; Returns the next character in the char-list, or `#f` if none.
  (define peek
    (lambda (chars)
      (and (not (null? chars))
	   (car chars))))

  ;; Returns the next character in the char-list, or `#f` if none.
  (define peek-nth
    (lambda (chars n)
      (if (null? chars)
	#f
	(if (= n 0)
	  (car chars)
	  (peek-nth (cdr chars) (- n 1))))))

  ;; Returns the list after skipping the first `n` characters.
  ;;
  ;; # Exceptions
  ;; * If there aren't enough characters in the list, an exception is thrown.
  (define skip
    (lambda (chars n)
      (if (or (= n 0)
	      (null? chars))
	chars
	(skip (cdr chars) (- n 1)))))

  ;; Returns the first `n` characters of `chars` as a list.
  ;;
  ;; # Exceptions
  ;; * If there aren't enough characters in the list, an exception is thrown.
  (define take
    (lambda (chars n)
      (let loop ([chars chars]
		 [n n]
		 [acc '()])
	(if (= n 0)
	  (reverse acc)
	  (loop (cdr chars)
		(- n 1)
		(cons (car chars)
		      acc))))))

  ;; Separates the character list into two, the second list starts with the
  ;; first character that satisfies `pred`.
  (define take-til
    (lambda (chars pred)
      (let loop ([lst chars]
		 [acc '()])
	(if (or (null? lst)
		(pred (car lst)))
	  (values (reverse acc)
		  lst)
	  (loop (cdr lst)
		(cons (car lst)
		      acc))))))

  ;; Separates the character list into two, the second list starts with the
  ;; first character that doesn't satisfy `pred`.
  (define take-while
    (lambda (chars pred)
      (let loop ([lst chars]
		 [acc '()])
	(if (or (null? lst)
		(not (pred (car lst))))
	  (values (reverse acc)
		  lst)
	  (loop (cdr lst)
		(cons (car lst)
		      acc))))))

  ;; Consumes a string from chars. The first value is the consumed string,
  ;; the second is the rest of the string.
  (define eat-string
    (lambda (chars)
      (let loop ([chars (cdr chars)]
		 [acc (list (car chars))])
	(cond
	  [(null? chars)
	   (values (reverse acc)
		   chars)]
	  [(eqv? #\" (peek chars))
	   (values (reverse (cons (car chars)
				  acc))
		   (cdr chars))]
	  [(and (eqv? #\\ (peek chars))
		(peek-nth chars 1))
	   (loop (skip chars 2)
		 (cons* (cadr chars)
			(car chars)
			acc))]
	  [else (loop (cdr chars)
		      (cons (car chars)
			    acc))])))))
