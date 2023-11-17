;;; Parse tree module.
;;;
;;; A parse tree comprises of simple atoms and lists. It has no knowledge about
;;; about its contents, such as defines, exprs, macros, etc. It serves as an
;;; input to the expander, which transforms a parse tree into an abstract syntax
;;; tree, after resolving and expanding macros (e.g. define, lambda, if,
;;; define-macro, macro usages, etc.)
(library
  (syntax parse-tree)
  (export pt-root-sexps
	  pt-abbreviation?
	  pt-sexp?
	  pt-atom?
	  pt-boolean?
	  pt-boolean-value
	  pt-char?
	  pt-string?
	  pt-identifier?
	  pt-list?
	  pt-dot?
	  pt-vector?
	  pt-bytevector?
	  pt-abbreviation?
	  pt-open-delim?
	  pt-close-delim?
	  pt-span
	  pt-offset
	  (rename (conifer-syntax-kind pt-syntax-kind)))
  (import (rnrs base)
	  (only (rnrs lists)
		filter)
	  (only (conifer)
		conifer-red-tree?
		conifer-syntax-kind
		conifer-red-offset
		conifer-red-children
		conifer-text-length
		conifer-token-text)
	  (only (syntax parser)
		parse-char
		parse-string)
	  (common))

  ;; Returns the s-exps of the `root` red-tree.
  (define pt-root-sexps
    (lambda (root)
      (cond
	[(and (conifer-red-tree? root)
	      (eq? 'root (conifer-syntax-kind root)))
	 (filter pt-sexp?
		 (vector->list (conifer-red-children root)))]
	[else #f])))

  ;; Returns `node` as-is if it is an expression, `#f` otherwise.
  (define pt-sexp?
    (lambda (node)
      (case (conifer-syntax-kind node)
	[(atom list vector bytevector abbreviation)
	 node]
	[else #f])))

  ;; Returns the internal node if `node` is an atom, `#f` otherwise.
  (define pt-atom?
    (lambda (node)
      (and (eq? 'atom
		(conifer-syntax-kind node))
	   (vector-ref (conifer-red-children node) 0))))

  ;; Returns `node` as-is if it is a boolean, `#f` otherwise.
  (define pt-boolean?
    (lambda (node)
      (and (or (eq? 'true
		    (conifer-syntax-kind node))
	       (eq? 'false
		    (conifer-syntax-kind node)))
	   node)))

  ;; Returns the internal value for `boolean`.
  ;;
  ;; # Exceptions
  ;;
  ;; Throws an excpetion if `boolean`is not a `'true` or `'false`.
  (define pt-boolean-value
    (lambda (boolean)
      (cond
	[(eq? 'true (conifer-syntax-kind boolean)) #t]
	[(eq? 'false (conifer-syntax-kind boolean)) #f]
	[else (assertion-violation
		'pt-boolean-value
		"expected a boolean red tree, found: ~a"
		boolean)])))

  ;; Returns the inner character if `node` is a `'char`, `#f` otherwise.
  (define pt-char?
    (lambda (node)
      (and (eq? 'char (conifer-syntax-kind node))
	   (let ([c (parse-char (conifer-token-text node))])
	     (if (char? c)
	       c
	       ; replacement character �
	       #\xFFFD)))))

  ;; Returns the inner string if `node` is a `'string`, `#f` otherwise.
  (define pt-string?
    (lambda (node)
      (and (eq? 'string (conifer-syntax-kind node))
	   (let ([c (parse-string (conifer-token-text node))])
	     (if (string? c)
	       c
	       "")))))

  ;; Returns the inner text as a symbol if `node` is a variable, `#f` otherwise.
  (define pt-identifier?
    (lambda (node)
      (and (eq? 'identifier (conifer-syntax-kind node))
	   (string->symbol (conifer-token-text node)))))

  ;; Returns `#f` if `node` is not a list. Otherwise returns a pair, whose car
  ;; is a list of all the s-exps before the first `.` and whose car is a list
  ;; of all the s-exps after the first `.`.
  (define pt-list?
    (lambda (node)
      (and (eq? 'list
		(conifer-syntax-kind node))
	   (let collect-before ([elems (vector->list (conifer-red-children node))]
				[acc '()])
	     (cond
	       [(null? elems)
		(cons (reverse acc) '())]
	       [(pt-dot? (car elems))
		(cons (reverse acc)
		      (filter
			(lambda (x) x)
			(map
			  pt-sexp?
			  (cdr elems))))]
	       [(pt-sexp? (car elems))
		=> (lambda (sexp)
		     (collect-before (cdr elems)
				     (cons sexp acc)))]
	       [else (collect-before (cdr elems)
				     acc)])))))

  ;; Returns `node` as-is if it is a `.`, `#f` otherwise.
  (define pt-dot?
    (lambda (node)
      (and (eq? 'dot
		(conifer-syntax-kind node))
	   node)))

  ;; Returns a pair of `abbreviation` and sexp
  (define pt-abbreviation?
    (lambda (node)
      (if (eq? 'abbreviation (conifer-syntax-kind node))
	(let ([elems (conifer-red-children node)])
	  (assert (>= (vector-length elems) 1))
	  (let ([abbrev (pt-abbrev? (vector-ref elems 0))])
	    (assert abbrev)
	    (cons abbrev
		  (and (>= (vector-length elems) 2)
		       (pt-sexp? (vector-ref elems 1))))))
	#f)))

  ;; Returns a pair of `abbreviation` and sexp
  (define pt-abbrev?
    (lambda (node)
      (case (conifer-syntax-kind node)
	[(quote backtick comma comma-at hash-quote hash-backtick hash-comma hash-comma-at)
	 node]
	[else #f])))

  ;; Returns `node` as-is if it is a vector, `#f` otherwise.
  (define pt-vector?
    (lambda (node)
      (and (eq? 'vector
		(conifer-syntax-kind node))
	   node)))

  ;; Returns `#f` if `node` is not a vector. Otherwise returns a list of all
  ;; s-exps.
  (define pt-vector?
    (lambda (node)
      (and (eq? 'vector
		(conifer-syntax-kind node))
	   (filter
	     (lambda (x) x)
	     (map
	       pt-sexp?
	       (vector->list (conifer-red-children node)))))))

  ;; Returns `node` as-is if it is a bytevector, `#f` otherwise.
  (define pt-bytevector?
    (lambda (node)
      (and (eq? 'bytevector
		(conifer-syntax-kind node))
	   node)))

  ;; Returns `node` as-is if it is a open delimiter, `#f` otherwise.
  (define pt-open-delim?
    (lambda (node)
      (and (eq? 'open-delim
		(conifer-syntax-kind node))
	   node)))

  ;; Returns `node` as-is if it is a close delimiter, `#f` otherwise.
  (define pt-close-delim?
    (lambda (node)
      (and (eq? 'close-delim
		(conifer-syntax-kind node))
	   node)))

  ;; Returns the offset of `node`.
  (define pt-offset conifer-red-offset)

  ;; Returns the span of `node`. (<offset> . <length>)
  (define pt-span
    (lambda (node)
      (cons (conifer-red-offset node)
	    (conifer-text-length node)))))
