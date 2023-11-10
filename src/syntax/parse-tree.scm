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
	  pt-sexp?
	  pt-atom?
	  pt-boolean?
	  pt-boolean-value
	  pt-char?
	  pt-list?
	  pt-vector?
	  pt-bytevector?
	  pt-abbreviation?)
  (import (rnrs base)
	  (only (rnrs lists)
		filter)
	  (only (conifer)
		conifer-red-tree?
		conifer-syntax-kind
		conifer-red-children
		conifer-token-text)
	  (only (syntax parser)
		parse-char))

  ;; Returns the s-exps of the `root` red-tree.
  (define pt-root-sexps
    (lambda (root)
      (cond
	[(and (conifer-red-tree? root)
	      (eq? 'root (conifer-syntax-kind root)))
	 (filter pt-sexp?
		 (conifer-red-children root))]
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
	   (car (conifer-red-children node)))))

  ;; Returns `node` as-is if it is a boolean, `#f` otherwise.
  (define pt-boolean?
    (lambda (node)
      (and (or (eq? 'true
		    (conifer-syntax-kind node))
	       (eq? 'false
		    (conifer-syntax-kind node)))
	   node)))

  (define pt-boolean-value
    (lambda (atom)
      (cond
	[(eq? 'true (conifer-syntax-kind atom)) #t]
	[(eq? 'false (conifer-syntax-kind atom)) #f]
	[else (assertion-violation
		'pt-boolean-value
		"expected a boolean red tree, found: ~a"
		atom)])))

  ;; Returns the internal node if `node` is an atom, `#f` otherwise.
  (define pt-char?
    (lambda (node)
      (and (eq? 'char (conifer-syntax-kind node))
	   (let ([c (parse-char (conifer-token-text node))])
	     (if (char? c)
	       c
	       #\xFFFD)))))

  ;; Returns `node` as-is if it is a list, `#f` otherwise.
  (define pt-list?
    (lambda (node)
      (and (eq? 'list
		(conifer-syntax-kind node))
	   node)))

  ;; Returns `node` as-is if it is a vector, `#f` otherwise.
  (define pt-vector?
    (lambda (node)
      (and (eq? 'vector
		(conifer-syntax-kind node))
	   node)))

  ;; Returns `node` as-is if it is a bytevector, `#f` otherwise.
  (define pt-bytevector?
    (lambda (node)
      (and (eq? 'bytevector
		(conifer-syntax-kind node))
	   node)))

  ;; Returns `node` as-is if it is an abbreviation, `#f` otherwise.
  (define pt-abbreviation?
    (lambda (node)
      (and (eq? 'abbreviation
		(conifer-syntax-kind node))
	   node))))
