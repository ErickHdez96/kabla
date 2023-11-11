(library
  (env)
  (export make-root-env
	  make-child-env
	  env-lookup)
  (import (rnrs base)
	  (only (rnrs control)
		when)
	  (only (rnrs records syntactic)
		define-record-type)
	  (only (rnrs hashtables)
		make-hashtable
		hashtable-ref
		equal-hash))

  (define-record-type
    env
    (fields
      parent
      bindings))

  ;; Creates a new root environment, with no parent.
  (define make-root-env
    (lambda ()
      (make-env #f (make-hashtable equal-hash string=?))))

  ;; Creates a new environment with `p` as its enclosing environment.
  (define make-child-env
    (lambda (p)
      (when (not (env? p))
	(assertion-violation
	  'make-child-env
	  "parent must be an environment: ~a"
	  p))
      (make-env p (make-hashtable equal-hash string=?))))

  (define env-lookup
    (lambda (e k)
      (cond
	[(hashtable-ref (env-bindings e)
			k
			#f)
	 => (lambda (res) res)]
	[(env-parent e)
	 (env-lookup (env-parent e)
		     k)]
	[else #f]))))
