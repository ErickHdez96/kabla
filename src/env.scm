(library
  (env)
  (export make-root-env
	  make-child-env
	  env-lookup
	  env-insert!)
  (import (rnrs base)
	  (only (rnrs control)
		when)
	  (only (rnrs records syntactic)
		define-record-type)
	  (only (rnrs hashtables)
		make-eq-hashtable
		hashtable-ref
		hashtable-set!
		equal-hash)
	  (common))

  (define-record-type
    env
    (fields
      parent
      bindings))

  ;; Creates a new root environment, with no parent.
  (define make-root-env
    (lambda ()
      (make-env #f (make-eq-hashtable))))

  ;; Creates a new environment with `p` as its enclosing environment.
  (define make-child-env
    (lambda (p)
      (when (not (env? p))
	(assertion-violation
	  'make-child-env
	  "parent must be an environment: ~a"
	  p))
      (make-env p (make-eq-hashtable))))

  ;; Looks up `k` in the environment `e`. If it is not present in `e`, `k` is
  ;; looked up recursively in its parents.
  (define env-lookup
    (lambda (e k)
      (or (hashtable-ref (env-bindings e)
			 k
			 #f)
	  (and (env-parent e)
	       (env-lookup (env-parent e)
			   k)))))

  ;; Inserts a mapping from `k` to `v` in the environment `e`.
  (define env-insert!
    (lambda (e k v)
      (hashtable-set! (env-bindings e)
		      k
		      v))))
