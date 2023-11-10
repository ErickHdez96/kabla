(library
  (env)
  (export make-root-env)
  (import (rnrs base)
	  (rnrs records syntactic)
	  (rnrs hashtables))

  (define-record-type
    env
    (fields
      parent
      bindings))

  (define make-root-env
    (lambda ()
      (make-env #f (make-hashtable equal? string=?)))))
