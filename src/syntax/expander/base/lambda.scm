(library
  (syntax expander base lambda)
  (export keyword-lambda)
  (import (rnrs base)
          (only (rnrs control)
                when)
          (only (rnrs lists)
                find
                filter)
          (only (srfi srfi-28)
                format)
          (only (conifer)
                conifer-tree->string)
          (only (syntax ast)
		ast-span
                ast-define-offset
                ast-define-variable
                ast-define-expr
                ast-define?
                ast-expr?
                make-ast-identifier
                make-ast-lambda
                make-ast-let)
          (only (syntax parse-tree)
                pt-atom?
                pt-identifier?
                pt-list?
                pt-span
		pt-offset)
          (only (syntax expander)
                expand-datum
                expand-emit-error
		expand-enter-scope
		expand-exit-scope
                expand-deferred-items
                expand-take-items!)
          (only (syntax expander base common)
                maybe-unexpected-dot
                close-delim-or-dot-span
                expected-closing-delim)
          (common))

  ;; Expands a `lambda` expression
  ;; ( lambda <formals> <body> )
  (define keyword-lambda
    (lambda (e node elems dot)
      (define last-span (close-delim-or-dot-span node))
      (define elems-length (length elems))

      (cond
	[(< elems-length 2)
	 (expand-emit-error
	   e
	   last-span
	   "expected lambda formals")]
	[(< elems-length 3)
	 (parse-formals e (cadr elems))
	 (expand-emit-error
	   e
	   last-span
	   "expected at least one expression")]
	[else
	  (let ([formals (parse-formals e (cadr elems))])
	    (expand-enter-scope e)

            (for-each
              (lambda (d) (expand-datum e d 'body))
              (cddr elems))
            (expand-deferred-items e)

            (let* ([items (expand-take-items! e)]
                   [defs (map
                           (lambda (d)
                             (cons (ast-define-variable d)
                                   (ast-define-expr d)))
                           (filter ast-define? items))]
                   [exprs (filter ast-expr? items)])
              (let loop ([items items]
                         [seen-expr #f])
                (when (not (null? items))
                  (cond
                    [(and (ast-define? (car items))
                          seen-expr)
                     (expand-emit-error
                       e
                       (ast-span (car items))
                       "definitions are not allowed after the first expression")
                     (loop (cdr items)
                           seen-expr)]
                    [else
                      (loop (cdr items)
                            (or seen-expr
                                (ast-expr? (car items))))])))

              (when (null? exprs)
                (expand-emit-error
                  e
                  (pt-span node)
                  "expected at least one expression"))
              (make-ast-lambda
                (pt-offset node)
		node
                (car formals)
                (cdr formals)
                (make-ast-let
                  (pt-offset node)
		  node
                  'letrec*
                  defs
                  exprs))))])))

  ;; <formals> → ( <formal>+ )
  ;;	   | <formal>
  ;;	   | ( <formal>+ . <formal> )
  (define parse-formals
    (lambda (e node)
      (cond
        [(and-then (pt-atom? node)
                   pt-identifier?)
         => (lambda (formal)
              (cons '() (make-ast-identifier
                          (pt-offset node)
			  node
                          formal)))]
        [(pt-list? node)
         => (lambda (f-elems)
              (let loop ([before-dot (car f-elems)]
                         [formals '()])
                (cond
                  [(null? before-dot)
                   (cond
                     [(null? (cdr f-elems))
                      (cons (reverse formals)
                            #f)]
                     [else
                       (let ([rest (cond
                                     [(and-then (pt-atom? (cadr f-elems))
                                                pt-identifier?)
                                      => (lambda (rest)
                                           (make-ast-identifier
                                             (pt-offset (cadr f-elems))
					     (cadr f-elems)
                                             rest))]
                                     [else (expand-emit-error
                                             e
                                             (pt-span (cadr f-elems))
                                             (format
                                               "expected an identifier, found ~a"
                                               (conifer-tree->string (cadr f-elems))))
                                           (make-ast-identifier
                                             (pt-offset (cadr f-elems))
					     (cadr f-elems)
                                             (string->symbol
                                               (format
                                                 "|~a|"
                                                 (conifer-tree->string (cadr f-elems)))))])])
                         (when (>= (length (cdr f-elems)) 2)
                           (expand-emit-error
                             e
                             (pt-span (caddr f-elems))
                             (format
                               "expected ~a, found ~a"
                               (expected-closing-delim node)
                               (conifer-tree->string (caddr f-elems)))))
                         (cons (reverse formals)
                               rest))])]
                  [(and-then (pt-atom? (car before-dot))
                             pt-identifier?)
                   => (lambda (var)
                        (loop (cdr before-dot)
                              (cons (make-ast-identifier
                                      (pt-offset (car before-dot))
				      (car before-dot)
                                      var)
                                    formals)))]
                  [else (expand-emit-error
                          e
                          (pt-span (car before-dot))
                          (format "expected an identifier, found ~a"
                                  (conifer-tree->string (car before-dot))))
                        (loop (cdr before-dot)
                              formals)])))]
        [else
          (expand-emit-error
            e
            (pt-span node)
            (format "expected an identifier or an open delimiter, found ~a"
                    (conifer-tree->string node)))
          (cons '() (make-ast-identifier
                      (pt-offset node)
		      node
                      (string->symbol
                        (format
                          "|~a|"
                          (conifer-tree->string node)))))]))))
