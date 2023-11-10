(library
  (syntax records)
  (export make-parse-result
	  parse-result-file-id
	  parse-result-parse-tree
	  parse-result-errors
	  parse-result?
	  make-expand-result
	  expand-result-file-id
	  expand-result-ast
	  expand-result-errors)
  (import (rnrs records syntactic))

  (define-record-type
    parse-result
    (fields
      file-id
      parse-tree
      errors))

  (define-record-type
    expand-result
    (fields
      file-id
      ast
      errors)))
