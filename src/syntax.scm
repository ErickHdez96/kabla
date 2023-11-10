(library
  (syntax)
  (export parse-result?
	  parse-result-file-id
	  parse-result-parse-tree
	  parse-result-errors
	  pt-root-sexps
	  (rename (register-queries syntax-register-queries)))
  (import (only (syntax query)
		parse-result?
		parse-result-file-id
		parse-result-parse-tree
		parse-result-errors
		register-queries)
	  (only (syntax parse-tree)
		pt-root-sexps)))
