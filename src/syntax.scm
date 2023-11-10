(library
  (syntax)
  (export parse-result?
	  parse-result-file-id
	  parse-result-parse-tree
	  parse-result-errors
	  pt-root-sexps
	  (rename (register-queries syntax-register-queries)))
  (import (only (syntax query)
		register-queries)
	  (only (syntax records)
		parse-result?
		parse-result-file-id
		parse-result-parse-tree
		parse-result-errors)
	  (only (syntax parse-tree)
		pt-root-sexps)))
