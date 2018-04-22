;;;; mnas-dim-value.asd

(asdf:defsystem #:mnas-dim-value
  :description "Describe mnas-dim-value here"
  :author "Nick Matvyeyev <mnasoft@gmail.com>"
  :license "GNU GENERAL PUBLIC LICENSE Version 3, 29 June 2007"
  :serial t
  :depends-on (#:cl-ppcre)
  :components ((:file "package")
	       (:file "classes")
       	       (:file "si-main-units")
	       (:file "si-derived-units")
	       (:file "constants") 
	       (:file "si-units")
	       (:file "mult-coeff")
	       (:file "hash-tables")
       	       (:file "methods")
	       (:file "marco")

	       (:file "razmernost")
	       (:file "mnas-dim-value")	       
;;;;	       (:file "values")
	       ))


