;;;; mnas-dim-value.asd

(asdf:defsystem #:mnas-dim-value
  :description "Describe mnas-dim-value here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :serial t
  :depends-on (#:cl-ppcre)
  :components ((:file "package")
	       (:file "classes")
	       (:file "si-units")
       	       (:file "methods")
;;;;           (:file "mnas-dim-value")
	       (:file "constants")	       
	       (:file "razmernost")
;;;;	       (:file "values")
	       ))


