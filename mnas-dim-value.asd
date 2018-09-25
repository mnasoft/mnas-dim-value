;;;; mnas-dim-value.asd

(defsystem #:mnas-dim-value
  :description "Describe mnas-dim-value here"
  :author "Nick Matvyeyev <mnasoft@gmail.com>"
  :license "GNU GENERAL PUBLIC LICENSE Version 3, 29 June 2007 or later"
  :serial nil
  :depends-on (#:str #:mnas-string ) ;;;; #:cl-ppcre
  :components ((:file "package")
;;;;           (:file "cl-user-funcs"    :depends-on ("package"))
;;;;           (:file "mnas-dim-value"   :depends-on ("package" "classes" "methods" "defparameter-vd" "si-main-units" "si-derived-units" "constants" "si-units" "hash-tables")) 
               (:file "razmernost"       :depends-on ("package"))
               (:file "marco"            :depends-on ("package"))
               (:file "hash-tables"      :depends-on ("package" "classes" "defparameter" "mult-coeff" "methods" "defparameter-vd" "si-main-units" "si-derived-units" "constants" "si-units"))
               (:file "mult-coeff"       :depends-on ("package" "classes" "methods"))
               (:file "si-units"         :depends-on ("package" "classes" "methods"))
               (:file "constants"        :depends-on ("package" "classes" "methods"))
               (:file "si-derived-units" :depends-on ("package" "classes" "methods"))
               (:file "si-main-units"    :depends-on ("package" "classes"))
               (:file "defparameter-vd"  :depends-on ("package" "classes"))
               (:file "methods"          :depends-on ("package" "classes"))
               (:file "defparameter"     :depends-on ("package"))	       
               (:file "classes"          :depends-on ("package"))
	       ))
