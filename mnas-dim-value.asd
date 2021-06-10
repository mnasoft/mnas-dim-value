;;;; mnas-dim-value.asd

(defsystem "mnas-dim-value"
  :description "Describe mnas-dim-value here"
  :author "Nick Matvyeyev <mnasoft@gmail.com>"
  :license "GNU GENERAL PUBLIC LICENSE Version 3, 29 June 2007 or later"
  :serial nil
  :depends-on (#:str #:mnas-string ) ;;;; #:cl-ppcre
  :components
  ((:file "package")
   (:module "src" :depends-on ("package")
    :serial nil
    :components
    ((:file "razmernost" )
     (:file "marco"            :depends-on ("methods"))
     (:file "hash-tables"      :depends-on ("classes" "defparameter" "mult-coeff" "methods"
						      "defparameter-vd" "si-main-units"
						      "si-derived-units" "constants" "si-units"))
     (:file "mult-coeff"       :depends-on ("classes" "methods"))
     (:file "si-units"         :depends-on ("classes" "methods"))
     (:file "constants"        :depends-on ("classes" "methods"))
     (:file "si-derived-units" :depends-on ("classes" "methods"))
     (:file "si-main-units"    :depends-on ("classes"))
     (:file "defparameter-vd"  :depends-on ("classes"))
     (:file "methods"          :depends-on ("classes"))
     (:file "defparameter"     )	       
     (:file "classes"          )
     (:file "mnas-dim-value"
      :depends-on ("razmernost" "marco" "classes" "defparameter" "methods"
				"mult-coeff" "defparameter-vd"
				"si-main-units" "si-derived-units" "constants" "si-units" "hash-tables"))
     (:file "values"
      :depends-on ("razmernost" "marco" "classes" "defparameter" "methods"
				"mult-coeff" "defparameter-vd"
				"si-main-units" "si-derived-units" "constants" "si-units" "hash-tables" "mnas-dim-value"))
     (:file "defunses"
      :depends-on ("razmernost" "marco" "classes" "defparameter" "methods"
				"mult-coeff" "defparameter-vd"
				"si-main-units" "si-derived-units" "constants" "si-units" "hash-tables" "mnas-dim-value"))
     (:file "cl-user-funcs"
      :depends-on ("razmernost" "marco" "classes" "defparameter" "methods"
				"mult-coeff" "defparameter-vd"
				"si-main-units" "si-derived-units" "constants" "si-units" "hash-tables" "mnas-dim-value"))))))


(defsystem "mnas-dim-value/docs"
  :description "Зависимости для сборки документации"
  :author "Nick Matvyeyev <mnasoft@gmail.com>"
  :license "GNU GENERAL PUBLIC LICENSE Version 3, 29 June 2007 or later"  
  :depends-on ("mnas-string" "mnas-package"))
