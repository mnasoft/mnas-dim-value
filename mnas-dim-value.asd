;;;; mnas-dim-value.asd

(defsystem "mnas-dim-value"
  :description "Describe mnas-dim-value here"
  :author "Mykola Matvyeyev <mnasoft@gmail.com>"
  :license "GNU GENERAL PUBLIC LICENSE Version 3, 29 June 2007 or later"

  :serial nil
  :depends-on ("str" "mnas-string" ) ;;;; #:cl-ppcre
  :components
  ((:module "src"
    :serial nil
    :components
    ((:file "package")
     (:file "generic"          :depends-on ("package"))
     (:file "razmernost"       :depends-on ("package"))
     (:file "macro"            :depends-on ("method"))
     (:file "hash-tables"      :depends-on ("class"
                                            "defparameter"
                                            "mult-coeff"
                                            "method"
					    "defparameter-vd"
                                            "si-main-units"
					    "si-derived-units"
                                            "constants"
                                            "si-units"))
     (:file "mult-coeff"       :depends-on ("class"
                                            "method"))
     (:file "si-units"         :depends-on ("class"
                                            "method"))
     (:file "constants"        :depends-on ("class"
                                            "method"))
     (:file "si-derived-units" :depends-on ("class"
                                            "method"))
     (:file "si-main-units"    :depends-on ("class"))
     (:file "defparameter-vd"  :depends-on ("class"))
     (:file "method"           :depends-on ("class" "generic"))
     (:file "defparameter"     :depends-on ("package"))	       
     (:file "class"            :depends-on ("package"))
     (:file "mnas-dim-value"   :depends-on ("razmernost"
                                            "macro"
                                            "class"
                                            "defparameter"
                                            "method"
                                            "mult-coeff"
                                            "defparameter-vd"
                                            "si-main-units"
                                            "si-derived-units"
                                            "constants"
                                            "si-units"
                                            "hash-tables"))

     (:file "defunses"         :depends-on ("razmernost"
                                            "macro"
                                            "class"
                                            "defparameter"
                                            "method"
                                            "mult-coeff"
                                            "defparameter-vd"
                                            "si-main-units"
                                            "si-derived-units"
                                            "constants"
                                            "si-units"
                                            "hash-tables"
                                            "mnas-dim-value"))
     (:file "cl-user-funcs"    :depends-on ("razmernost"
                                            "macro"
                                            "class"
                                            "defparameter"
                                            "method"
                                            "mult-coeff"
                                            "defparameter-vd"
                                            "si-main-units"
                                            "si-derived-units"
                                            "constants"
                                            "si-units"
                                            "hash-tables"
                                            "mnas-dim-value"))))))

(defsystem "mnas-dim-value/docs"
  :description "Зависимости для сборки документации"
  :author "Mykola Matvyeyev <mnasoft@gmail.com>"
  :license "GNU GENERAL PUBLIC LICENSE Version 3, 29 June 2007 or later"  
  ;; :depends-on ("mnas-string" "mnas-package")
  :depends-on ("mnas-dim-value" "codex" "mnas-package")
  :components ((:module "src/docs"
		:serial nil
                :components ((:file "docs")))))

(defsystem "mnas-dim-value/calc"
  :description "Describe mnas-dim-value here"
  :author "Mykola Matvyeyev <mnasoft@gmail.com>"
  :license "GNU GENERAL PUBLIC LICENSE Version 3, 29 June 2007 or later"
  
  :defsystem-depends-on ("deploy")
  :build-operation "deploy-op"
  :build-pathname "q-calc"
  :entry-point "mnas-dim-value/calc:start"

  :serial nil
  :depends-on ("mnas-dim-value")
    :components ((:module "src/calc"
		:serial nil
                :components ((:file "calc")))))
