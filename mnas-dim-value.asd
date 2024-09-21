;;;; mnas-dim-value.asd

(defsystem "mnas-dim-value"
  :description "Describe mnas-dim-value here"
  :author "Mykola Matvyeyev <mnasoft@gmail.com>"
  :license "GNU GENERAL PUBLIC LICENSE Version 3, 29 June 2007 or later"

  :serial nil
  :depends-on ("str" "mnas-string" "mnas-dim-value/generic" ) ;;;; #:cl-ppcre

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
  
  :defsystem-depends-on ("deploy")
  :build-operation "deploy-op"
  :build-pathname "q-calc"
  :entry-point "mnas-dim-value/calc:start"

  :serial nil
  :depends-on ("mnas-dim-value")
    :components ((:module "src/calc"
		:serial nil
                :components ((:file "calc")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defsystem "mnas-dim-value/generic"
  :description "Система определяет обобщенные функции"
  :serial nil
  :components ((:module "src/generic"
		:serial nil
                :components ((:file "generic")))))

(defsystem "mnas-dim-value/class"
  :description "Система определяет классы"
  
  :serial nil
  ;;;; :depends-on ()
  :components ((:module "src/class"
		:serial nil
                :components ((:file "class")))))

(defsystem "mnas-dim-value/mk-class"
  :description "Система определяет классы"
  :serial nil
  :depends-on ("mnas-dim-value/class")
  :components ((:module "src/mk-class"
		:serial nil
                :components ((:file "mk-class")))))

(defsystem "mnas-dim-value/func"
  :description "Система определяет вспомогательные функции"
  
  :serial nil
  ;;;; :depends-on ()
  :components ((:module "src/func"
		:serial nil
                :components ((:file "func")))))

(defsystem "mnas-dim-value/tbl"
  :description "@b(Описание:) система @b(mnas-dim-value/tbl)
опеделяет таблицы системы SI.
"
  :serial nil
  :depends-on (
               "mnas-dim-value/class"
               "mnas-dim-value/mk-class"
               )
  :components ((:module "src/tbl"
		:serial nil
                :components ((:file "tbl")))))

(defsystem "mnas-dim-value/ht"
  :description "@b(Описание:) система @b(mnas-dim-value/ht) определяет
 хеш-таблицы."
  :serial nil
  :depends-on ("mnas-dim-value/class"
               "mnas-dim-value/func"
               "mnas-dim-value/tbl"
               "mnas-hash-table"
               )
  :components ((:module "src/ht"
		:serial nil
                :components ((:file "ht")))))

(defsystem "mnas-dim-value/method"
  :description "Система определяет методы"
  :serial nil
  :depends-on ("mnas-dim-value/class"
               "mnas-dim-value/func"
               "mnas-dim-value/tbl"
               "mnas-hash-table"
               "mnas-dim-value/ht"
               "mnas-dim-value/generic"               
               )
  :components ((:module "src/method" 
		:serial nil
                :components ((:file "method")))))

(defsystem "mnas-dim-value/const"
  :description "Система определяет методы"
  :serial nil
  :depends-on ("mnas-dim-value/class"
               "mnas-dim-value/func"
               "mnas-dim-value/tbl"
               "mnas-hash-table"
               "mnas-dim-value/ht"
               "mnas-dim-value/generic"
               "mnas-dim-value/method"               
               )
  :components ((:module "src/const" 
		:serial nil
                :components ((:file "const")))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
