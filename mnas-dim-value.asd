;;;; mnas-dim-value.asd

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

(defsystem "mnas-dim-value/macro"
  :description "Система определяет методы"
  :serial nil
  :depends-on ("mnas-dim-value/convert"
;;;               
               "mnas-dim-value/class"
               "mnas-dim-value/func"
               "mnas-dim-value/tbl"
               "mnas-hash-table"
               "mnas-dim-value/ht"
               "mnas-dim-value/generic"
               "mnas-dim-value/method"
               "mnas-dim-value/const" 
               )
  :components ((:module "src/macro" 
		:serial nil
                :components ((:file "macro")))))

(defsystem "mnas-dim-value/convert"
  :description "Система определяет функции конвертирования."
  :serial nil
  :components ((:module "src/convert" 
		:serial nil
                :components ((:file "convert")))))

(defsystem "mnas-dim-value"
  :author "Mykola Matvyeyev <mnasoft@gmail.com>"
  :maintainer "Mykola Matvyeyev <mnasoft@gmail.com>"    
  :name "Mnas Dim Value"
  :license "GNU GENERAL PUBLIC LICENSE Version 3, 29 June 2007 or later"
  :homepage "https://github.com/mnasoft/mnas-dim-value"
  :version "0.0.3" 
  :serial nil
  :depends-on (
               "mnas-dim-value/class"
               "mnas-dim-value/func"
               "mnas-dim-value/tbl"
               "mnas-hash-table"
               "mnas-dim-value/ht"
               "mnas-dim-value/generic"
               "mnas-dim-value/method"
               "mnas-dim-value/const"
               "mnas-dim-value/macro"
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;               
               "str" "mnas-string"
               ) ;;;; #:cl-ppcre

  :components  ((:module "src"
                 :serial nil
                 :components ((:file "mnas-dim-value"))))
  :description "Describe mnas-dim-value here"
  :long-description
  #.(uiop:read-file-string
     (uiop:subpathname *load-pathname* "org/README.org"))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+nil 
(defsystem "mnas-dim-value/sample"
  :author "John Q. Lisper <jql@example.com>"
  :maintainer "John Q. Lisper <jql@example.com>"
  :license "MIT"
  :homepage "https://github.com/johnqlisp/my-project"
  :version "0.1"
  :depends-on (:local-time
               :clack)
  :components ((:module "src"
                :serial t
                :components
                ((:file "my-project"))))
  :description "A description of the project."
  :long-description
  #.(uiop:read-file-string
     (uiop:subpathname *load-pathname* "README.md"))
  :in-order-to ((test-op (test-op my-project-test))))
