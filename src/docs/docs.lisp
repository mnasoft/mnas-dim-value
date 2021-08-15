(defpackage #:mnas-dim-value/docs
  (:use #:cl ) 
  (:nicknames "MDV/DOCS")
  (:export make-all)
  (:documentation "Пакет @b(mnas-dim-value/docs) содержит функции
  генерирования и публикации документации."))

(in-package :mnas-dim-value/docs)

(defun make-document ()
  (loop
    :for i :in
    '(
      (:mnas-dim-value nil)
      )
    :do (apply #'mnas-package:document i)))

(defun make-graphs ()
  (loop
    :for i :in
    '(
      :mnas-dim-value
      )
    :do (mnas-package:make-codex-graphs i i)))

(defun make-all (&aux
                   (of (if (find (uiop:hostname)
                                 mnas-package:*intranet-hosts*
                                 :test #'string=)
                           '(:type :multi-html :template :gamma)
                           '(:type :multi-html :template :minima))))
  "@b(Описание:) функция @b(make-all) служит для создания документации.

 Пакет документации формируется в каталоге
~/public_html/Common-Lisp-Programs/mnas-dim-value.
"
  (mnas-package:make-html-path :mnas-dim-value/docs)
  (make-document)
  (make-graphs)
  (mnas-package:make-mainfest-lisp
   '(:mnas-dim-value :mnas-dim-value/docs)
   "Mnas-Dim-Value"
   '("Nick Matvyeyev")
   (mnas-package:find-sources "mnas-dim-value")
   :output-format of)
  (codex:document :mnas-dim-value)
  (make-graphs)
  (mnas-package:copy-doc->public-html "mnas-dim-value")
  (mnas-package:rsync-doc "mnas-dim-value"))

;;;; (make-all)
