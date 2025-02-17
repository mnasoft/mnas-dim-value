(defpackage :mnas-dim-value/tbl
  (:use #:cl
        #:mnas-dim-value/class
        #:mnas-dim-value/mk-class
        )
  (:export nd-check
           nd-check-table))

(in-package :mnas-dim-value/tbl)

(defun nd-check (nd)
  (unless (and (eq '<vd> (type-of (<nd>-value nd)))
               (listp (<nd>-coeff nd)))
    (format t "~S~%" nd)))

(defun nd-check-table (table)
  "Проверка таблиц за исключением первой и седьмой на наличие ошибок."
  (loop :for nd :in table
        :do (nd-check nd)))
