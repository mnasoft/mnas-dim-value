;;;; test.lisp

(in-package #:mnas-dim-value)

(vd/ (vd* 2.56054  |kg| |m| |m| ) |s| |s| |s|)


(vd-val |kg|)
(vd-dims |kg|)

(defparameter *a* (vd*  |kg|))




(gethash (vd-dims *a*) *dimension->string*)

(print-hash-table *dimension->string*)

(vd-print *a* t)
