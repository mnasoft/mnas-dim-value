;;;; si-main-units.lisp

(in-package :mnas-dim-value/tbl)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;@intern
(defparameter *nd-si-main-units* (make-nd-items    *si-main-units*)  "Задает основные единицы измерения системы SI.")

(setf (documentation  '*nd-si-main-units* 'variable)  (documentation  '*si-main-units* 'variable))

