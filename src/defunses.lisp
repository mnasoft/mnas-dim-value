;;;; defunses.lisp

(in-package :mnas-dim-value)

(defparameter *nd-list*
  (reverse
   (append
    *nd-si-main-units*
    *nd-si-derived-units-tbl-02*
    *nd-si-derived-units-tbl-03*
    *nd-si-derived-units-tbl-04*
    *nd-not-si-units-tbl-05*
    *nd-not-si-units-tbl-07*
    )))



