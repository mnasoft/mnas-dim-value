;;;; si-units.lisp

(in-package :mnas-dim-value)

(defparameter *nd-not-si-units-tbl-05*  (make-nd-form-list-el *not-si-units-tbl-05*) "Внесистемные единицы, допустимые к применению наравне с единицами СИ")
(setf (documentation  '*nd-not-si-units-tbl-05* 'variable) (documentation  '*not-si-units-tbl-05* 'variable))

;;@intern
(defparameter *nd-not-si-units-tbl-07*  (make-nd-form-list-el *not-si-units-tbl-07*) "Внесистемные единицы, временно допустимые к применению")

(setf (documentation  '*nd-not-si-units-tbl-07* 'variable) (documentation  '*not-si-units-tbl-07* 'variable))

;;@intern
(defparameter *nd-other-units-tbl-b-01* (make-nd-form-list-el *other-units-tbl-b-01*) "Соотношение некоторых внесистемных единиц с единицами СИ")
(setf (documentation  '*nd-other-units-tbl-b-01* 'variable) (documentation  '*other-units-tbl-b-01* 'variable))

(defun check-is-si-table-good (tbl)
  (let ((rez t))
    (mapc
     #'(lambda (el)
	 (when (/= (length el) 9)
	   (setf rez nil)
	   (print el)))
     tbl)
    rez))

(check-is-si-table-good (append *not-si-units-tbl-05* *not-si-units-tbl-07* *other-units-tbl-b-01*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

   (list '("pressure" "stress") '("давление" "напряжение")  "" "" "m_Hg"  "м pт.ст."     (vd* 133322.0       |Pa|))
   (list '("pressure" "stress") '("давление" "напряжение")  "" "" "m_H2O"  "м вод.ст."   (vd* 9806.65        |Pa|))
   (list '("pressure" "stress") '("давление" "напряжение")  "" "" "kgf/m^2"  "кгс/м^2"    (vd/ |*g*|  |m| |m|))
   (list "atm"   (vd* 101325         |Pa|))
   (list "psi"   (vd/
		  ( vd* 0.45359237 |*g*| |kg|)
		  ( vd* 0.0254 |m|)
		  ( vd* 0.0254 |m|)))
   (list "length" "длина" "in"    (vd* 0.0254         |m|))
   (list "lb"    (vd* 0.45359237     |kg|))
   (list "lbf"   (vd* 0.45359237 |*g*| |kg|))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




(mapcar
 #'(lambda (el)
     (if (listp el) (= (length el) 5)
	 T))
 *tbl-g1*)
