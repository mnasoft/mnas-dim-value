;;;; defunses.lisp

(in-package #:mnas-dim-value)

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

(defun quantity-name (value &key (vd-language *vd-language*))
  "Возвращает наименование величины.
Пример использования:
;;;; (quantity-name (vd/ |kg| |m| |m| |m|) :vd-language :en) => (\"density\" \"mass density\")
;;;; (quantity-name (vd/ (vd* |kg| *g*) (vd-expt (vd* 0.01 |m|) 2) 1000))
"
  (let ((rez nil)
	(item nil)
	(quantity-name-language
	 (cond
	   ((eq vd-language :en) #'nd-quantity-name-en)
	   ((eq vd-language :ru) #'nd-quantity-name-ru))))
    (mapc
     #'(lambda (el)
	 (when (equal (vd-dims (nd-value el))
		      (vd-dims value))
	   (setf item (funcall quantity-name-language el))
	   (cond
	     ((stringp item) (push item rez))
	     ((listp item) (setf rez (append item rez))))))
     *nd-list*)
    (remove-duplicates rez :test #'equal)))

(quantity-name (vd/ "N" (vd*  "cm" "cm")))

(quantity-name (vd*  "cm" "cm"))
