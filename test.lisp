;;;; test.lisp

(in-package #:mnas-dim-value)

(defparameter *nd-list*
  (reverse
   (append
    *nd-si-main-units*
    *nd-si-derived-units-tbl-02*
    *nd-si-derived-units-tbl-03*
    *nd-si-derived-units-tbl-04*
    *nd-not-si-units-tbl-05*
    *nd-not-si-units-tbl-07*)))

(defun quantity-name (value &key (vd-language *vd-language*))
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

(quantity-name (vd* |m| |m| |m|) )

(vd/ (vd* |N| |m|) |rad|)
