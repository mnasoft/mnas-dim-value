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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod mult ((x vd) (y string))
  (mult x (gethash y *nm-vl*)))

(defmethod mult ((y string) (x vd))
  (mult x (gethash y *nm-vl*)))

(defmethod mult ((x number) (y string))
  (mult x (gethash y *nm-vl*)))

(defmethod mult ((y number) (x vd))
  (mult x (gethash y *nm-vl*)))


(defmethod vd* ((x string) &rest args)
  (let ((rez (gethash x *nm-vl*)))
    (dolist (y args)
      (setf rez (mult rez y)))
    rez))

(defmethod vd* ((x string) &rest args)
  (let ((rez (gethash x *nm-vl*)))
    (dolist (y args)
      (setf rez (mult rez y)))
    rez))

(defmethod vd ((x string))
  (gethash x *nm-vl*))

(vd "sr")

(defun vd (x &key (m 0) (kg 0) (s 0) (A 0) (K 0) (cd 0) (mol 0) (rad 0) (sr 0))
  (make-instance 'vd :val x :dims (list m kg s A K cd mol rad sr) ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


