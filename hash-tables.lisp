;;;; hash-tables.lisp

(in-package #:mnas-dim-value)

(defun print-hash-table (ht) (maphash #'(lambda (key val) (format t "~S ~S~%" key val)) ht))

(defun hash-table->list (ht)
  (let ((rez nil))
    (maphash #'(lambda (key val) (push (list key val) rez) ) ht)
    rez))

(progn
  (defparameter *nm-vl* (make-hash-table :test #'equal) "Задает соответствие сроки, обозначающей размерность значению.")
  (mapc #'(lambda (el) (setf (gethash (sixth  el) *nm-vl*) (car (last el)))) *si-main-units*)
  (mapc #'(lambda (el) (setf (gethash (sixth  el) *nm-vl*) (car (last el)))) *si-derived-units-tbl-03*)
  (print-hash-table *nm-vl*))

(mapc 
 #'(lambda (n-v)
     (mapc
      #'(lambda (n-c)
	  (setf (gethash (concatenate 'string (first n-c ) (first n-v)) *nm-vl*)
		(vd* (second n-c ) (second n-v))))
      (hash-table->list *m-coeff-en*)))
 (hash-table->list  *nm-vl*))

(progn
  (defparameter *nm-vl-ru->en* (make-hash-table :test #'equal) "Задает соответствие сроки, обозначающей размерность значению.")
  (mapc #'(lambda (el) (setf (gethash (seventh el) *nm-vl-ru->en*) (sixth el))) *si-main-units*)
  (mapc #'(lambda (el) (setf (gethash (seventh el) *nm-vl-ru->en*) (sixth el))) *si-derived-units-tbl-03*)
  (print-hash-table *nm-vl-ru->en* ))

(progn
  (defparameter *nm-vl-en->ru* (make-hash-table :test #'equal) "Задает соответствие сроки, обозначающей размерность значению.")
  (mapc #'(lambda (el) (setf (gethash (sixth el) *nm-vl-en->ru*) (seventh el))) *si-main-units*)
  (mapc #'(lambda (el) (setf (gethash (sixth el) *nm-vl-en->ru*) (seventh el))) *si-derived-units-tbl-03*)
  (print-hash-table *nm-vl-en->ru*))

(progn
  (defparameter *mult-nm-vl* (make-hash-table :test #'equal) "Задает соответствие сроки, обозначающей размерность значению.")
  (maphash #'(lambda (key val) (setf (gethash key *mult-nm-vl*) val)
		     (maphash
		      #'(lambda (m-key m-val)
			  (setf (gethash (concatenate 'string m-key key) *mult-nm-vl*)
				(vd* val m-val)))
		      *m-coeff-en*))
	   *nm-vl*))

(print-hash-table *mult-nm-vl*)
"kgf/mm^2" "kgf/cm^2" "kgf/m^2" "mm_Hg" "mm_H2O"

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(progn
  (defparameter *dim->unit-symbol-en* (make-hash-table :test #'equal) "Задает соответствие размерности величины сторке.")
  (flet ((dimension->string (tbl) (mapc #'(lambda (el) (setf (gethash (vd-dims (nd-value el)) *dim->unit-symbol-en*) (nd-unit-symbol-en el))) (reverse tbl))))
    (mapc #'(lambda (el) (dimension->string el)) (list *nd-si-derived-units-tbl-04* *nd-si-derived-units-tbl-03* *nd-si-derived-units-tbl-02* *nd-si-main-units*))
    (setf (gethash '(0 1 0 0 0 0 0 0 0) *dim->unit-symbol-en*) "kg"))
  (print-hash-table *dim->unit-symbol-en*))

(progn
  (defparameter *unit-symbol-en->dim* (make-hash-table :test #'equal) "Задает соответствие строки обозначающей размернсть списку размерностей")
  (flet ((string->dimension (tbl) (mapc #'(lambda (el) (setf (gethash  (nd-unit-symbol-en el) *unit-symbol-en->dim*) (vd-dims (nd-value el)))) (reverse tbl))))
    (mapc #'(lambda (el) (string->dimension el)) (list *nd-si-derived-units-tbl-04* *nd-si-derived-units-tbl-03* *nd-si-derived-units-tbl-02* *nd-si-main-units*))
    (remhash "g" *unit-symbol-en->dim*)
    (setf (gethash "kg" *unit-symbol-en->dim*)  '(0 1 0 0 0 0 0 0 0)))
  (print-hash-table *unit-symbol-en->dim*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(progn
  (defparameter *dim->unit-symbol-ru* (make-hash-table :test #'equal) "Задает соответствие размерности величины сторке.")
  (flet ((dimension->string-ru (tbl) (mapc #'(lambda (el) (setf (gethash (vd-dims (nd-value el)) *dim->unit-symbol-ru*) (nd-unit-symbol-ru el))) (reverse tbl))))
    (mapc #'(lambda (el) (dimension->string-ru el)) (list *nd-si-derived-units-tbl-04* *nd-si-derived-units-tbl-03* *nd-si-derived-units-tbl-02* *nd-si-main-units*))
    (setf (gethash '(0 1 0 0 0 0 0 0 0) *dim->unit-symbol-ru*) "кг"))
  (print-hash-table *dim->unit-symbol-ru*))

(progn
  (defparameter *unit-symbol-ru->dim* (make-hash-table :test #'equal) "Задает соответствие строки обозначающей размернсть списку размерностей")
  (flet ((string->dimension-ru (tbl) (mapc #'(lambda (el) (setf (gethash  (nd-unit-symbol-ru el) *unit-symbol-ru->dim*) (vd-dims (nd-value el)))) (reverse tbl))))
    (mapc #'(lambda (el) (string->dimension-ru el)) (list *nd-si-derived-units-tbl-04* *nd-si-derived-units-tbl-03* *nd-si-derived-units-tbl-02* *nd-si-main-units*))
    (remhash "г" *unit-symbol-ru->dim*)
    (setf (gethash "кг" *unit-symbol-ru->dim*)  '(0 1 0 0 0 0 0 0 0)))
  (print-hash-table *unit-symbol-ru->dim*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(progn
  (defparameter *dim->quantity-name-en* (make-hash-table :test #'equal) "Задает соответствие размерности наименованию величины.")
  (flet ((dim->quantity-name-en (tbl) (mapc #'(lambda (el) (setf (gethash (vd-dims (nd-value el)) *dim->quantity-name-en*) (nd-quantity-name-en el))) (reverse tbl))))
    (mapc #'(lambda (el) (dim->quantity-name-en el)) (list *nd-si-derived-units-tbl-04* *nd-si-derived-units-tbl-03* *nd-si-derived-units-tbl-02* *nd-si-main-units*)))
  (print-hash-table *dim->quantity-name-en*))

(progn
  (defparameter *quantity-name-en->dim* (make-hash-table :test #'equal) "Задает соответствие строки обозначающей размернсть списку размерностей")
  (flet ((string->dimension (tbl) (mapc #'(lambda (el) (setf (gethash  (nd-quantity-name-en el) *quantity-name-en->dim*) (vd-dims (nd-value el)))) (reverse tbl))))
    (mapc #'(lambda (el) (string->dimension el)) (list *nd-si-derived-units-tbl-04* *nd-si-derived-units-tbl-03* *nd-si-derived-units-tbl-02* *nd-si-main-units*)))
  (print-hash-table *quantity-name-en->dim*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
