;;;; hash-tables.lisp

(in-package #:mnas-dim-value)

(defun print-hash-table (ht) (maphash #'(lambda (key val) (format t "~S ~S~%" key val)) ht))

(progn
  (defparameter *nm-vl* (make-hash-table :test #'equal) "Задает соответствие сроки, обозначающей размерность значению.")
  (mapc #'(lambda (el) (setf (gethash (sixth  el) *nm-vl*) (car (last el)))) *si-main-units*)
  (mapc #'(lambda (el) (setf (gethash (sixth  el) *nm-vl*) (car (last el)))) *si-derived-units-tbl-03*)
  (print-hash-table *nm-vl*))

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
		      *mut-prefix*))
	   *nm-vl*))

"kgf/mm^2" "kgf/cm^2" "kgf/m^2" "mm_Hg" "mm_H2O"

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(progn
  (defparameter *dimension->string* (make-hash-table :test #'equal)
    "Задает соответствие размерности величины сторке.")
  (mapc #'(lambda (el) (setf (gethash (vd-dims (car (last el))) *dimension->string* ) (fourth el))) (reverse *si-derived-units-tbl-04*))
  (mapc #'(lambda (el) (setf (gethash (vd-dims (car (last el))) *dimension->string* ) (sixth  el))) (reverse *si-derived-units-tbl-03*))
  (mapc #'(lambda (el) (setf (gethash (vd-dims (car (last el))) *dimension->string* ) (fourth el))) (reverse *si-derived-units-tbl-02*))
  (mapc #'(lambda (el) (setf (gethash (vd-dims (car (last el))) *dimension->string* ) (sixth  el))) (reverse *si-main-units*))
  (setf (gethash '(0 1 0 0 0 0 0 0 0) *dimension->string*) "kg")
  (print-hash-table *dimension->string*))

(progn
  (defparameter *string->dimension* (make-hash-table :test #'equal)
    "Задает соответствие строки обозначающей размернсть списку размерностей")
  (mapc #'(lambda (el) (setf (gethash (fourth el) *string->dimension*) (vd-dims (car (last el))))) (reverse *si-derived-units-tbl-04*))
  (mapc #'(lambda (el) (setf (gethash (sixth  el) *string->dimension*) (vd-dims (car (last el))))) (reverse *si-derived-units-tbl-03*))
  (mapc #'(lambda (el) (setf (gethash (fourth el) *string->dimension*) (vd-dims (car (last el))))) (reverse *si-derived-units-tbl-02*))
  (mapc #'(lambda (el) (setf (gethash (sixth  el) *string->dimension*) (vd-dims (car (last el))))) (reverse *si-main-units*))
  (remhash "g" *string->dimension*)
  (setf (gethash "kg" *string->dimension*)  '(0 1 0 0 0 0 0 0 0))
  (print-hash-table *string->dimension*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(progn
  (defparameter *dimension->string-ru* (make-hash-table :test #'equal ))
  (mapc #'(lambda (el) (setf (gethash (vd-dims (car (last el))) *dimension->string-ru*) (seventh el))) *si-main-units*)
  (mapc #'(lambda (el) (setf (gethash (vd-dims (car (last el))) *dimension->string-ru*) (fourth  el))) *si-derived-units-tbl-02*)
  (mapc #'(lambda (el) (setf (gethash (vd-dims (car (last el))) *dimension->string-ru*) (seventh el))) *si-derived-units-tbl-03*)
  (mapc #'(lambda (el) (setf (gethash (vd-dims (car (last el))) *dimension->string-ru*) (fourth  el))) *si-derived-units-tbl-04*)
  (setf (gethash '(0 1 0 0 0 0 0 0 0) *dimension->string-ru*) "кг")
  (print-hash-table *dimension->string-ru*))

(progn
  (defparameter *string->dimension-ru* (make-hash-table :test #'equal ))
  (mapc #'(lambda (el) (setf (gethash (seventh el) *string->dimension-ru*) (vd-dims (car (last el))))) *si-main-units*)
  (mapc #'(lambda (el) (setf (gethash (fourth el)  *string->dimension-ru*) (vd-dims (car (last el))))) *si-derived-units-tbl-02*)
  (mapc #'(lambda (el) (setf (gethash (seventh el) *string->dimension-ru*) (vd-dims (car (last el))))) *si-derived-units-tbl-03*)
  (mapc #'(lambda (el) (setf (gethash (fourth el)  *string->dimension-ru*) (vd-dims (car (last el))))) *si-derived-units-tbl-04*)
  (remhash "г" *string->dimension-ru*)
  (setf (gethash "кг" *string->dimension-ru*)  '(0 1 0 0 0 0 0 0 0))
  (print-hash-table *string->dimension-ru*))

"ToDo
Необходимо после чевертых элементов добавить пятый со строками на русском языке
*si-derived-units-tbl-02*
*si-derived-units-tbl-04*
"

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(progn
  (defparameter *dimension->name* (make-hash-table :test #'equal ))
  (mapc #'(lambda (el) (setf (gethash (vd-dims (car (last el))) *dimension->name*) (first el))) *si-main-units*)
  (mapc #'(lambda (el) (setf (gethash (vd-dims (car (last el))) *dimension->name*) (first el))) *si-derived-units-tbl-02*)
  (mapc #'(lambda (el) (setf (gethash (vd-dims (car (last el))) *dimension->name*) (first el))) *si-derived-units-tbl-03*)
  (mapc #'(lambda (el) (setf (gethash (vd-dims (car (last el))) *dimension->name*) (first el))) *si-derived-units-tbl-04*)
  (print-hash-table *dimension->name*))

(progn
  (defparameter *name->dimension* (make-hash-table :test #'equal ))
  (mapc #'(lambda (el) (setf (gethash (first el) *name->dimension*) (vd-dims (car (last el))))) *si-main-units*)
  (mapc #'(lambda (el) (setf (gethash (first el) *name->dimension*) (vd-dims (car (last el))))) *si-derived-units-tbl-02*)
  (mapc #'(lambda (el) (setf (gethash (first el) *name->dimension*) (vd-dims (car (last el))))) *si-derived-units-tbl-03*)
  (mapc #'(lambda (el) (setf (gethash (first el) *name->dimension*) (vd-dims (car (last el))))) *si-derived-units-tbl-04*)
  (print-hash-table *name->dimension*))

"ToDo 
Необходимо доработать так, чтобы каждой размерности в переменной *dimension->name* соответствовали списки имен зазмерностей
Необходимо доработать так, чтобы ключами были строки, а не списки
"
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
