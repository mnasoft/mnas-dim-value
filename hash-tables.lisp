;;;; hash-tables.lisp

(in-package #:mnas-dim-value)


(progn
  (defparameter *dimension->string* (make-hash-table :test #'equal ))
  (mapc #'(lambda (el) (setf (gethash (d-lst (car (last el))) *dimension->string* ) (fourth el))) (reverse *si-derived-units-tbl-04*))
  (mapc #'(lambda (el) (setf (gethash (d-lst (car (last el))) *dimension->string* ) (sixth  el))) (reverse *si-derived-units-tbl-03*))
  (mapc #'(lambda (el) (setf (gethash (d-lst (car (last el))) *dimension->string* ) (fourth el))) (reverse *si-derived-units-tbl-02*))
  (mapc #'(lambda (el) (setf (gethash (d-lst (car (last el))) *dimension->string* ) (sixth  el))) (reverse *si-main-units*))
  (maphash #'(lambda (key val) (format t "~S ~S~%" key val)) *dimension->string*))

(progn
  (defparameter *string->dimension* (make-hash-table :test #'equal ))
  (mapc #'(lambda (el) (setf (gethash (fourth el) *string->dimension*) (d-lst (car (last el))))) (reverse *si-derived-units-tbl-04*))
  (mapc #'(lambda (el) (setf (gethash (sixth  el) *string->dimension*) (d-lst (car (last el))))) (reverse *si-derived-units-tbl-03*))
  (mapc #'(lambda (el) (setf (gethash (fourth el) *string->dimension*) (d-lst (car (last el))))) (reverse *si-derived-units-tbl-02*))
  (mapc #'(lambda (el) (setf (gethash (sixth  el) *string->dimension*) (d-lst (car (last el))))) (reverse *si-main-units*))
  (maphash #'(lambda (key val) (format t "~S ~S~%" key val)) *string->dimension*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(progn
  (defparameter *dimension->string-ru* (make-hash-table :test #'equal ))
  (mapc #'(lambda (el) (setf (gethash (d-lst (car (last el))) *dimension->string-ru*) (seventh el))) *si-main-units*)
  (mapc #'(lambda (el) (setf (gethash (d-lst (car (last el))) *dimension->string-ru*) (fourth  el))) *si-derived-units-tbl-02*)
  (mapc #'(lambda (el) (setf (gethash (d-lst (car (last el))) *dimension->string-ru*) (seventh el))) *si-derived-units-tbl-03*)
  (mapc #'(lambda (el) (setf (gethash (d-lst (car (last el))) *dimension->string-ru*) (fourth  el))) *si-derived-units-tbl-04*)
  (maphash #'(lambda (key val) (format t "~S ~S~%" key val)) *dimension->string-ru*))

(progn
  (defparameter *string->dimension-ru* (make-hash-table :test #'equal ))
  (mapc #'(lambda (el) (setf (gethash (seventh el) *string->dimension-ru*) (d-lst (car (last el))))) *si-main-units*)
  (mapc #'(lambda (el) (setf (gethash (fourth el)  *string->dimension-ru*) (d-lst (car (last el))))) *si-derived-units-tbl-02*)
  (mapc #'(lambda (el) (setf (gethash (seventh el) *string->dimension-ru*) (d-lst (car (last el))))) *si-derived-units-tbl-03*)
  (mapc #'(lambda (el) (setf (gethash (fourth el)  *string->dimension-ru*) (d-lst (car (last el))))) *si-derived-units-tbl-04*)
  (maphash #'(lambda (key val) (format t "~S ~S~%" key val)) *string->dimension-ru*))

"
ToDo Необходимо после чевертых элементов добавить пятый со строками на русском языке
*si-derived-units-tbl-02*
*si-derived-units-tbl-04*
"

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(progn
  (defparameter *dimension->name* (make-hash-table :test #'equal ))
  (mapc #'(lambda (el) (setf (gethash (d-lst (car (last el))) *dimension->name*) (first el))) *si-main-units*)
  (mapc #'(lambda (el) (setf (gethash (d-lst (car (last el))) *dimension->name*) (first el))) *si-derived-units-tbl-02*)
  (mapc #'(lambda (el) (setf (gethash (d-lst (car (last el))) *dimension->name*) (first el))) *si-derived-units-tbl-03*)
  (mapc #'(lambda (el) (setf (gethash (d-lst (car (last el))) *dimension->name*) (first el))) *si-derived-units-tbl-04*)
  (maphash #'(lambda (key val) (format t "~S ~S~%" key val)) *dimension->name*))

(progn
  (defparameter *name->dimension* (make-hash-table :test #'equal ))
  (mapc #'(lambda (el) (setf (gethash (first el) *name->dimension*) (d-lst (car (last el))))) *si-main-units*)
  (mapc #'(lambda (el) (setf (gethash (first el) *name->dimension*) (d-lst (car (last el))))) *si-derived-units-tbl-02*)
  (mapc #'(lambda (el) (setf (gethash (first el) *name->dimension*) (d-lst (car (last el))))) *si-derived-units-tbl-03*)
  (mapc #'(lambda (el) (setf (gethash (first el) *name->dimension*) (d-lst (car (last el))))) *si-derived-units-tbl-04*)
  (maphash #'(lambda (key val) (format t "~S ~S~%" key val)) *name->dimension*))

"
ToDo 
Необходимо доработать так, чтобы каждой размерности в переменной *dimension->name* соответствовали списки имен зазмерностей
Необходимо доработать так, чтобы ключами были строки, а не списки
"
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
