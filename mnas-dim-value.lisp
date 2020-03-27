;;;; mnas-dim-value.lisp

(in-package #:mnas-dim-value)

(annot:enable-annot-syntax)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@export
@annot.doc:doc
"Перевод градусов значения, заданного в градусах Кельвина, в градусы Цельсия."
(defun K->C(K) (- K 273.15))

@export
@annot.doc:doc
"Перевод градусов значения, заданного в градусах Цельсия, в градусы Кельвина."
(defun C->K(C) (+ C 273.15))

@export
@annot.doc:doc
"Перевод значения с приставкой кило в число с приставкой мега"
(defun k->M (k) (* 0.001 k))

@export
@annot.doc:doc
"Перевод значения с приставкой мега в число с приставкой кило"
(defun M->k (M) (* 1000.0 M))

@export
@annot.doc:doc
"Переводит значение давления, заданное в kgs/cm2, в Pa."
(defun kgs/cm2->Pa (kgs/cm2) (* 9.8065 10000.0 kgs/cm2))

@export
@annot.doc:doc
"Переводит значение давления, заданное в Pa, в kgs/cm2."
(defun Pa->kgs/cm2 (Pa) (/ Pa 9.8065 10000.0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@export
@annot.doc:doc
  "Пример использования:
;;;;(mnas-dim-value:dim-string-by-dim-name \"length\")
;;;;(mnas-dim-value:dim-string-by-dim-name \"specific entropy\")
;;;;(mnas-dim-value:dim-string-by-dim-name \"capacitance\")
;;;;(mnas-dim-value:dim-string-by-dim-name \"mass density\")
"
(defun dim-string-by-dim-name ( d-type )

  (let ((rez nil))
    (mapc #'(lambda (f-data)
	      (let ((func-dim-string (first f-data))
		    (data-tbl        (second f-data)))
		(mapc
		 #'(lambda (el )
		     (when (or
			    (and (stringp (first el)) (string= d-type (first el)))
			    (and (listp (first el)) (find  d-type (first el) :test #'equal)))
		       (push (funcall func-dim-string el) rez)))
		 data-tbl)))
	  (list
	   (list #'sixth  *si-main-units*)
	   (list #'fourth *si-derived-units-tbl-02*)
	   (list #'sixth  *si-derived-units-tbl-03*)
	   (list #'fourth *si-derived-units-tbl-04*)
	   (list #'fifth  *not-si-units-tbl-05*)))
    rez))

@export @annot.doc:doc "Возвращает список наименований величин
Пример использования 
;;;;(dim-name-list) 
;;;;(dim-name-list :en-ru #'second)
"
(defun dim-name-list (&key  (en-ru #'first))

  (let ((rez nil))
    (mapc #'(lambda (f-data)
	      (let ((func-dim-string (first f-data))
		    (data-tbl        (second f-data)))
		(mapc
		 #'(lambda (el )
		     (cond
		       ((stringp (funcall func-dim-string el))
			(push (funcall func-dim-string el) rez))
		       ((listp (funcall func-dim-string el))
			(dolist (i (funcall func-dim-string el))
			  (push i rez)))))
		 data-tbl)))
	  (list
	   (list en-ru *si-main-units*)
	   (list en-ru *si-derived-units-tbl-02*)
	   (list en-ru *si-derived-units-tbl-03*)
	   (list en-ru *si-derived-units-tbl-04*)
	   (list en-ru *not-si-units-tbl-05*)))
    (delete-duplicates (sort rez #'string< ) :test #'equal )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@export
(defparameter help 
  " 
 (in-package :mnas-dim-value)                         ;;;; Загрузка пакета
 (quantity 220 \"V\" 15 \"A\")                            ;;;; Пример 1
 (quantity 101325 \"Pa\" + ( 2.0 *g* / (1 * \"cm\" ^ 2))) ;;;; Пример 2
 (quantity 36 *d + 15 *m  + 45.23 *s)
 ...
 (quit)                                               ;;;; Выход без сохранения

;;;; Выход с сохранением
 *home*
 (defparameter *home* \"d:\\\")                          ;;;; Каталог для записи
 (slad)                                               ;;;; Сохранить состояние и завершить работу
")

@export
@annot.doc:doc
"help"
(defun help () (format t help ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
