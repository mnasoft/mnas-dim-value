;;;; /src/mnas-dim-value.lisp

(defpackage :mnas-dim-value
  (:nicknames "MDV")
  (:use #:cl
        #:mnas-dim-value/func
        #:mnas-dim-value/class
        #:mnas-dim-value/mk-class
        #:mnas-dim-value/tbl
        #:mnas-hash-table
        #:mnas-dim-value/ht
        #:mnas-dim-value/generic
        #:mnas-dim-value/method
        #:mnas-dim-value/const
        )
  (:export <vd>
           <vd>-val
           <vd>-dims)
  (:export vd
           )
  (:export vd+ vd- vd* vd/
           )
  (:export vd-expt
           vd-sqrt
           )
  (:export |m| |kg| |s| |A| |K| |cd| |mol|
           |rad| |sr|
           )
  (:export |Hz| |N| |Pa| |J| |W|
           |C| |V| |F| |Ω| |S| |Wb|
           |Τ| |H| |lm| |lx| |Bq|
           |Gy| |Sv| |kat|
           )
;;; From :mnas-dim-value/const
  (:export |*g*| |*Gn*| |*C-0*| |*V-0*|
           |*R-0*| |*Na*| |*No*|
           |*k*| |*a-e-m*|
           |*m-e*| |*e*|
           |*F*| |*h*| |*c*| |*μ-0*| |*ε-0*|
           )
;;; From :mnas-dim-value/convert  
  (:export C->K K->C 
           M->K K->M 
           KGS/CM2->PA
           PA->KGS/CM2
           )
  (:export quantity
           )

  (:export 
   HELP
   
   DIM-STRING-BY-DIM-NAME
   DIM-NAME-LIST

   UNUSE-MNAS-DIM-VALUE

   USE-MNAS-DIM-VALUE

   QUANTITY-NAME
   ))

(in-package :mnas-dim-value)

(unexport '(DIMENSIONP *NM-VL-EN->RU* *NM-VL-RU->EN*
           *NM-VL*
           UNIT-NAME))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun dim-string-by-dim-name ( d-type )
  "Пример использования:
;;;;(mnas-dim-value:dim-string-by-dim-name \"length\")
;;;;(mnas-dim-value:dim-string-by-dim-name \"specific entropy\")
;;;;(mnas-dim-value:dim-string-by-dim-name \"capacitance\")
;;;;(mnas-dim-value:dim-string-by-dim-name \"mass density\")
"
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

(defun dim-name-list (&key  (en-ru #'first))
"Возвращает список наименований величин
Пример использования 
;;;;(dim-name-list) 
;;;;(dim-name-list :en-ru #'second)
"
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

(defparameter *help*
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



(defun help ()
"help"
  (format t *help* ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
