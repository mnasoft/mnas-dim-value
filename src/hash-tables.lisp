;;;; hash-tables.lisp

(in-package :mnas-dim-value)

(defun print-hash-table (ht &optional (s t))
"@b(Описание:) функция print-hash-table выполняет вывод содержимого таблицы в поток s.
@begin[lang=lisp](code)
 (print-hash-table *nm-vl*)
@end(code)
"
  (maphash #'(lambda (key val) (format s "~S ~S~%" key val)) ht))
(defun hash-table->list (ht)
  (let ((rez nil))
    (maphash #'(lambda (key val) (push (list key val) rez) ) ht)
    rez))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *nm-vl-loaded* nil)

(defun is-in-range (val r-list)
" @b(Описание:) функция is-in-range служит для определения того
может-ли использоваться та или иная множительная приставка 
системы SI для данной единицы измерения.

 @b(Переменые:)
@begin(list)
 @item(val - множитель, определяемый формулой val=10@sup(n), где n - целое число;)
 @item(r-list - список, содержащий списки диапазонов. 
Диапазон - упорядоченый список двух целых - (n@sub(1) n@sub(2)).
Для n@sub(1) и n@sub(2) должно выполняться следующее неравенство n@sub(1) <= n@sub(2).)
@end(list)

 Функция возвращает T, если для одного из диапазонов выполняется следующее неравенство:
10@sup(n@sub(1)) <= val <= 10@sup(n@sub(2))

 @b(Пример использования:)
@begin[lang=lisp](code)
 (is-in-range 1/10 '((0 2))) => nil
 (is-in-range 1    '((0 2))) => T
 (is-in-range 10   '((0 2))) => T
 (is-in-range 100  '((0 2))) => T
 (is-in-range 1000 '((0 2))) => NIL
 (is-in-range 1000 '((0 2) (4 6))) => NIL
@end(code)
"
    (eval (append (list 'or)
	  (mapcar
	   #'(lambda (range)
	       (let ((r-rez
		      (mapcar
		       #'(lambda (el) (expt 10 el)) range)))
		 (list '<= (first r-rez) val (second r-rez) )))
	   r-list))))

(defun add-multiplid-values (var)
  (setf (gethash (nd-unit-symbol-en var) *nm-vl*)
	(nd-value var))
  (mapcar
   #'(lambda (el)
       (when (is-in-range (second el) (nd-coeff var))
	 (setf (gethash (concatenate 'string (first el) (nd-unit-symbol-en var)) *nm-vl*)
	       (vd* (second el) (nd-value var)))))
   (hash-table->list *m-coeff-en*)))

(defun load-nm-vl ()
  (unless *nm-vl-loaded*
    (mapc #'add-multiplid-values
	  (append *nd-si-derived-units-tbl-03* *nd-si-main-units*
		  *nd-not-si-units-tbl-07* *nd-not-si-units-tbl-05*
		  *nd-other-units-tbl-b-01*))
    (print-hash-table *nm-vl*)
    (setf *nm-vl-loaded* t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *nm-vl-ru->en-loaded* nil)

(defun load-nm-vl-ru->en ()
  (unless *nm-vl-ru->en-loaded*
    (mapc #'(lambda (el) (setf (gethash (seventh el) *nm-vl-ru->en*) (sixth el))) *si-main-units*)
    (mapc #'(lambda (el) (setf (gethash (seventh el) *nm-vl-ru->en*) (sixth el))) *si-derived-units-tbl-03*)
    (print-hash-table *nm-vl-ru->en* )
    (setf *nm-vl-ru->en-loaded* t)))

(defparameter *nm-vl-en->ru-loaded* nil)

(defun load-nm-vl-en->ru ()
  (unless *nm-vl-en->ru-loaded*
    (mapc #'(lambda (el) (setf (gethash (sixth el) *nm-vl-en->ru*) (seventh el))) *si-main-units*)
    (mapc #'(lambda (el) (setf (gethash (sixth el) *nm-vl-en->ru*) (seventh el))) *si-derived-units-tbl-03*)
    (print-hash-table *nm-vl-en->ru*)
    (setf *nm-vl-en->ru-loaded* t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *dim->unit-symbol-en-loaded* nil)

(defun load-dim->unit-symbol-en ()
  (unless *dim->unit-symbol-en-loaded*
    (flet ((dimension->string (tbl) (mapc #'(lambda (el) (setf (gethash (vd-dims (nd-value el)) *dim->unit-symbol-en*) (nd-unit-symbol-en el))) (reverse tbl))))
      (mapc #'(lambda (el) (dimension->string el)) (list *nd-si-derived-units-tbl-04* *nd-si-derived-units-tbl-03* *nd-si-derived-units-tbl-02* *nd-si-main-units*))
      (setf (gethash '(0 1 0 0 0 0 0 0 0) *dim->unit-symbol-en*) "kg"))
    (print-hash-table *dim->unit-symbol-en*)
    (setf *dim->unit-symbol-en-loaded* t)))

(defparameter *unit-symbol-en->dim-loaded* nil)

(defun load-unit-symbol-en->dim ()
  (unless *unit-symbol-en->dim-loaded*
    (flet ((string->dimension (tbl) (mapc #'(lambda (el) (setf (gethash  (nd-unit-symbol-en el) *unit-symbol-en->dim*) (vd-dims (nd-value el)))) (reverse tbl))))
      (mapc #'(lambda (el) (string->dimension el)) (list *nd-si-derived-units-tbl-04* *nd-si-derived-units-tbl-03* *nd-si-derived-units-tbl-02* *nd-si-main-units*))
      (remhash "g" *unit-symbol-en->dim*)
      (setf (gethash "kg" *unit-symbol-en->dim*)  '(0 1 0 0 0 0 0 0 0)))
    (print-hash-table *unit-symbol-en->dim*)
    (setf *unit-symbol-en->dim-loaded* t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *dim->unit-symbol-ru-loaded* nil)

(defun load-dim->unit-symbol-ru ()
  (unless *dim->unit-symbol-ru-loaded*
    (flet ((dimension->string-ru (tbl) (mapc #'(lambda (el) (setf (gethash (vd-dims (nd-value el)) *dim->unit-symbol-ru*) (nd-unit-symbol-ru el))) (reverse tbl))))
      (mapc #'(lambda (el) (dimension->string-ru el)) (list *nd-si-derived-units-tbl-04* *nd-si-derived-units-tbl-03* *nd-si-derived-units-tbl-02* *nd-si-main-units*))
      (setf (gethash '(0 1 0 0 0 0 0 0 0) *dim->unit-symbol-ru*) "кг"))
    (print-hash-table *dim->unit-symbol-ru*)
    (setf *dim->unit-symbol-ru-loaded* t)))

(defparameter *unit-symbol-ru->dim-loaded* nil)

(defun load-unit-symbol-ru->dim ()
  (unless *unit-symbol-ru->dim-loaded*
    (flet ((string->dimension-ru (tbl) (mapc #'(lambda (el) (setf (gethash  (nd-unit-symbol-ru el) *unit-symbol-ru->dim*) (vd-dims (nd-value el)))) (reverse tbl))))
      (mapc #'(lambda (el) (string->dimension-ru el)) (list *nd-si-derived-units-tbl-04* *nd-si-derived-units-tbl-03* *nd-si-derived-units-tbl-02* *nd-si-main-units*))
      (remhash "г" *unit-symbol-ru->dim*)
      (setf (gethash "кг" *unit-symbol-ru->dim*)  '(0 1 0 0 0 0 0 0 0)))
    (print-hash-table *unit-symbol-ru->dim*)
    (setf *unit-symbol-ru->dim-loaded* t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *dim->quantity-name-en-loaded* nil)

(defun load-dim->quantity-name-en ()
  (unless *dim->quantity-name-en-loaded*
    (flet ((dim->quantity-name-en (tbl) (mapc #'(lambda (el) (setf (gethash (vd-dims (nd-value el)) *dim->quantity-name-en*) (nd-quantity-name-en el))) (reverse tbl))))
      (mapc #'(lambda (el) (dim->quantity-name-en el)) (list *nd-si-derived-units-tbl-04* *nd-si-derived-units-tbl-03* *nd-si-derived-units-tbl-02* *nd-si-main-units*)))
    (print-hash-table *dim->quantity-name-en*)
    (setf *dim->quantity-name-en-loaded* t)))

(defparameter *quantity-name-en->dim-loaded* nil)

(defun load-quantity-name-en->dim ()
  (unless *quantity-name-en->dim-loaded*
    (flet ((string->dimension (tbl) (mapc #'(lambda (el) (setf (gethash  (nd-quantity-name-en el) *quantity-name-en->dim*) (vd-dims (nd-value el)))) (reverse tbl))))
      (mapc #'(lambda (el) (string->dimension el)) (list *nd-si-derived-units-tbl-04* *nd-si-derived-units-tbl-03* *nd-si-derived-units-tbl-02* *nd-si-main-units*)))
    (print-hash-table *quantity-name-en->dim*)
    (setf *quantity-name-en->dim-loaded* t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun load-si-units ()
  (load-nm-vl)
  (load-nm-vl-ru->en)
  (load-nm-vl-en->ru)
  (load-dim->unit-symbol-en)
  (load-unit-symbol-en->dim)
  (load-dim->unit-symbol-ru)
  (load-unit-symbol-ru->dim)
  (load-dim->quantity-name-en)
  (load-quantity-name-en->dim))

(load-si-units)
