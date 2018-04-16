;;;; methods.lisp

(in-package #:mnas-dim-value)

;;;; (defmethod print-object :before ((x vd) s) (format s "#vd" ))

(defparameter *vd-language* :ru "Язык (member :en :ru)")

(defmethod print-object ((x vd) o-s)
  (multiple-value-bind (dimens find)
      (gethash (vd-dims x)
	       (cond
		 ((eq *vd-language* :en) *dim->unit-symbol-en*)
		 ((eq *vd-language* :ru) *dim->unit-symbol-ru*)))
    (if find
	(format o-s "~S ~A" (vd-val x) dimens)
	(progn (format o-s "~S [" (vd-val x))
	       (mapc #'(lambda (no str)
			 (cond
			   ((= (nth no (vd-dims x)) 1) (format o-s (concatenate 'string str " ")))
			   ((/= (nth no (vd-dims x)) 0) (format o-s (concatenate 'string str "^~A ") (nth no (vd-dims x))))))
		     '( 0    1   2   3   4    5     6     7    8)
		     (cond
		       ((eq *vd-language* :en) '("m" "kg" "s" "A" "K" "cd" "mol" "rad" "sr"))
		       ((eq *vd-language* :ru) '("м" "кг" "с" "А" "К" "кд" "моль" "рад" "ср"))))
	       (format o-s "])")))))

"ToDo
Необходимо установить глобальную переменную для задания списка приоритетов при выводе
"

(defmethod vd-print ((x vd) &optional (o-stream t) )
  "Метод печати внутреннего представления размерной величины"
  (format o-stream "~S ~S" (vd-val *a*) (vd-dims *a*)))


(defun dimensionp (str)
  (multiple-value-bind (val find) (gethash str *mult-nm-vl*)
    (if find val nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod print-object ((x nd) o-s)
  (format o-s "~S~%"
	  (list 'q-n-en (nd-quantity-name-en x)
		'q-n-ru (nd-quantity-name-ru x)
		'u-n-en (nd-unit-name-en     x)
		'u-n-ru (nd-unit-name-ru     x)
		'u-s-en (nd-unit-symbol-en   x)
		'u-s-ru (nd-unit-symbol-ru   x)
		'd-symb (nd-dimension-symbol x)
		'value  (nd-value            x))))
