;;;; methods.lisp

(in-package #:mnas-dim-value)

;;;; (defmethod print-object :before ((x vd) s) (format s "#vd" ))

(defmethod print-object         ((x vd) o-s)
  (multiple-value-bind (dimens find) (gethash (vd-dims x) *dimension->string*)
    (if find
	(format o-s "~S ~A" (vd-val x) dimens)
	(progn (format o-s "~S [" (vd-val x))
	   (mapc #'(lambda (no str)
		     (cond
		       ((= (nth no (vd-dims x)) 1) (format o-s (concatenate 'string str " ")))
		       ((/= (nth no (vd-dims x)) 0) (format o-s (concatenate 'string str "^~A ") (nth no (vd-dims x))))))
		 '( 0    1   2   3   4    5     6     7    8)
		 '("m" "kg" "s" "A" "K" "cd" "mol" "rad" "sr"))
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

