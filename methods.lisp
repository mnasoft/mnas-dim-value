;;;; methods.lisp

(in-package #:mnas-dim-value)

;;;; (defmethod print-object :before ((x vd) s) (format s "#vd" ))

(defmethod print-object         ((x vd) o-s)
  (multiple-value-bind (dimens find) (gethash (d-lst x) *dimension->string*)
    (if find
	(format o-s "~S ~A" (vd-val x) dimens)
	(progn (format o-s "~S [" (vd-val x))
	   (mapc #'(lambda (no str)
		     (cond
		       ((= (nth no (d-lst x)) 1) (format o-s (concatenate 'string str " ")))
		       ((/= (nth no (d-lst x)) 0) (format o-s (concatenate 'string str "^~A ") (nth no (d-lst x))))))
		 '( 0    1   2   3   4    5     6     7    8)
		 '("m" "kg" "s" "A" "K" "cd" "mol" "rad" "sr"))
	   (format o-s "])")))))
"ToDo
Необходимо установить глобальную переменную для задания списка приоритетов при выводе
"

