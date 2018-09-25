;;;; methods.lisp

(in-package #:mnas-dim-value)

;;;; (defmethod print-object :before ((x vd) s) (format s "#vd" ))

(defparameter *vd-language* :en "Язык (member :en :ru)")

(defmethod print-object ((x vd) o-s)
  (multiple-value-bind (dimens find)
      (gethash (vd-dims x) (cond ((eq *vd-language* :ru) *dim->unit-symbol-ru*)
				 (t *dim->unit-symbol-en*)))
    (if find
	(format o-s "~S ~A" (vd-val x) dimens)
	(progn (format o-s "~S [" (vd-val x))
	       (mapc #'(lambda (no str)
			 (cond
			   ((= (nth no (vd-dims x)) 1) (format o-s (concatenate 'string str "")))
			   ((/= (nth no (vd-dims x)) 0) (format o-s (concatenate 'string str "^~A") (nth no (vd-dims x))))))
		     '( 0    1   2   3   4    5     6     7    8)
		     (cond
		       ((eq *vd-language* :en) '("m" "kg" "s" "A" "K" "cd" "mol"  "rad" "sr"))
		       ((eq *vd-language* :ru) '("м" "кг" "с" "А" "К" "кд" "моль" "рад" "ср"))))
	       (format o-s "]")))))

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
		'value  (nd-value            x)
		'coeff  (nd-coeff            x))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

"ToDo
Необходимо установить глобальную переменную для задания списка приоритетов при выводе
"

(defun dimensionp (str)
  (multiple-value-bind (val find) (gethash str *nm-vl*)
    (if find val nil)))

(defun vd (x &key (m 0) (kg 0) (s 0) (A 0) (K 0) (cd 0) (mol 0) (rad 0) (sr 0))
  (make-instance 'vd :val x :dims (list m kg s A K cd mol rad sr) ))

(defmethod vd-print ((x vd) &optional (o-stream t) )
  "Метод печати внутреннего представления размерной величины"
  (format o-stream "~S ~S" (vd-val x) (vd-dims x)))

(defmethod same-dimension ((x vd) (y vd))
  "Проверяет два числа с размерностью на совпадение"
  (equal (vd-dims x) (vd-dims y)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defgeneric vd-convert (obj))

(defmethod vd-convert ((x vd)) x)

(defmethod vd-convert ((x number)) (vd x))

(defmethod vd-convert ((x string)) (gethash x *nm-vl*)) 


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod mult ((x vd) (y vd) )
  (let ((rez (vd 0)))
    (setf (vd-val rez) (* (vd-val x) (vd-val y))
	  (vd-dims rez) (mapcar #'+ (vd-dims x) (vd-dims y)))
    rez))

(defun vd* (x &rest args)
  (let ((rez (vd-convert x)))
    (dolist (y args)
      (setf rez (mult rez (vd-convert y))))
    rez))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod div ((x vd) (y vd) )
  (let ((rez (vd 0)))
    (setf (vd-val rez) (/ (vd-val x) (vd-val y))
	  (vd-dims rez) (mapcar #'- (vd-dims x) (vd-dims y)))
    rez))

(defun vd/ (x &rest args)
  (if args
      (let ((rez (vd-convert x)))
	(dolist (y args)
	  (setf rez (div rez (vd-convert y))))
	rez)
      (div (vd 1) (vd-convert x))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod sum ((x vd) (y vd) )
  (let ((rez (make-instance 'vd)))
    (setf (vd-val rez) (+ (vd-val x) (vd-val y))
	  (vd-dims rez) (vd-dims x))
    rez))

(defun vd+ (x &rest args)
  (let ((rez (vd-convert x)))
    (dolist (y args)
      (setf rez (sum rez (vd-convert y))))
    rez))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod diff ((x vd) (y vd) )
  (let ((rez (make-instance 'vd)))
    (setf (vd-val rez) (- (vd-val x) (vd-val y))
	  (vd-dims rez) (vd-dims x))
    rez))

(defun vd- (x  &rest args)
  (if args
    (let ((rez (vd-convert x)))
      (dolist (y args)
	(setf rez (diff rez (vd-convert y))))
      rez)
    (make-instance 'vd :val (- (vd-val x)) :dims (vd-dims x))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod vd-expt (val (p number))
  (let ((x (vd-convert val)))
  (make-instance
   'vd
   :val (expt (vd-val x) p)
   :dims (mapcar  #'(lambda (el) (* el p)) (vd-dims x)))))

(defmethod vd-sqrt ((x vd))
  (make-instance
   'vd
   :val (sqrt (vd-val x))
   :dims (mapcar  #'(lambda (el) (/ el 2)) (vd-dims x))))

