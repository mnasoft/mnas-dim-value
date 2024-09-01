;;;; methods.lisp

(in-package :mnas-dim-value)



(defun dim->unit-symbol ()
    (cond ((eq *vd-language* :ru) *dim->unit-symbol-ru*)
	  (t                      *dim->unit-symbol-en*)))

(defmethod print-object ((x <vd>) o-s)
  (multiple-value-bind (dimens find) (gethash (<vd>-dims x) (dim->unit-symbol))
    (if find
	(format o-s "~S [~A]" (<vd>-val x) dimens)
	(progn (format o-s "~S " (<vd>-val x))
	       (let ((st+ nil)
		     (st- nil))
		 (map nil
		      #'(lambda (v d)
			  (cond
			    ((< 1  v) (push (format nil "~A^~A" d v) st+))
			    ((= 1  v) (push (format nil "~A"    d  ) st+))
     			    ((= v -1) (push (format nil "~A"    d  ) st-))
			    ((< v -1) (push (format nil "~A^~A" d v) st-))))
		      (<vd>-dims x) (vd-names))
		 (cond 
		   ((and st+ (null st-)) (format o-s "[~{~A~^*~}]"           (nreverse st+) ))
		   ((and st+ st-)        (format o-s "[~{~A~^*~}/~{~A~^*~}]" (nreverse st+) (nreverse st-)))
		   ((and (null st+) st-) (format o-s "[1/~{~A~^*~}]"         (nreverse st-)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; ToDo Необходимо установить глобальную переменную для задания списка приоритетов при выводе "
(defmethod print-object ((x <nd>) o-s)
  (format o-s "~S~%"
	  (list 'q-n-en (<nd>-quantity-name-en x)
		'q-n-ru (<nd>-quantity-name-ru x)
		'u-n-en (<nd>-unit-name-en     x)
		'u-n-ru (<nd>-unit-name-ru     x)
		'u-s-en (<nd>-unit-symbol-en   x)
		'u-s-ru (<nd>-unit-symbol-ru   x)
		'd-symb (<nd>-dimension-symbol x)
		'value  (<nd>-value            x)
		'coeff  (<nd>-coeff            x))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun dimensionp (str)
"@b(Описание:) функция dimensionp 
@begin[lang=lisp](code)
   (dimensionp \"m\")                 => 1 m
   (dimensionp (string-upcase \"t\")) => 1 T
   (dimensionp \"Pa\")                => 1 Pa
 ;;(dimensionp \"knot\")
@end(code)
"
  (multiple-value-bind (val find) (gethash str *nm-vl*)
    (if find val nil)))



(defmethod vd-print ((x <vd>) &optional (o-stream t) )
  (format o-stream "~S ~S" (<vd>-val x) (<vd>-dims x)))

(defmethod same-dimension ((x <vd>) (y <vd>))
  "Проверяет два числа с размерностью на совпадение"
  (equal (<vd>-dims x) (<vd>-dims y)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod vd-convert ((x <vd>))
  x)

(defmethod vd-convert ((x number))
  (vd x))

(defmethod vd-convert ((x string))
  (multiple-value-bind (val find) (gethash x *nm-vl*)
    (if find
        val
        (progn
          (format t "~&Размерность ~S неизвестна: заменяю ~S -> ~S~%"
                  x x (vd 1.0))
          (vd 1.0)))))

(defmethod vd-convert ((x null))
  (progn
    (format t "~&Размерность ~S неизвестна: заменяю ~S -> ~S~%"
            x x (vd 1.0))
    (vd 0.0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod mult ((x <vd>) (y <vd>) )
  (let ((rez (vd 0)))
    (setf (<vd>-val rez) (* (<vd>-val x) (<vd>-val y))
	  (<vd>-dims rez) (mapcar #'+ (<vd>-dims x) (<vd>-dims y)))
    rez))

(defun vd* (x &rest args)
"Перемножение чисел с размерностью."
  (let ((rez (vd-convert x)))
    (dolist (y args)
      (setf rez (mult rez (vd-convert y))))
    rez))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod div ((x <vd>) (y <vd>))
  (let ((rez (vd 0)))
    (setf (<vd>-val rez) (/ (<vd>-val x) (<vd>-val y))
	  (<vd>-dims rez) (mapcar #'- (<vd>-dims x) (<vd>-dims y)))
    rez))

(defun vd/ (x &rest args)
"Деление чисел с размерностью."
  (if args
      (let ((rez (vd-convert x)))
	(dolist (y args)
	  (setf rez (div rez (vd-convert y))))
	rez)
      (div (vd 1) (vd-convert x))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod sum ((x <vd>) (y <vd>) )
  (let ((rez (make-instance '<vd>)))
    (setf (<vd>-val rez) (+ (<vd>-val x) (<vd>-val y))
	  (<vd>-dims rez) (<vd>-dims x))
    rez))

(defun vd+ (x &rest args)
"Сложение чисел с размерностью."
  (let ((rez (vd-convert x)))
    (dolist (y args)
      (setf rez (sum rez (vd-convert y))))
    rez))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod diff ((x <vd>) (y <vd>))
  (let ((rez (make-instance '<vd>)))
    (setf (<vd>-val rez) (- (<vd>-val x) (<vd>-val y))
	  (<vd>-dims rez) (<vd>-dims x))
    rez))

(defun vd- (x  &rest args)
"Вычитание чисел с размерностью."
  (if args
    (let ((rez (vd-convert x)))
      (dolist (y args)
	(setf rez (diff rez (vd-convert y))))
      rez)
    (make-instance '<vd> :val (- (<vd>-val x)) :dims (<vd>-dims x))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod vd-expt (val (p number))
  (let ((x (vd-convert val)))
  (make-instance '<vd>
   :val (expt (<vd>-val x) p)
   :dims (mapcar  #'(lambda (el) (* el p)) (<vd>-dims x)))))

(defmethod vd-sqrt ((x <vd>))
  (make-instance '<vd>
   :val (sqrt (<vd>-val x))
   :dims (mapcar  #'(lambda (el) (/ el 2)) (<vd>-dims x))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod unit-name ((x <vd>) o-s)
  (multiple-value-bind (dimens find)
      (gethash (<vd>-dims x) (cond ((eq *vd-language* :ru) *dim->unit-symbol-ru*)
				 (t *dim->unit-symbol-en*)))
    (if find
	(format o-s "~A" dimens)
	(progn (format o-s "[" )
	       (mapc #'(lambda (no str)
			 (cond
			   ((= (nth no (<vd>-dims x)) 1) (format o-s (concatenate 'string str "")))
			   ((/= (nth no (<vd>-dims x)) 0) (format o-s (concatenate 'string str "^~A") (nth no (<vd>-dims x))))))
		     '( 0    1   2   3   4    5     6     7    8)
		     (cond
		       ((eq *vd-language* :en) '("m" "kg" "s" "A" "K" "cd" "mol"  "rad" "sr"))
		       ((eq *vd-language* :ru) '("м" "кг" "с" "А" "К" "кд" "моль" "рад" "ср"))))
	       (format o-s "]")))))

(defmethod quantity-name ((value <vd>) &key (vd-language *vd-language*))
  "Возвращает наименование величины.
Пример использования:
;;;; (quantity-name (vd/ |kg| |m| |m| |m|) :vd-language :en) => (\"density\" \"mass density\")
;;;; (quantity-name (vd/ (vd* |kg| |*g*|) (vd-expt (vd* 0.01 |m|) 2) 1000))
"
  (let ((rez nil)
	(item nil)
	(quantity-name-language
	 (cond
	   ((eq vd-language :en) #'<nd>-quantity-name-en)
	   ((eq vd-language :ru) #'<nd>-quantity-name-ru))))
    (mapc
     #'(lambda (el)
	 (when (equal (<vd>-dims (<nd>-value el))
		      (<vd>-dims value))
	   (setf item (funcall quantity-name-language el))
	   (cond
	     ((stringp item) (push item rez))
	     ((listp item) (setf rez (append item rez))))))
     *nd-list*)
    (remove-duplicates rez :test #'equal)))
