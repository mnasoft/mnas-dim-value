;;;; ./src/method/method.lisp

(defpackage :mnas-dim-value/method
  (:use #:cl
        #:mnas-dim-value/func
        #:mnas-dim-value/class
        #:mnas-dim-value/mk-class
        #:mnas-dim-value/tbl
        #:mnas-hash-table
        #:mnas-dim-value/ht
        #:mnas-dim-value/generic
        )
  (:export dim->unit-symbol
           dimensionp
           )
  (:export vd~+                 ; Сложение
           vd~-                 ; Вычирание
           vd~*                 ; Умножение
           vd~/                 ; Деление
           vd~pow               ; Возведение в целочисленную степень
           vd~root              ; Извлечение корня степени
           vd~sqrt              ; Извлечение квадратного
           vd~exp
           vd~expt
           vd~ln
           vd~log
           vd~sin
           vd~cos
           vd~tan
           vd~asin
           vd~acos
           vd~atan
           vd~sinh
           vd~cosh
           vd~tanh
           vd~asinh
           vd~acosh
           vd~atanh
           vd~abs
           vd~equal
           vd~equalp
           )
  (:export print-object
           vd-print
           same-dimension
           vd-convert
           unit-name
           quantity-name
           )
  (:export angle-string)
  )


(in-package :mnas-dim-value/method)

(defun dim->unit-symbol ()
    (cond ((eq *vd-language* :ru) mnas-dim-value/ht-ru:*dim->unit-symbol*)
	  (t                      mnas-dim-value/ht-en:*dim->unit-symbol*)))

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

(defun dimensionp (str)
"@b(Описание:) функция dimensionp 
@begin[lang=lisp](code)
   (dimensionp \"m\")                 => 1 m
   (dimensionp (string-upcase \"t\")) => 1 T
   (dimensionp \"Pa\")                => 1 Pa
 ;;(dimensionp \"knot\")
@end(code)
"
  (multiple-value-bind (val find) (gethash str mnas-dim-value/ht-en:*nm->value*)
    (if find val nil)))

(let ((+format+ :f-01)
      (+unit-symbol+ '("m" "kg" "s" "A" "K" "cd" "mol" "rad" "sr"))
      )
  (defun vd-print-format (fmt)
    (setf +format+ fmt))

  (defmethod vd-print ((x <vd>) &optional (o-stream t) )
  (cond
    ((eq +format+ :f-01)
     (format o-stream "(vd ~S " (<vd>-val x))
     (loop :for u-s :in +unit-symbol+
           :for u-v :in (<vd>-dims x)
           :when (/= u-v 0)
             :do (format o-stream ":~A ~A " u-s u-v))
     (format o-stream ")")
     )
    
    (t
     (format o-stream "~S ~S" (<vd>-val x) (<vd>-dims x)))))
  )

;;;(vd-print-format :f-02)
;;;(vd-print (vd~* 101.325 1000.0 "Pa"))
;;;"m" "kg" "s" "A" "K" "cd" "mol" "rad" "sr"

(defmethod vd-print ((s string) &optional (o-stream t) &aux (x (vd-convert s)))
  (vd-print x o-stream))

(defmethod same-dimension ((x <vd>) (y <vd>))
  "Проверяет два числа с размерностью на совпадение"
  (equal (<vd>-dims x) (<vd>-dims y)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod vd-convert ((x <vd>))
  x)

(defmethod vd-convert ((x number))
  (vd x))

(defmethod vd-convert ((x string))
  (multiple-value-bind (val find) (gethash x mnas-dim-value/ht-en:*nm->value*)
    (if find
        val
        (progn
          (format t "~&Размерность ~S неизвестна: заменяю ~S -> ~S~%"
                  x x (vd 1.0))
          (vd 1.0)))))



(defun parse-float (str)
  (if (string= str "") 0
      (mnas-string/parse:read-number str)))

(defun parse-d-m-s (str)
  (cond
    ((string= "°" str) 1)
    ((string= "d" str) 1)
    ((string= "'" str) 1/60)
    ((string= "\"" str) 1/3600)
    (t 0)))

(defun degrees-minutes-seconds-to-radians (dms-string)
  (let ((regex "^([-+])?([0-9]*\\.?[0-9]*)([°d'\"]?)([0-9]*\\.?[0-9]*)([°d'\"]?)([0-9]*\\.?[0-9]*)([°d'\"]?)$"))
    (multiple-value-bind (match substrings)
        (cl-ppcre:scan-to-strings regex dms-string)
      (if match
          (let* ((a-sign (aref substrings 0))
                 (a-v1   (aref substrings 1))
                 (a-v2   (aref substrings 3))
                 (a-v3   (aref substrings 5))
                 (a-s1   (aref substrings 2))
                 (a-s2   (aref substrings 4))
                 (a-s3   (aref substrings 6))
                 (sign (cond
                         ((null a-sign) 1)
                         ((string= "+" a-sign)  1)
                         ((string= "-" a-sign) -1)
                         (t 1))))
            (loop :for v :in (list a-v1 a-v2 a-v3)
                  :as  s :in (list a-s1 a-s2 a-s3)
                  :summing (* (parse-float v) (parse-d-m-s s))
                    :into dms
                  :finally (return (* sign dms (/ pi 180)))))
          nil))))

(defmethod vd-convert ((x string))
  (let ((dms (degrees-minutes-seconds-to-radians x)))
    (when dms (return-from vd-convert (vd~* dms "rad"))))
  (multiple-value-bind (val find) (gethash x mnas-dim-value/ht-en:*nm->value*)
    (if find
        val
        (progn
          (format t "~&Размерность ~S неизвестна: заменяю ~S -> ~S~%"
                  x x (vd 1.0))
          (vd 1.0)))))


(defmethod vd-convert ((x null))
  (progn
    (format t "~&Размерность ~S неизвестна: заменяю ~S -> ~S~%"
            x x (vd 0.0))
    (vd 0.0)))

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
		       ((eq *vd-language* :en) +vd-names-en+)
		       ((eq *vd-language* :ru) +vd-names-ru+)))
	       (format o-s "]")))))
 
(defmethod quantity-name ((value <vd>) &key (vd-language *vd-language*))
  "Возвращает наименование величины.
Пример использования:

 @b(Пример использования:)
@begin[lang=lisp](code)
  (quantity-name (vd/ \"kg\" \"m\" \"m\" \"m\") :vd-language :en)
  => (\"density\" \"mass density\")
  (quantity-name (vd/ (vd* \"kg\" *g*) (vd-expt (vd* 0.01 \"m\") 2) 1000))
@end(code)

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
     (apply #'append *nd-list*))
    (remove-duplicates rez :test #'equal)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun vd~+ (&rest args)
  "Операция сложения для физических величин."
  (let* ((lst (mapcar #'vd-convert args))
         (val (apply #'+ (mapcar #'<vd>-val lst))))
    (make-instance '<vd>
                   :dims (copy-list (<vd>-dims (first lst)))
                   :val val)))

(defun vd~- (&rest args)
  "Возвращает список наименований величин, которым соответствует
размерность числа x (\"acceleration\")."
  (let* ((lst (mapcar #'vd-convert args))
         (val (apply #'- (mapcar #'<vd>-val lst))))
    (make-instance '<vd>
                   :dims (copy-list (<vd>-dims (first lst)))
                   :val val)))

(defmethod mult ((x <vd>) (y <vd>) )
  (let ((rez (vd 0)))
    (setf (<vd>-val rez) (* (<vd>-val x) (<vd>-val y))
	  (<vd>-dims rez) (mapcar #'+ (<vd>-dims x) (<vd>-dims y)))
    rez))

(defun vd~* (&rest args)
"Перемножение чисел с размерностью."
  (let* ((lst (mapcar #'vd-convert args))
         (val (apply #'* (mapcar #'<vd>-val lst)))
         (dim (mapcar #'<vd>-dims lst)))
    (make-instance '<vd>
                   :dims (apply #'mapcar #'+ dim)
                   :val val)))

(defun vd~/ (&rest args)
"Перемножение чисел с размерностью."
  (let* ((lst (mapcar #'vd-convert args))
         (val (apply #'/ (mapcar #'<vd>-val lst)))
         (dim (mapcar #'<vd>-dims lst)))
    (make-instance '<vd>
                   :dims (apply #'mapcar #'- dim)
                   :val val)))

(defun vd~pow (base power)
  "Возведение в целочисленную степень."
  (let* ((vd (vd-convert base))
         (val (<vd>-val  vd))
         (dim (<vd>-dims vd)))
    (make-instance '<vd>
                   :dims (mapcar
                          #'(lambda (el)
                              (if (/= power 0) (* el power) 0))
                          dim)
                   :val  (expt val power))))

(defun vd~root (radicand degree)
  "Извлечение корня."
  (let* ((vd (vd-convert radicand))
         (val (<vd>-val  vd))
         (dim (<vd>-dims vd)))
    (make-instance '<vd>
                   :dims (mapcar
                          #'(lambda (el)
                              (if (/= degree 0)
                                  (/ el degree)
                                  0))
                          dim)
                   :val  (expt val (/ degree)))))

(defun vd~sqrt (x)
  (vd~root x 2))

(defun vd~exp (power)
  "Экспонента."
  (let* ((vd (vd-convert power))
         (val (<vd>-val  vd))
         (dim (<vd>-dims vd)))
    (make-instance '<vd>
                   :dims (copy-list dim)
                   :val  (exp val))))

(defun vd~expt (base power)
  "Возведение в произвольную степень."
  (let* ((vd (vd-convert base))
         (val (<vd>-val  vd))
         (dim (<vd>-dims vd)))
    (make-instance '<vd>
                   :dims (copy-list dim)
                   :val  (expt val power))))

(defun vd~ln (x)
  "Натуратьный логарифм."
  (let* ((vd (vd-convert x))
         (val (<vd>-val  vd))
         (dim (<vd>-dims vd)))
    (make-instance '<vd>
                   :dims (copy-list dim)
                   :val  (log val))))

(defun vd~log (x base)
  "Логарифм по основанию."
  (let* ((vd  (vd-convert x))
         (val (<vd>-val  vd))
         (dim (<vd>-dims vd)))
    (make-instance '<vd>
                   :dims (copy-list dim)
                   :val  (log val base))))

(defun vd~sin (x)
  "Синус."
  (let* ((vd (vd-convert x))
         (val (<vd>-val  vd))
         (dim (<vd>-dims vd)))
    (make-instance '<vd>
                   :dims (copy-list dim)
                   :val  (sin val))))
(defun vd~cos (x)
  "Косинус."
  (let* ((vd (vd-convert x))
         (val (<vd>-val  vd))
         (dim (<vd>-dims vd)))
    (make-instance '<vd>
                   :dims (copy-list dim)
                   :val  (cos val))))

(defun vd~tan (x)
  "Тангенс."
  (let* ((vd (vd-convert x))
         (val (<vd>-val  vd))
         (dim (<vd>-dims vd)))
    (make-instance '<vd>
                   :dims (copy-list dim)
                   :val  (tan val))))

(defun vd~asin (x)
  "Арксинус."
  (let* ((vd (vd-convert x))
         (val (<vd>-val  vd))
         (dim (<vd>-dims vd)))
    (make-instance '<vd>
                   :dims (copy-list dim)
                   :val  (asin val))))

(defun vd~acos (x)
  "Арккосинус."
  (let* ((vd (vd-convert x))
         (val (<vd>-val  vd))
         (dim (<vd>-dims vd)))
    (make-instance '<vd>
                   :dims (copy-list dim)
                   :val  (acos val))))

(defun vd~atan (y &optional (x (vd 1)))
  "Арктангенс."
  (let* ((vd  (vd~/ y x))
         (val (<vd>-val  vd))
         (dim (<vd>-dims vd)))
    (make-instance '<vd>
                   :dims (copy-list dim)
                   :val  (atan val))))

(defun vd~sinh (x)
  "Синус гиперболический."
  (let* ((vd (vd-convert x))
         (val (<vd>-val  vd))
         (dim (<vd>-dims vd)))
    (make-instance '<vd>
                   :dims (copy-list dim)
                   :val  (sinh val))))
(defun vd~cosh (x)
  "Косинус гиперболический."
  (let* ((vd (vd-convert x))
         (val (<vd>-val  vd))
         (dim (<vd>-dims vd)))
    (make-instance '<vd>
                   :dims (copy-list dim)
                   :val  (cosh val))))

(defun vd~tanh (x)
  "Тангенс гиперболический."
  (let* ((vd (vd-convert x))
         (val (<vd>-val  vd))
         (dim (<vd>-dims vd)))
    (make-instance '<vd>
                   :dims (copy-list dim)
                   :val  (tanh val))))

(defun vd~asinh (x)
  "Арксинус гиперболический."
  (let* ((vd (vd-convert x))
         (val (<vd>-val  vd))
         (dim (<vd>-dims vd)))
    (make-instance '<vd>
                   :dims (copy-list dim)
                   :val  (asinh val))))

(defun vd~acosh (x)
    "Арккосинус гиперболический."
  (let* ((vd (vd-convert x))
         (val (<vd>-val  vd))
         (dim (<vd>-dims vd)))
    (make-instance '<vd>
                   :dims (copy-list dim)
                   :val  (acosh val))))

(defun vd~atanh (x)
    "Арктангенс гиперболический."
  (let* ((vd (vd-convert x))
         (val (<vd>-val  vd))
         (dim (<vd>-dims vd)))
    (make-instance '<vd>
                   :dims (copy-list dim)
                   :val  (atanh val))))

(defun vd~abs (x)
    "Абсолютное значение."
  (let* ((vd (vd-convert x))
         (val (<vd>-val  vd))
         (dim (<vd>-dims vd)))
    (make-instance '<vd>
                   :dims (copy-list dim)
                   :val  (abs val))))

(defun vd~equal (x y)
    "Абсолютное значение."
  (let ((x-vd (vd-convert x))
        (y-vd (vd-convert y)))
    (and (equal (<vd>-val  x-vd) (<vd>-val  y-vd))
         (equal (<vd>-dims  x-vd) (<vd>-dims  y-vd)))))

(defun vd~equalp (x y)
    "Абсолютное значение."
  (let ((x-vd (vd-convert x))
        (y-vd (vd-convert y)))
    (and (equalp (<vd>-val  x-vd) (<vd>-val  y-vd))
         (equalp (<vd>-dims  x-vd) (<vd>-dims  y-vd)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+nil
(defmethod print-object ((x <vd>) o-s)
  (progn (format o-s "~S " (<vd>-val x))
	       (let ((st+ nil)
		     (st- nil))
		 (map nil
		      #'(lambda (v d)
			  (cond
			    ((< 1  v) (push (format nil "~A^~A" d v      ) st+))
			    ((= 1  v) (push (format nil "~A"    d        ) st+))
     			    ((= v -1) (push (format nil "~A"    d        ) st-))
			    ((< v -1) (push (format nil "~A^~A" d (abs v)) st-))))
		      (<vd>-dims x) (vd-names))
		 (cond 
		   ((and st+ (null st-)) (format o-s "[~{~A~^*~}]"           (nreverse st+) ))
		   ((and st+ st-)        (format o-s "[~{~A~^*~}/~{~A~^*~}]" (nreverse st+) (nreverse st-)))
		   ((and (null st+) st-) (format o-s "[1/~{~A~^*~}]"         (nreverse st-)))))))

(defmethod print-object ((x <vd>) o-s)
  (multiple-value-bind (dimens find) (gethash (<vd>-dims x) (mnas-dim-value/method:dim->unit-symbol))
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

(defmethod print-object ((obj <nd>) o-s)
  (print-unreadable-object (obj o-s :type t :identity nil)
    (format o-s "~&~4t q-en:~A" (<nd>-quantity    obj))
    (format o-s "~&~4t u-en:~A" (<nd>-unit-name   obj))
    (format o-s "~&~4t s-en:~A" (<nd>-unit-symbol obj))
    (format o-s "~&~4t dim:~A"  (<nd>-dimension   obj))
    (format o-s "~&~4t val:~A"  (<nd>-value       obj))
    (format o-s "~&~4t c:~A"    (<nd>-coeff       obj))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(vd~/ (vd~* "20d" "N" "m") "rad")

(vd~/ (vd~* "20d" "N" "m") "rad")

(defconstant +d-s+  #\DEGREE_SIGN)
(defconstant +pr+   #\PRIME)
(defconstant +d-pr+ #\DOUBLE_PRIME)
(defconstant +t-pr+ #\TRIPLE_PRIME)
(defconstant +q-pr+ #\QUADRUPLE_PRIME)

(defun angle-string (angle &key (output :dms) (force-sign nil) (round 8))
  "@b(Описание:) функция|метод|обобщенная_функция| @b(angle-string)
 возвращает строку, представляющую угловую меру угла.

 @b(Переменые:)
@begin(list)
 @item(angle - угол, выраженный в радианах;)
 @item(output - определяет формат вывода:)
 @begin(list)
  @item(:d выводить значение в градусах; )
  @item(:dm - градусах и минутах; )
  @item(:dms - градусах минутах и секундах;)
  @item(:m - минутах;)
  @item(:ms - минутах и секундах;)
  @item(:s - секундах.)
 @end(list)
 @item(force-sign - при значении t указывает, что нужно принудительно
  выводить знак +, если угол положительный; не нужно выводить + при
  значении nil;)
 @item(round - сообщает сколько значащих цифр выводить:)
 @begin(list)
  @item(0, 1 или 2 - округлять до градусов;)
  @item(3 или 4 - округлять до минут или десятых и сотых долей градуса;)
  @item(5 или 6 - округлять до секунд или ее десятых и сотых минуты
   или тысячных и десятитысячных долей градуса;)
  @item(7 и более- округлять до десятых секунды)
 @end(list)
@end(list)

Целая часть младших разрядов выводятся в виде двух цифр например:
@begin(list)
 @item(41°09′02.56″;)
 @item(9′02.56″;)
 @item(2.56″.)
@end(list)
"
  (let* ((sign (cond
                 ((minusp angle) "-")
                 ((and force-sign (plusp angle)) "+")
                 (t "" )))
         (degrees (abs (/ (* angle 180) pi)))
         (minutes)
         (seconds)
         (pw)
         (d) (m) (s) (dd) (mm) (ss)
         (scale)
         (d-scaled)
         (m-scaled)
         (s-scaled)
         )
    (case output
      (:d
       (cond 
         ((< round 3)                   ; округляем до градусов
          (setf d (round degrees)
                m 0
                s 0)
          (format nil "~a~a~c" sign d +d-s+))
         ((> round 2)    ; округляем до определенного разряда градусов
          (setf pw (- round 2)
                scale (expt 10 pw)
                d-scaled (round (* scale degrees))
                d (truncate (/ d-scaled scale))
                dd (- d-scaled (* scale d))
                m 0
                s 0)
          (format nil (format nil "~a~a~a"  "~a~a.~" pw ",'0d~c") sign d dd +d-s+))))
      (:dm
       (cond 
         ((< round 3)                   ; округляем до градусов
          (setf d (round degrees)
                m 0
                s 0)
          (format nil "~a~a~c" sign d +d-s+))
         ((< round 5)                   ; округляем до минут
          (setf minutes (round (* 60 degrees))
                d (truncate minutes 60)
                m (- minutes (* 60 d))
                s 0)
          (format nil "~a~a~c~2,'0d~c" sign d +d-s+ m +pr+))
         ((> round 4)       ; округляем до определенного разряда минут
          (setf pw (- round 4)
                scale (expt 10 pw)
                m-scaled (round (* 60 scale degrees))
                d (truncate (/ m-scaled scale) 60)
                m (truncate (- (/ m-scaled scale) (* 60 d)))
                mm (- m-scaled (* 60 scale d) (* scale m))
                s 0)
          (format nil (format nil "~a~a~a"  "~a~a~c~2,'0d.~" pw ",'0d~c") sign d +d-s+ m mm +pr+))))
      (:dms
       (cond 
         ((< round 3)                   ; округляем до градусов
          (setf d (round degrees)
                m 0
                s 0)
          (format nil "~a~a~c" sign d +d-s+))
         ((< round 5)                   ; округляем до минут
          (setf minutes (round (* 60 degrees))
                d (truncate minutes 60)
                m (- minutes (* 60 d))
                s 0)
          (format nil "~a~a~c~2,'0d~c" sign d +d-s+ m +pr+))
         ((< round 7)                   ; округляем до секунд
          (setf seconds (round (* 3600 degrees))
                d (truncate seconds 3600)
                m (truncate (- seconds (* 3600 d)) 60)
                s (- seconds (* 3600 d) (* m 60)))
          (format nil "~a~a~c~2,'0d~c~2,'0d~c" sign d +d-s+ m +pr+ s +d-pr+))
         ((> round 6)       ; округляем до определенного разада секунд
          (setf pw (- round 6)
                scale (expt 10 pw)
                s-scaled (round (* 3600 scale degrees))
                d (truncate (/ s-scaled scale) 3600)
                m (truncate (- (/ s-scaled scale) (* 3600 d)) 60)
                s (truncate (- (/ s-scaled scale) (* 3600 d) (* m 60)))
                ss (- s-scaled (* 3600 scale d) (* scale m 60) (* s scale)))
          (format nil (format nil "~a~a~a"  "~a~a~c~2,'0d~c~2,'0d.~" pw ",'0d~c") sign d +d-s+ m +pr+ s ss +d-pr+))))
      (:ms
       (cond 
         ((< round 5)                   ; округляем до минут
          (setf minutes (round (* 60 degrees))
                d 0
                m (- minutes (* 60 d))
                s 0)
          (format nil "~a~a~c" sign m +pr+))
         ((< round 7)                   ; округляем до секунд
          (setf seconds (round (* 3600 degrees))
                d 0
                m (truncate (- seconds (* 3600 d)) 60)
                s (- seconds (* 3600 d) (* m 60)))
          (format nil "~a~a~c~2,'0D~c" sign m +pr+ s +d-pr+))
         ((> round 6)        ; округляем до определенного знака секунд
          (setf pw (- round 6)
                scale (expt 10 pw)
                s-scaled (round (* 3600 scale degrees))
                d 0
                m (truncate (- (/ s-scaled scale) (* 3600 d)) 60)
                s (truncate (- (/ s-scaled scale) (* 3600 d) (* m 60)))
                ss (- s-scaled (* 3600 scale d) (* scale m 60) (* s scale))
                )
          (format nil (format nil "~a~a~a"  "~a~a~c~2,'0d.~" pw ",'0d~c") sign m +pr+ s ss +d-pr+))))
      (:m
       (cond 
         ((< round 5)                   ; округляем до минут
          (setf minutes (round (* 60 degrees))
                d 0
                m (- minutes (* 60 d) )
                s 0)
          (format nil "~a~a~c" sign m +pr+))
         ((> round 4)         ; округляем до определенного знака минут
          (setf pw (- round 4)
                scale (expt 10 pw)
                m-scaled (round (* 60 scale degrees))
                d 0
                m (truncate (- (/ m-scaled scale) (* 60 d)))
                mm (- m-scaled (* scale m))
                s 0 )
          #+nil (format nil (format nil "~a~a~a"  "~A~," (- round 4) "f~C") sign m +pr+)
          (format nil (format nil "~a~a~a"  "~a~a.~" pw ",'0d~c") sign m mm +pr+)
          )))
      (:s
       (cond 
         ((< round 7)                   ; округляем до секунд
          (setf seconds (round (* 3600 degrees))
                d 0
                m 0
                s (- seconds (* 3600 d) (* m 60)))
          (format nil "~a~a~c" sign s +d-pr+))
         ((> round 6)        ; округляем до определенного знака секунд
          (setf pw (- round 6)
                scale (expt 10 pw)
                s-scaled (round (* 3600 scale degrees))
                d 0
                m 0
                s (truncate (- (/ s-scaled scale) (* 3600 d) (* m 60)))
                ss (- s-scaled (* 3600 scale d) (* scale m 60) (* s scale)))
          (format nil (format nil "~a~a~a"  "~a~a.~" pw ",'0d~c") sign s ss +d-pr+)))))))
