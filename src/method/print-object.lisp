;;;; ./src/method/print-object.lisp

(in-package :mnas-dim-value/method)

(defparameter *variable-set* (make-instance '<variable-set>))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(insert (make-instance '<variable>
                 :name "LANGUAGE"
                 :value :en
                 :descr "Определяет язык вывода"
                 :validator #'(lambda (el)
                                (member el '(:en :ru :uk))))
        *variable-set*)

(insert (make-instance '<variable>
                 :name "SI"
                 :value :adaptive
                 :descr "Формат для печати единиц измерения в виде перемножения единиц SI.
(member :force :adaptive)
 - :force    - вывод в виде перемножения единиц SI;
 - :adaptive - выполнять адаптацию."
                 :validator #'(lambda (el)
                                (member el '(:force :adaptive))))
        *variable-set*)

(insert (make-instance '<variable>
                 :name "ANGLE"
                 :value :dms
                 :descr "Формат для печати углов
(member :rad :grad :rot :dms :dm :d :ms :m :s)
 - :rad  - вывод в радианах (круг=2π);
 - :grad - вывод в градах (круг=400 град);
 - :rot  - вывод в оборотах;
 - :dms  - например: 9°07′06.56″;
 - :dm   - например: 9°07.1093′;
 - :d    - например: 9.118489°;
 - :ms   - например: 547′06.56″;
 - :m    - например: 547.1093′;
 - :s    - например: 32826.56″."
                 :validator #'(lambda (el)
                                (member el '(:rad :grad :rot :dms :dm :d :ms :m :s))))
        *variable-set*)

(insert (make-instance '<variable>
                 :name "AUNITS"
                 :value 8
                 :descr "Примерное количество значащих цифр в представлении угловых величин."
                 :validator #'(lambda (el)
                                (and (integerp el) (>= el 0))))
        *variable-set*)
(insert (make-instance '<variable>
                 :name "UNITS"
                 :value 6
                 :descr "Примерное количество значащих цифр в представлении угловых величин."
                 :validator #'(lambda (el)
                                (and (integerp el) (>= el 0))))
        *variable-set*)

(insert (make-instance '<variable>
                       :name "TIME"
                       :value :dhms
                       :descr "Формат для печати интервалов времени
(member :year :mon :d :h :m :s :dhms :hms :ms)
 - :year - годы = 365.25 суток;
 - :mon  - месяцы = (/ 365.25 12) суток;
 - :d    - сутки;
 - :h    - часы;
 - :m    - минуты;
 - :s    - секунды.
 - :dhms - сутки, часы, минуты секунды;
 - :hms  - часы, минуты, секунды;
 - :ms   - минуты, секунды."
                       :validator #'(lambda (el)
                                      (member el '(:year :mon :d :h :m :s :dhms :hms :ms))))
        *variable-set*)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; <nd>
(defmethod print-object ((obj <nd>) o-s)
  (print-unreadable-object (obj o-s :type t :identity nil)
    (format o-s "~&~4t q-en:~A" (<nd>-quantity    obj))
    (format o-s "~&~4t u-en:~A" (<nd>-unit-name   obj))
    (format o-s "~&~4t s-en:~A" (<nd>-unit-symbol obj))
    (format o-s "~&~4t dim:~A"  (<nd>-dimension   obj))
    (format o-s "~&~4t val:~A"  (<nd>-value       obj))
    (format o-s "~&~4t c:~A"    (<nd>-coeff       obj))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; <vd>

(defconstant +d-s+  #\DEGREE_SIGN)
(defconstant +pr+   #\PRIME)
(defconstant +d-pr+ #\DOUBLE_PRIME)
(defconstant +t-pr+ #\TRIPLE_PRIME)
(defconstant +q-pr+ #\QUADRUPLE_PRIME)


(defun angle-string (angle &key (output (get-env "ANGLE" *variable-set*)) (force-sign nil) (a-units (get-env "AUNITS" *variable-set*)))
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
 @item(a-units - сообщает сколько значащих цифр выводить:)
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
      (:rad
       (format nil (format nil "~a~a~a"  "~," a-units "f [rad]") angle))
      (:rot
       (format nil (format nil "~a~a~a"  "~," a-units "f [rot]") (/ angle 2 pi))) 
      (:grad
       (format nil (format nil "~a~a~a"  "~," (max 0 (- a-units 2)) "f [grad]") (* (/ 200 pi) angle)))
      (:d
       (cond 
         ((< a-units 3)                   ; округляем до градусов
          (setf d (round degrees)
                m 0
                s 0)
          (format nil "~a~a~c" sign d +d-s+))
         ((> a-units 2)    ; округляем до определенного разряда градусов
          (setf pw (- a-units 2)
                scale (expt 10 pw)
                d-scaled (round (* scale degrees))
                d (truncate (/ d-scaled scale))
                dd (- d-scaled (* scale d))
                m 0
                s 0)
          (format nil (format nil "~a~a~a"  "~a~a.~" pw ",'0d~c") sign d dd +d-s+))))
      (:dm
       (cond 
         ((< a-units 3)                   ; округляем до градусов
          (setf d (round degrees)
                m 0
                s 0)
          (format nil "~a~a~c" sign d +d-s+))
         ((< a-units 5)                   ; округляем до минут
          (setf minutes (round (* 60 degrees))
                d (truncate minutes 60)
                m (- minutes (* 60 d))
                s 0)
          (format nil "~a~a~c~2,'0d~c" sign d +d-s+ m +pr+))
         ((> a-units 4)       ; округляем до определенного разряда минут
          (setf pw (- a-units 4)
                scale (expt 10 pw)
                m-scaled (round (* 60 scale degrees))
                d (truncate (/ m-scaled scale) 60)
                m (truncate (- (/ m-scaled scale) (* 60 d)))
                mm (- m-scaled (* 60 scale d) (* scale m))
                s 0)
          (format nil (format nil "~a~a~a"  "~a~a~c~2,'0d.~" pw ",'0d~c") sign d +d-s+ m mm +pr+))))
      (:dms
       (cond 
         ((< a-units 3)                   ; округляем до градусов
          (setf d (round degrees)
                m 0
                s 0)
          (format nil "~a~a~c" sign d +d-s+))
         ((< a-units 5)                   ; округляем до минут
          (setf minutes (round (* 60 degrees))
                d (truncate minutes 60)
                m (- minutes (* 60 d))
                s 0)
          (format nil "~a~a~c~2,'0d~c" sign d +d-s+ m +pr+))
         ((< a-units 7)                   ; округляем до секунд
          (setf seconds (round (* 3600 degrees))
                d (truncate seconds 3600)
                m (truncate (- seconds (* 3600 d)) 60)
                s (- seconds (* 3600 d) (* m 60)))
          (format nil "~a~a~c~2,'0d~c~2,'0d~c" sign d +d-s+ m +pr+ s +d-pr+))
         ((> a-units 6)       ; округляем до определенного разада секунд
          (setf pw (- a-units 6)
                scale (expt 10 pw)
                s-scaled (round (* 3600 scale degrees))
                d (truncate (/ s-scaled scale) 3600)
                m (truncate (- (/ s-scaled scale) (* 3600 d)) 60)
                s (truncate (- (/ s-scaled scale) (* 3600 d) (* m 60)))
                ss (- s-scaled (* 3600 scale d) (* scale m 60) (* s scale)))
          (format nil (format nil "~a~a~a"  "~a~a~c~2,'0d~c~2,'0d.~" pw ",'0d~c") sign d +d-s+ m +pr+ s ss +d-pr+))))
      (:ms
       (cond 
         ((< a-units 5)                   ; округляем до минут
          (setf minutes (round (* 60 degrees))
                d 0
                m (- minutes (* 60 d))
                s 0)
          (format nil "~a~a~c" sign m +pr+))
         ((< a-units 7)                   ; округляем до секунд
          (setf seconds (round (* 3600 degrees))
                d 0
                m (truncate (- seconds (* 3600 d)) 60)
                s (- seconds (* 3600 d) (* m 60)))
          (format nil "~a~a~c~2,'0D~c" sign m +pr+ s +d-pr+))
         ((> a-units 6)        ; округляем до определенного знака секунд
          (setf pw (- a-units 6)
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
         ((< a-units 5)                   ; округляем до минут
          (setf minutes (round (* 60 degrees))
                d 0
                m (- minutes (* 60 d) )
                s 0)
          (format nil "~a~a~c" sign m +pr+))
         ((> a-units 4)         ; округляем до определенного знака минут
          (setf pw (- a-units 4)
                scale (expt 10 pw)
                m-scaled (round (* 60 scale degrees))
                d 0
                m (truncate (- (/ m-scaled scale) (* 60 d)))
                mm (- m-scaled (* scale m))
                s 0 )
          #+nil (format nil (format nil "~a~a~a"  "~A~," (- a-units 4) "f~C") sign m +pr+)
          (format nil (format nil "~a~a~a"  "~a~a.~" pw ",'0d~c") sign m mm +pr+)
          )))
      (:s
       (cond 
         ((< a-units 7)                   ; округляем до секунд
          (setf seconds (round (* 3600 degrees))
                d 0
                m 0
                s (- seconds (* 3600 d) (* m 60)))
          (format nil "~a~a~c" sign s +d-pr+))
         ((> a-units 6)        ; округляем до определенного знака секунд
          (setf pw (- a-units 6)
                scale (expt 10 pw)
                s-scaled (round (* 3600 scale degrees))
                d 0
                m 0
                s (truncate (- (/ s-scaled scale) (* 3600 d) (* m 60)))
                ss (- s-scaled (* 3600 scale d) (* scale m 60) (* s scale)))
          (format nil (format nil "~a~a~a"  "~a~a.~" pw ",'0d~c") sign s ss +d-pr+)))))))

;;;;

(defparameter +days-per-year+ 36525/100)
(defparameter +seconds-per-year+
  (* +days-per-year+ local-time:+seconds-per-day+))
(defparameter +seconds-per-month+
  (* local-time:+seconds-per-day+
   (/ +days-per-year+ local-time:+months-per-year+)))

(defun time-string (time-in-seconds
                    &key
                      (output (get-env "TIME" *variable-set*))
                      (units (get-env "UNITS" *variable-set*)))
  (let ((time (abs time-in-seconds)))
    (case output
      (:year (format nil (format nil "~a~a~a"  "~," (max 0 units) "f [year]") (/ time +seconds-per-year+)))
      (:mon  (format nil (format nil "~a~a~a"  "~," (max 0 (- units 1)) "f [month]") (/ time +seconds-per-month+)))
      (:d    (format nil (format nil "~a~a~a"  "~," (max 0 (- units 2)) "f [day]") (/ time local-time:+seconds-per-day+)))
      (:h    (format nil (format nil "~a~a~a"  "~," (max 0 (- units 3)) "f [hour]") (/ time local-time:+seconds-per-hour+)))
      (:m    (format nil (format nil "~a~a~a"  "~," (max 0 (- units 4)) "f [minutes]") (/ time local-time:+seconds-per-minute+)))
      (:s    (format nil (format nil "~a~a~a"  "~," (max 0 (- units 5)) "f [seconds]") time))
      (:dhms
       (let* ((d-s (multiple-value-list (truncate time local-time:+seconds-per-day+)))
              (h-s (multiple-value-list (truncate (second d-s) local-time:+seconds-per-hour+)))
              (m-s (multiple-value-list (truncate (second h-s) local-time:+seconds-per-minute+))))
         (format nil
                 (format nil "~a~a~a"  "~D [day] ~D [hour] ~D [minutes] ~," (max 0 (- units 5)) "f [seconds]")
                 (first d-s) (first h-s) (first m-s) (second m-s))))
      (:hms
       (let* ((h-s (multiple-value-list (truncate time local-time:+seconds-per-hour+)))
              (m-s (multiple-value-list (truncate (second h-s) local-time:+seconds-per-minute+))))
         (format nil
                 (format nil "~a~a~a"  "~D [hour] ~D [minutes] ~," (max 0 (- units 5)) "f [seconds]")
                 (first h-s) (first m-s) (second m-s))))
      (:ms (let* ((m-s (multiple-value-list (truncate time local-time:+seconds-per-minute+))))
         (format nil
                 (format nil "~a~a~a" "~D [minutes] ~," (max 0 (- units 5)) "f [seconds]")
                  (first m-s) (second m-s)))))))

;;;;

(defmethod print-object ((x <vd>) o-s)
  (labels ((foo (x)
             (format o-s "~S " (<vd>-val x))
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
		 ((and (null st+) st-) (format o-s "[1/~{~A~^*~}]"         (nreverse st-)))))))
    (cond
      ((eq :force (get-env "SI" *variable-set*))
       (foo x))
      ((and (eq :adaptive (get-env "SI" *variable-set*))
            (same-dimension (vd-convert "rad") x))
       (format o-s "~A" (angle-string (<vd>-val x) :output (get-env "ANGLE" *variable-set*) :a-units (get-env "AUNITS" *variable-set*))))
      ((and (eq :adaptive (get-env "SI" *variable-set*))
            (same-dimension (vd-convert "s") x))
       (format o-s "~A" (time-string (<vd>-val x) :output (get-env "TIME" *variable-set*) :units (get-env "UNITS" *variable-set*))))
      (and (eq :adaptive (get-env "SI" *variable-set*))
           (same-dimension (vd~/ "J" "kg") x))
      (t
       (multiple-value-bind (dimens find) (gethash (<vd>-dims x) (dim->unit-symbol))
         (if find
	     (format o-s "~S [~A]" (<vd>-val x) dimens)
	     (foo x)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; <variable>

(defmethod print-object ((x <variable>) s)
  (print-unreadable-object (x s :type t)
    (format s "Name: ~A Value: ~S Descr: ~A~%Validator:~S"
            (<variable>-name x)
            (<variable>-value x)
            (<variable>-descr x)
            (<variable>-validator x))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; <variable-set>

(defmethod print-object ((x <variable-set>) s)
  (print-unreadable-object (x s :type t)
    (format s "Name: ~S~%"
            (<variable-set>-name x))
    (loop :for k :in (alexandria:hash-table-keys (<variable-set>-vars *variable-set*))
          :for v :in (alexandria:hash-table-values (<variable-set>-vars *variable-set*))
          :do (format s "~S = ~S~%" k (<variable>-value v)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(get-env "LANGUAGE" *variable-set*)
(get-env "ANGLE"    *variable-set*)

(set-env :force "SI" *variable-set*)
(set-env :adaptive "SI" *variable-set*)
(descr-env "SI"    *variable-set*)
(set-env :dm "ANGLE" *variable-set*)
(vd-convert "30°20′22.5″")

(set-env :dm "ANGLE" *variable-set*)
(set-env 8 "UNITS" *variable-set*)

(vd~/ "J" "kg") (vd~* "Sv")


(quantity-name (vd~/ "J" "kg"))

(unit-name (vd~/ "J" "kg"))


