(defpackage :mnas-dim-value/vars
  (:use #:cl)
  (:export <variable>
           <variable>-name
           <variable>-value
           <variable>-descr
           <variable>-validator
           )
  (:export <variable-set>
           <variable-set>-name
           <variable-set>-vars
           )
  (:export get-env
           descr-env
           set-env)
  (:export *variable-set*)
  )

(in-package :mnas-dim-value/vars)

(defclass <variable> ()
  ((name     :accessor <variable>-name :initarg :name   :initform "variable"
             :documentation "Имя переменной")
   (value    :accessor <variable>-value :initarg :value :initform nil
             :documentation "Значение переменной")
   (descr    :accessor <variable>-descr :initarg :descr :initform nil
             :documentation "Описание переменной")
   (validator :accessor <variable>-validator :initarg   :validator
              :initform '#(lambda (el) (declare (ignore el)) t)
             :documentation "Функция-валидатор с одним параметром"))
  (:documentation "Класс предназначен для хранения системной переменной."))

(defmethod print-object ((x <variable>) s)
  (print-unreadable-object (x s :type t)
    (format s "Name: ~A Value: ~S Descr: ~A~%Validator:~S"
            (<variable>-name x)
            (<variable>-value x)
            (<variable>-descr x)
            (<variable>-validator x))))
;;;;

(defclass <variable-set> ()
  ((name     :accessor <variable-set>-name :initarg :name  :initform "variable-set"
             :documentation "Имя для множества переменных")
   (vars     :accessor <variable-set>-vars :initarg :vars
             :initform (make-hash-table :test #'equal) 
             :documentation "Имя для множества переменных"))
  (:documentation "Класс предназначен для хранения множества системных переменных."))

(defmethod print-object ((x <variable-set>) s)
  (print-unreadable-object (x s :type t)
    (format s "Name: ~S~%"
            (<variable-set>-name x))
    (loop :for k :in (alexandria:hash-table-keys (<variable-set>-vars *variable-set*))
          :for v :in (alexandria:hash-table-values (<variable-set>-vars *variable-set*))
          :do (format s "~S = ~S~%" k (<variable>-value v)))))

;;;;

(defmethod insert ((variable <variable>) (variable-set <variable-set>))
  (setf
   (gethash
    (<variable>-name variable)
    (<variable-set>-vars variable-set))
   variable))

(defmethod insert ((variable <variable>) (variable-set <variable-set>))
  (setf
   (gethash
    (<variable>-name variable)
    (<variable-set>-vars variable-set))
   variable))

(defmethod set-variable (value (variable <variable>))
  (when (funcall (<variable>-validator variable) value)
    (setf (<variable>-value variable) value)))

(defmethod get-variable ((variable <variable>))
  (<variable>-value variable))

(defmethod get-env ((name string) (variable-set <variable-set>))
  (<variable>-value (gethash name (<variable-set>-vars *variable-set*))))

(defmethod descr-env ((name string) (variable-set <variable-set>))
  (<variable>-descr (gethash name (<variable-set>-vars *variable-set*))))

(defmethod set-env (value (name string) (variable-set <variable-set>))
  (set-variable value (gethash name (<variable-set>-vars variable-set))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
                 :name "SOLID-ANGLE"
                 :value :sr
                 :descr "Формат для печати телесных углов
(member :sr :sp :d :m :s :dms)
       - sp - спаты;
       - sr - стерадианы;
       - d - квадратные градусы;
       - m - квадратные минуты;
       - s - квадратные секунды.
"
                 :validator #'(lambda (el)
                                (member el '(:sr :sp :d :m :s))))
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

(insert (make-instance '<variable>
                       :name "SPECIFIC-ENERGY"
                       :value :J/kg
                       :descr "Формат для печати удельной энергии
(member :Sv :J/kg :Gy)
 - :Sv   - ;
 - :J/kg - ;
 - :Gy   - ."
                       :validator #'(lambda (el)
                                      (member el '(:year :mon :d :h :m :s :dhms :hms :ms))))
        *variable-set*)


*variable-set*
