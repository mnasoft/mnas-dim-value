;;;; mnas-dim-value.lisp

(defpackage :mnas-dim-value/class
  (:use       #:cl )
  (:export <vd>
           <vd>-val
           <vd>-dims)
  (:export <nd>
           <nd>-quantity
           <nd>-unit-name
           <nd>-unit-symbol
           <nd>-dimension
           <nd>-value
           <nd>-coeff
           )
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
  (:export *vd-language*
           vd-names
           +vd-names-en+
           +vd-names-ru+
  ))

(in-package :mnas-dim-value/class)

(defparameter *vd-language* :en
  "Язык (member :en :ru)")

(defvar +vd-names-en+ '("m" "kg" "s" "A" "K" "cd" "mol"  "rad" "sr"))

(defvar +vd-names-ru+ '("м" "кг" "с" "А" "К" "кд" "моль" "рад" "ср"))

(defun vd-names ()
  (cond
    ((eq *vd-language* :ru) +vd-names-ru+)
    (t                      +vd-names-en+)))

(defclass <vd> ()
  ((val      :accessor <vd>-val      :initarg :val  :initform 0.0
             :documentation "Численное значение величины")
   (dims     :accessor <vd>-dims     :initarg :dims :initform '(0 0 0  0 0 0  0 0 0)
             :documentation "Список степеней размерности. Степени размерности
 находятся в порядке, соответствующем порядку из переменной +vd-names-en+"))
  (:documentation "Число с размерностью (ЧсР)."))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass <nd> ()
  ((quantity      :accessor <nd>-quantity       :initarg :quantity-name :initform ""
                  :documentation "Наименование величины английское. Например: length")
   (unit-name     :accessor <nd>-unit-name      :initarg :unit-name     :initform ""
                  :documentation "Наименование единицы английское. Например: metre") 
   (unit-symbol   :accessor <nd>-unit-symbol    :initarg :unit-symbol   :initform ""
                  :documentation "Обозначение единицы английское. Например: m")
   (dimension     :accessor <nd>-dimension      :initarg :dimension     :initform ""
                  :documentation "Символ размерности. Например: L")
   (value         :accessor <nd>-value          :initarg :value         :initform 1
                  :documentation "Значение, выраженное в единицах СИ. Например: (vd 1 :m 1)")
   (coeff         :accessor <nd>-coeff          :initarg :coeff         :initform '((-24 24))
                  :documentation "Список диапазонов разрешенных степеней множителей для данной величины системы СИ"))
  (:documentation "Величина с размерностью. Данный класс служит исключительно для
удобства в определении чисел с размерностью."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;;;;

(defclass <variable-set> ()
  ((name     :accessor <variable-set>-name :initarg :name  :initform "variable-set"
             :documentation "Имя для множества переменных")
   (vars     :accessor <variable-set>-vars :initarg :vars
             :initform (make-hash-table :test #'equal) 
             :documentation "Имя для множества переменных"))
  (:documentation "Класс предназначен для хранения множества системных переменных."))
