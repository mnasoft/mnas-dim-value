;;;; mnas-dim-value.lisp

(defpackage :mnas-dim-value/class
  (:use       #:cl
              #:mnas-dim-value/vars
              )
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
  (:export 
           vd-names
           +vd-names-en+
           +vd-names-ru+
           )
  (:export vd-m
           vd-kg
           vd-s
           vd-A
           vd-K
           vd-cd
           vd-mol
           vd-rad
           vd-sr
   ))

(in-package :mnas-dim-value/class)

(defvar +vd-names-en+ '("m" "kg" "s" "A" "K" "cd" "mol"  "rad" "sr"))

(defvar +vd-names-ru+ '("м" "кг" "с" "А" "К" "кд" "моль" "рад" "ср"))

mnas-dim-value/vars:*variable-set*

(defun vd-names ()
  (cond
    ((eq (get-env "LANGUAGE" *variable-set*) :ru) +vd-names-ru+)
    (t                      +vd-names-en+)))

(defclass <vd> ()
  ((val      :accessor <vd>-val      :initarg :val  :initform 0.0
             :documentation "Численное значение величины")
   (dims     :accessor <vd>-dims     :initarg :dims :initform '(0 0 0  0 0 0  0 0 0)
             :documentation "Список степеней размерности. Степени размерности
 находятся в порядке, соответствующем порядку из переменной +vd-names-en+"))
  (:documentation "Число с размерностью (ЧсР)."))

(defmethod vd-m   ((vd <vd>)) (nth 0 (<vd>-dims vd)))
(defmethod (setf vd-m) (m (vd <vd>)) (setf (nth 0 (<vd>-dims vd)) m) vd)
  
(defmethod vd-kg  ((vd <vd>)) (nth 1 (<vd>-dims vd)))
(defmethod (setf vd-kg) (kg (vd <vd>)) (setf (nth 1 (<vd>-dims vd)) kg) vd)

(defmethod vd-s   ((vd <vd>)) (nth 2 (<vd>-dims vd)))
(defmethod (setf vd-s) (s (vd <vd>)) (setf (nth 2 (<vd>-dims vd)) s) vd)
  
(defmethod vd-A   ((vd <vd>)) (nth 3 (<vd>-dims vd)))
(defmethod (setf vd-A) (A (vd <vd>)) (setf (nth 3 (<vd>-dims vd)) A) vd)

(defmethod vd-K   ((vd <vd>)) (nth 4 (<vd>-dims vd)))
(defmethod (setf vd-K) (K (vd <vd>)) (setf (nth 4 (<vd>-dims vd)) K) vd)

(defmethod vd-cd  ((vd <vd>)) (nth 5 (<vd>-dims vd)))
(defmethod (setf vd-cd) (cd (vd <vd>)) (setf (nth 5 (<vd>-dims vd)) cd) vd)

(defmethod vd-mol ((vd <vd>)) (nth 6 (<vd>-dims vd)))
(defmethod (setf vd-mol) (mol (vd <vd>)) (setf (nth 6 (<vd>-dims vd)) mol) vd)

(defmethod vd-rad ((vd <vd>)) (nth 7 (<vd>-dims vd)))
(defmethod (setf vd-rad) (rad (vd <vd>)) (setf (nth 7 (<vd>-dims vd)) rad) vd)

(defmethod vd-sr  ((vd <vd>)) (nth 8 (<vd>-dims vd)))
(defmethod (setf vd-sr) (sr (vd <vd>)) (setf (nth 8 (<vd>-dims vd)) sr) vd)


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


