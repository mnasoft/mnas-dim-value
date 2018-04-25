;;;; mnas-dim-value.lisp

(in-package #:mnas-dim-value)

;;; "mnas-dim-value" goes here. Hacks and glory await!

;;;;m kg s A K cd mol rad sr

(defclass vd ()
  ((val   :accessor vd-val  :initarg :val  :initform 0.0                        :documentation "Численное значение величины")
   (dims  :accessor vd-dims :initarg :dims :initform (list 0 0 0  0 0 0  0 0 0) :documentation "Список степеней размерности"))
  (:documentation "Число с размерностью."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass nd ()
  ((quantity-name-en :accessor nd-quantity-name-en  :initarg :quantity-name-en :initform "" :documentation "Наименование величины английское. Например: length")
   (quantity-name-ru :accessor nd-quantity-name-ru  :initarg :quantity-name-ru :initform "" :documentation "Наименование величины русское. Например: длина")
   (unit-name-en     :accessor nd-unit-name-en      :initarg :unit-name-en     :initform "" :documentation "Наименование единицы английское. Например: metre") 
   (unit-name-ru     :accessor nd-unit-name-ru      :initarg :unit-name-ru     :initform "" :documentation "Наименование единицы русское. Например: метр") 
   (unit-symbol-en   :accessor nd-unit-symbol-en    :initarg :unit-symbol-en   :initform "" :documentation "Обозначение единицы английское. Например: m")
   (unit-symbol-ru   :accessor nd-unit-symbol-ru    :initarg :unit-symbol-ru   :initform "" :documentation "Обозначение единицы русское. Например: м")
   (dimension-symbol :accessor nd-dimension-symbol  :initarg :dimension-symbol :initform "" :documentation "Символ размерности. Например: L")
   (value            :accessor nd-value             :initarg :value            :initform 1  :documentation "Значение, выраженное в единицах СИ. Например: (vd 1 :m 1)"))
  (:documentation "Число с размерностью."))
