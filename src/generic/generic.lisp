(defpackage :mnas-dim-value/generic
  (:use       #:cl )
  (:export    vd-convert
              vd-print
              same-dimension
              vd-convert
              mult
              div
              sum
              diff
              vd-expt
              vd-sqrt
              unit-name
              quantity-name 
              ))

(in-package :mnas-dim-value/generic)

(defgeneric vd-convert (obj))

(defgeneric vd-print (vd &optional stream)
  (:documentation
   "Метод печати внутреннего представления размерной величины"))

(defgeneric same-dimension (x y))

(defgeneric vd-convert (x)
  )

(defgeneric mult (x y))

(defgeneric div (x y))

(defgeneric sum (x y))

(defgeneric diff (x y))

(defgeneric vd-expt (x power)
  (:documentation
   "Возведение числа x в степень power.")
  )

(defgeneric vd-sqrt (x)
  (:documentation
   "Извлечение из числа с размерностью квадратного корня."))

(defgeneric unit-name (x stream)
  (:documentation
   "Возвращает наименование размерности."))

(defgeneric quantity-name (x &key vd-language)
  (:documentation
   "Возвращает список наименований величин, которым соответствует
размерность числа x (\"acceleration\")."))


