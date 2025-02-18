(defpackage :mnas-dim-value/generic
  (:use
   #:cl )
  (:export vd-convert
           )
  (:export same-dimension
           unit-name
           quantity-name 
           ))

(in-package :mnas-dim-value/generic)

(defgeneric vd-convert (obj)
  (:documentation
   "@b(Описание:) обобщенная_функция @b(vd-convert) перобразует объект obj
в объект класса <vd> и возвращает преобразованный объект класса <vd>.
"))

(defgeneric same-dimension (x y)
  (:documentation
   "@b(Описание:) обобщенная_функция @b(same-dimension) возвращает T, если
x и y имеют одинаковую размерность, и NIL в проивном случае.")
  )

(defgeneric unit-name (x)
  (:documentation
   "Возвращает наименование размерности."))

(defgeneric quantity-name (x &key vd-language)
  (:documentation
   "Возвращает список наименований величин, которым соответствует
размерность числа x (\"acceleration\")."))
