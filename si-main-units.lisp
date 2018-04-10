;;;; si-units-main.lisp

(in-package #:mnas-dim-value)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *si-main-units*
  (list
   (list "length"              "длина"	                       "L" "meter"    "метр"      "m"   "м"    |m|   )
;;;(list "mass"                "масса"                         "M" "kilogram" "килограмм" "kg"  "кг"   |kg|  )
   (list "mass"                "масса"                         "M" "kilogram" "килограмм" "g"   "г"    (vd* 1/1000 |kg|))
   (list "time"                "время"                         "T" "second"   "секунда"   "s"   "с"    |s|   )
   (list "electric current"    "сила тока электрического"      "I" "ampere"   "ампер"     "A"   "А"    |A|   )
   (list "temperature"         "температура термодинамическая" "Θ" "kelvin"   "кельвин"   "K"   "К"    |K|   )
   (list "amount of substance" "количество вещества"           "N" "mole"     "моль"      "mol" "моль" |mol| )
   (list "luminous intensity"  "сила света"                    "J" "candela"  "кандела"   "cd"  "кд"   |cd|  ))
  "si-main-units
Задает основные единицы измерения системы SI
Каждый подсписок состоит из следующих элементов:
1 - английксое наименование величины
2 - русское наименование величины
3 - размерность величины
4 - английское наименование единицы
5 - русское наименование единицы
6 - международное обозначение единицы
7 - русское обозначение единицы
см. ГОСТ 8.417-2002, таблица 1")

(defparameter *nd-si-main-units*
  (mapcar 
   #'(lambda (el)
       (make-instance 'nd
		      :quantity-name-en (first el)
		      :quantity-name-ru (second el)
		      :dimension-symbol (third el)
		      :unit-name-en     (fourth el)
		      :unit-name-ru     (fifth el)
		      :unit-symbol-en   (sixth el)
		      :unit-symbol-ru   (seventh el)
		      :value            (car (last el))
		      ))
   *si-main-units*))
