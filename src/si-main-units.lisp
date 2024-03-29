;;;; si-main-units.lisp

(in-package :mnas-dim-value)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;@intern
(defparameter *si-main-units*
  (list
   (list "dimensionless"       "безразмерный"                  "U" "ul"       "бр"        "ul"  "бр"   (vd* 1))
   (list "length"              "длина"	                       "L" "meter"    "метр"      "m"   "м"    |m|    )
;;;(list "mass"                "масса"                         "M" "kilogram" "килограмм" "kg"  "кг"   |kg|   )
   (list "mass"                "масса"                         "M" "kilogram" "килограмм" "g"   "г"    (vd* 1/1000 |kg|))
   (list "time"                "время"                         "T" "second"   "секунда"   "s"   "с"    |s|    )
   (list "electric current"    "сила тока электрического"      "I" "ampere"   "ампер"     "A"   "А"    |A|    )
   (list "temperature"         "температура термодинамическая" "Θ" "kelvin"   "кельвин"   "K"   "К"    |K|    )
   (list "amount of substance" "количество вещества"           "N" "mole"     "моль"      "mol" "моль" |mol|  )
   (list "luminous intensity"  "сила света"                    "J" "candela"  "кандела"   "cd"  "кд"   |cd|   ))
  "Задает основные единицы измерения системы SI
Каждый подсписок состоит из следующих элементов:
@begin(enum)
@item(английксое наименование величины;)
@item(русское наименование величины;)
@item(размерность величины;)
@item(английское наименование единицы;)
@item(русское наименование единицы;)
@item(международное обозначение единицы;)
@item(русское обозначение единицы.)
@end(enum)
см. ГОСТ 8.417-2002, таблица 1")

;;@intern
(defparameter *nd-si-main-units* (make-nd-items    *si-main-units*)  "Задает основные единицы измерения системы SI.")

(setf (documentation  '*nd-si-main-units* 'variable)  (documentation  '*si-main-units* 'variable))

