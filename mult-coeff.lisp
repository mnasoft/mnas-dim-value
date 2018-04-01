;;;; mult-coeff.lisp

(in-package #:mnas-dim-value)

(defparameter *mult-prefix*
  '(
    (24  "йотта" "Yotta" "И"  "Y")
    (21  "зетта" "Zetta" "З"  "Z")
    (18  "экса"  "Exa"   "Э"  "E")
    (15  "пета"  "Peta"  "П"  "P")
    (12  "тера"  "Tera"  "Т"  "T")
    (9 	 "гига"  "Giga"  "Г"  "G")
    (6 	 "мега"  "Mega"  "М"  "M")
    (3 	 "кило"  "kilo"  "к"  "k")
    (2 	 "гекто" "hecto" "г"  "h")
    (1 	 "дека"  "deca"  "да" "da")
;;; (0   ""      ""      ""   "")
    (-1  "деци"  "deci"  "д"  "d")
    (-2  "санти" "centi" "с"  "c")
    (-3  "милли" "milli" "м"  "m")
    (-6  "микро" "micro" "мк" "μ")
    (-9  "нано"  "nano"  "н"  "n")
    (-12 "пико"  "pico"  "п"  "p")
    (-15 "фемто" "femto" "ф"  "f")
    (-18 "атто"  "atto"  "а"  "a")
    (-21 "зепто" "zepto" "з"  "z")
    (-24 "йокто" "yocto" "и"  "y"))
  "*muti-prefix-data* содержит множителные приставки;
Каждый подсписок содержит описание одной множительной приставки в следующем формате:
1 - степень в которую необходимо возвести 10 для раскрытия приставки;
2 - наименование множителя русское;
3 - наименование множителя международное;
4 - обозначение множителя русское;
5 - обозначение множителя международное;")


(progn
  (defparameter *mut-prefix* (make-hash-table :test 'equal))
  (mapc #'(lambda (el)
	    (setf (gethash (fifth el) *mut-prefix*)
		  (expt 10 (first el))))
	*mult-prefix*))

(progn
  (defparameter *mut-prefix-ru* (make-hash-table :test 'equal))
  (mapc #'(lambda (el)
	    (setf (gethash (fourth el) *mut-prefix-ru*)
		  (expt 10 (first el))))
	*mult-prefix*))

(defun prefix-from->to(x str-prefix-from str-prefix-to)
  "Перевод значения числа х, предваряемого приставкой str-prefix-from,
в число с приставкой str-prefix-to
Пример использования:
5.5 ΜPa -> 5500 kPa
;;;; (prefix-from->to 5.5 \"M\" \"k\")=> 5500.0
;;;; (prefix-from->to 5.5 \"\" \"k\") => 0.0055
;;;; (prefix-from->to 5.5 \"\" \"\") => 1.0"
  (* x (/ (gethash str-prefix-from *mut-prefix*)
	  (gethash str-prefix-to   *mut-prefix*))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
