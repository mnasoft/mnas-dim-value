;;;; defparameter.lisp

(in-package #:mnas-dim-value)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defparameter *nm-vl* (make-hash-table :test #'equal) "Задает соответствие сроки, обозначающей размерность значению.")

(defparameter *nm-vl-ru->en* (make-hash-table :test #'equal) "Задает соответствие сроки, обозначающей размерность значению.")
(defparameter *nm-vl-en->ru* (make-hash-table :test #'equal) "Задает соответствие сроки, обозначающей размерность значению.")

(defparameter *dim->unit-symbol-en* (make-hash-table :test #'equal) "Задает соответствие размерности величины сторке.")
(defparameter *dim->unit-symbol-ru* (make-hash-table :test #'equal) "Задает соответствие размерности величины сторке.")

(defparameter *unit-symbol-en->dim* (make-hash-table :test #'equal) "Задает соответствие строки обозначающей размернсть списку размерностей")
(defparameter *unit-symbol-ru->dim* (make-hash-table :test #'equal) "Задает соответствие строки обозначающей размернсть списку размерностей")

(defparameter *dim->quantity-name-en* (make-hash-table :test #'equal) "Задает соответствие размерности наименованию величины.")
(defparameter *quantity-name-en->dim* (make-hash-table :test #'equal) "Задает соответствие строки обозначающей размернсть списку размерностей")

(defparameter *d        "°"    "Градус плоского угла")

(defparameter *m        "'"    "Минута плоского угла")

(defparameter *s        "\""   "Секунда плоского угла")

(defparameter *sd       "□˚"   "Квадратный градус телесного угла")

(defparameter *angstrom "Å"    "Ангстрем" )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
