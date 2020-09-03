;;;; defparameter.lisp

(in-package #:mnas-dim-value)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(export '*nm-vl* )
(defvar *nm-vl* (make-hash-table :test #'equal) "Задает соответствие сроки, обозначающей размерность значению.")

(export '*nm-vl-ru->en* )
(defvar *nm-vl-ru->en* (make-hash-table :test #'equal) "Задает соответствие сроки, обозначающей размерность значению.")

(export '*nm-vl-en->ru* )
(defvar *nm-vl-en->ru* (make-hash-table :test #'equal) "Задает соответствие сроки, обозначающей размерность значению.")

(defvar *dim->unit-symbol-en* (make-hash-table :test #'equal) "Задает соответствие размерности величины сторке.")
(defvar *dim->unit-symbol-ru* (make-hash-table :test #'equal) "Задает соответствие размерности величины сторке.")



(defvar *unit-symbol-en->dim* (make-hash-table :test #'equal) "Задает соответствие строки обозначающей размернсть списку размерностей")
(defvar *unit-symbol-ru->dim* (make-hash-table :test #'equal) "Задает соответствие строки обозначающей размернсть списку размерностей")

(defvar *dim->quantity-name-en* (make-hash-table :test #'equal) "Задает соответствие размерности наименованию величины.")
(defvar *quantity-name-en->dim* (make-hash-table :test #'equal) "Задает соответствие строки обозначающей размернсть списку размерностей")

(defparameter *d        "°"    "Градус плоского угла")

(defparameter *m        "'"    "Минута плоского угла")

(defparameter *s        "\""   "Секунда плоского угла")

(defparameter *sd       "□˚"   "Квадратный градус телесного угла")

(defparameter *angstrom "Å"    "Ангстрем" )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
