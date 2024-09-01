;;;; mult-coeff.lisp

(defpackage :mnas-dim-value/ht
  (:use #:cl
        #:mnas-dim-value/func
        #:mnas-dim-value/class)
  (:export *m-coeff-en*
           *m-coeff-ru*)
  (:export prefix-from->to)
  (:export *nm-vl*
           *nm-vl-ru->en*
           *nm-vl-en->ru*
           *dim->unit-symbol-en*
           *dim->unit-symbol-ru*
           *unit-symbol-en->dim*
           *unit-symbol-ru->dim*
           *dim->quantity-name-en*
           *quantity-name-en->dim*)
  (:export *nm-vl-loaded*
           *nm-vl-ru->en-loaded* 
           *nm-vl-en->ru-loaded* 
           *dim->unit-symbol-en-loaded* 
           *unit-symbol-en->dim-loaded* 
           *dim->unit-symbol-ru-loaded* 
           *unit-symbol-ru->dim-loaded* 
           *dim->quantity-name-en-loaded* 
           *quantity-name-en->dim-loaded* ))

(in-package :mnas-dim-value/ht)

(defvar *m-coeff-en* (make-hash-table :test 'equal)
  "Хеш- таблица *m-coeff-en* содержит международные множителные приставки
системы СИ

Ключами являются строки.
Значаниями являются числа.
Пример использования:
  (gethash \"M\" *m-coeff-en*) => 1000000
  (gethash \"m\" *m-coeff-en*) => 1/1000000")

(defvar *m-coeff-ru* (make-hash-table :test 'equal)
    "Хеш- таблица *m-coeff-en* содержит международные множителные приставки системы СИ 
Ключами являются строки.
Значаниями являются числа.
Пример использования:
  (gethash \"М\" *m-coeff-ru*) => 1000000
  (gethash \"м\" *m-coeff-ru*) => 1/1000000")

(defun prefix-from->to (x str-prefix-from str-prefix-to)
  "Перевод значения числа х, предваряемого приставкой str-prefix-from, в
число с приставкой str-prefix-to.

Пример использования:
5.5 ΜPa -> 5500 kPa
;;;; (prefix-from->to 5.5 \"M\" \"k\")=> 5500.0
;;;; (prefix-from->to 5.5 \"\" \"k\") => 0.0055
;;;; (prefix-from->to 5.5 \"\" \"\") => 1.0"
  (* x (/ (gethash str-prefix-from *m-coeff-en*)
	  (gethash str-prefix-to   *m-coeff-en*))))

(defparameter *nm-vl-loaded* nil)

(defparameter *nm-vl-ru->en-loaded* nil)

(defparameter *nm-vl-en->ru-loaded* nil)

(defparameter *dim->unit-symbol-en-loaded* nil)

(defparameter *unit-symbol-en->dim-loaded* nil)

(defparameter *dim->unit-symbol-ru-loaded* nil)

(defparameter *unit-symbol-ru->dim-loaded* nil)

(defparameter *dim->quantity-name-en-loaded* nil)

(defparameter *quantity-name-en->dim-loaded* nil)

(defvar *nm-vl*
  (make-hash-table :test #'equal)
  "Задает соответствие сроки, обозначающей размерность значению.")

(defvar *nm-vl-ru->en*
  (make-hash-table :test #'equal)
  "Задает соответствие сроки, обозначающей размерность значению.")

(defvar *nm-vl-en->ru*
  (make-hash-table :test #'equal)
  "Задает соответствие сроки, обозначающей размерность значению.")

(defvar *dim->unit-symbol-en*
  (make-hash-table :test #'equal)
  "Задает соответствие размерности величины сторке.")

(defvar *dim->unit-symbol-ru*
  (make-hash-table :test #'equal)
  "Задает соответствие размерности величины сторке.")

(defvar *unit-symbol-en->dim*
  (make-hash-table :test #'equal)
  "Задает соответствие строки обозначающей размернсть списку размерностей")

(defvar *unit-symbol-ru->dim*
  (make-hash-table :test #'equal)
  "Задает соответствие строки обозначающей размернсть списку размерностей")

(defvar *dim->quantity-name-en*
  (make-hash-table :test #'equal)
  "Задает соответствие размерности наименованию величины.")

(defvar *quantity-name-en->dim*
  (make-hash-table :test #'equal)
  "Задает соответствие строки обозначающей размернсть списку размерностей")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


