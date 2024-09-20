;;;; mult-coeff.lisp

(defpackage :mnas-dim-value/ht
  (:use #:cl
        #:mnas-dim-value/func
        #:mnas-dim-value/class
        #:mnas-dim-value/tbl
        )
  (:export *nd-list*)
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

(defparameter *nd-list*
  (reverse
   (append
    *nd-table-2-si-base-units*
    *nd-table-4-the-22-si-units-with-special-names-and-symbols*
    *nd-table-5-examples-of-coherent-derived-units-in-the-si-expressed-in-terms-of-base-units*
    *nd-table-6-examples-of-si-coherent-derived-units-whose-names-and-symbols-include-si-coherent-derived-units-with-special-names-and-symbols*
    *nd-table-8-non-si-units-accepted-for-use-with-the-si-units*
    )))

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


(defun populate-m-coeff-ht ()
  "@b(Описание:) функция @b(populate-m-coeff-ht)
наполняет хеш-таблицы *m-coeff-en* *m-coeff-ru*
"
  (loop :for (power nm-ru nm-en s-ru s-en) :in *table-7-si-prefixes*
        :do
           (setf (gethash s-en *m-coeff-en*) (expt 10 power))
           (setf (gethash s-ru *m-coeff-ru*) (expt 10 power))))

(defun add-multiplid-values (var)
  (setf (gethash (<nd>-unit-symbol-en var) *nm-vl*)
	(<nd>-value var))
  (loop :for (prefix coeff) :in (hash-table->list *m-coeff-en*)
        :collect
        (when (is-in-range coeff (<nd>-coeff var))
          (setf
           (gethash (concatenate 'string
                                 prefix
                                 (<nd>-unit-symbol-en var))
                    *nm-vl*)
           (make-instance
            '<vd>
            :val (* (<vd>-val (<nd>-value var)) coeff)
            :dims (<vd>-dims (<nd>-value var)))))))

(defun load-nm-vl ()
  (unless *nm-vl-loaded*
    (mapc #'add-multiplid-values
	  (append
           *nd-table-2-si-base-units*
           *nd-table-4-the-22-si-units-with-special-names-and-symbols*))
    (print-hash-table *nm-vl*)
    (setf *nm-vl-loaded* t)))

(defun load-nm-vl-ru->en ()
  (unless *nm-vl-ru->en-loaded*
    (loop
      :for nd
        :in (list
             *nd-table-2-si-base-units*
             *nd-table-4-the-22-si-units-with-special-names-and-symbols*)
      :do
         (loop :for i :in nd :do
           (setf
            (gethash (<nd>-unit-symbol-ru i) *nm-vl-ru->en*)
            (<nd>-unit-symbol-en i))))
    (print-hash-table *nm-vl-ru->en*)
    (setf *nm-vl-ru->en-loaded* t)))

(defun load-nm-vl-en->ru ()
  (unless *nm-vl-en->ru-loaded*
    (loop
      :for nd
        :in (list
             *nd-table-2-si-base-units*
             *nd-table-4-the-22-si-units-with-special-names-and-symbols*)
      :do
         (loop :for i :in nd :do
           (setf
            (gethash (<nd>-unit-symbol-en i) *nm-vl-en->ru*)
            (<nd>-unit-symbol-ru i))))
    (print-hash-table *nm-vl-en->ru*)
    (setf *nm-vl-en->ru-loaded* t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(populate-m-coeff-ht)
(load-nm-vl)
(load-nm-vl-ru->en)
(load-nm-vl-en->ru)
