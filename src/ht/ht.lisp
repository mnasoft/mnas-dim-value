;;;; mult-coeff.lisp

(defpackage :mnas-dim-value/ht
  (:use #:cl
        #:mnas-dim-value/func
        #:mnas-dim-value/class
        #:mnas-dim-value/tbl
        #:mnas-hash-table
        )
  (:export *nd-named*
           *nd-list*)
  (:export *m-coeff-en*
           *m-coeff-ru*)
  (:export prefix-from->to)
  (:export *nm-vl*)
  (:export *nm-vl-ru->en*
           *nm-vl-en->ru*)
  (:export *dim->unit-symbol-en*
           *unit-symbol-en->dim*)
  (:export *dim->unit-symbol-ru*
           *unit-symbol-ru->dim*)
  (:export *dim->quantity-name-en*
           *quantity-name-en->dim*)
  (:export *dim->quantity-name-ru*
           *quantity-name-ru->dim*)  
  (:export *nm-vl-loaded*
           *nm-vl-ru->en-loaded* 
           *nm-vl-en->ru-loaded* 
           *dim->unit-symbol-en-loaded* 
           *unit-symbol-en->dim-loaded* 
           *dim->unit-symbol-ru-loaded* 
           *unit-symbol-ru->dim-loaded* 
           *dim->quantity-name-en-loaded* 
           *quantity-name-en->dim-loaded*)
  (:export load-si-units
           ))

(in-package :mnas-dim-value/ht)

(defparameter *nd-named*
  (reverse
   (list *nd-table-2-si-base-units*
         *nd-table-4-the-22-si-units-with-special-names-and-symbols*)))

(defparameter *nd-list*
  (reverse
   (list
    *nd-table-2-si-base-units*
    *nd-table-4-the-22-si-units-with-special-names-and-symbols*
    *nd-table-5-examples-of-coherent-derived-units-in-the-si-expressed-in-terms-of-base-units*
    *nd-table-6-examples-of-si-coherent-derived-units-whose-names-and-symbols-include-si-coherent-derived-units-with-special-names-and-symbols*
    *nd-table-8-non-si-units-accepted-for-use-with-the-si-units*)))

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

(progn 
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
  )
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
  (loop :for (prefix coeff) :in (to-list *m-coeff-en*)
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

(defun load-nm-vl ()
  (unless *nm-vl-loaded*
    (loop :for nd :in *nd-list* :do ; ToDo здесь вероятное не *nd-named* *nd-list*
      (loop :for i :in nd :do
        (add-multiplid-values i)))
    (print-hash-table *nm-vl*)
    (setf *nm-vl-loaded* t)))

(defun load-nm-vl-ru->en ()
  (unless *nm-vl-ru->en-loaded*
    (loop :for nd :in *nd-named* :do
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

(defun load-dim->unit-symbol-en ()
  "
 @b(Пример использования:)
@begin[lang=lisp](code)
  (setf *dim->unit-symbol-en-loaded* nil)
@end(code)
"
  (unless *dim->unit-symbol-en-loaded*
    (loop :for nd :in *nd-list* :do
      (loop :for i :in nd :do
        (setf (gethash
               (<vd>-dims (<nd>-value i)) *dim->unit-symbol-en*)
              (<nd>-unit-symbol-en i))))
    (setf
     (gethash '(0 1 0 0 0 0 0 0 0) *dim->unit-symbol-en*)
     "kg")
    (print-hash-table *dim->unit-symbol-en*)
    (setf *dim->unit-symbol-en-loaded* t)))

(defun load-unit-symbol-en->dim ()
  "
 @b(Пример использования:)
@begin[lang=lisp](code)
  (setf *unit-symbol-en->dim-loaded* nil)
@end(code)
"
  (unless *unit-symbol-en->dim-loaded*
    (loop :for nd :in *nd-list* :do
         (loop :for i :in nd :do
           (setf
            (gethash (<nd>-unit-symbol-en i) *unit-symbol-en->dim*)
            (<vd>-dims (<nd>-value i)))))
    (remhash "g" *unit-symbol-en->dim*)
    (setf
     (gethash "kg" *unit-symbol-en->dim*)
     '(0 1 0 0 0 0 0 0 0))
    (print-hash-table *unit-symbol-en->dim*)
    (setf *unit-symbol-en->dim-loaded* t)))

(defun load-dim->unit-symbol-ru ()
  "
 @b(Пример использования:)
@begin[lang=lisp](code)
  (setf *dim->unit-symbol-ru-loaded* nil)
@end(code)
"
  (unless *dim->unit-symbol-ru-loaded*
    (loop :for nd :in *nd-list* :do
      (loop :for i :in nd :do
        (setf (gethash
               (<vd>-dims (<nd>-value i)) *dim->unit-symbol-ru*)
              (<nd>-unit-symbol-ru i))))
    (setf
     (gethash '(0 1 0 0 0 0 0 0 0) *dim->unit-symbol-ru*)
     "кг")
    (print-hash-table *dim->unit-symbol-ru*)
    (setf *dim->unit-symbol-ru-loaded* t)))

(defun load-unit-symbol-ru->dim ()
  "
 @b(Пример использования:)
@begin[lang=lisp](code)
  (setf *unit-symbol-ru->dim-loaded* nil)
@end(code)
"
  (unless *unit-symbol-ru->dim-loaded*
    (loop :for nd :in *nd-list* :do
      (loop :for i :in nd :do
        (setf
         (gethash (<nd>-unit-symbol-ru i) *unit-symbol-ru->dim*)
         (<vd>-dims (<nd>-value i)))))
    (remhash "г" *unit-symbol-ru->dim*)
    (setf
     (gethash "кг" *unit-symbol-ru->dim*)
     '(0 1 0 0 0 0 0 0 0))
    (print-hash-table *unit-symbol-ru->dim*)
    (setf *unit-symbol-ru->dim-loaded* t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun load-dim->quantity-name-en ()
  (unless *dim->quantity-name-en-loaded*
    "
 @b(Пример использования:)
@begin[lang=lisp](code)
  (setf *dim->quantity-name-en-loaded* nil)
@end(code)
"
    (loop :for nd :in *nd-list* :do
      (loop :for i :in nd :do
        (setf
         (gethash (<vd>-dims (<nd>-value i)) *dim->quantity-name-en*)
         (<nd>-quantity-name-en i))))
    (print-hash-table *dim->quantity-name-en*)
    (setf *dim->quantity-name-en-loaded* t)))

(defun load-quantity-name-en->dim ()
  "
 @b(Пример использования:)
@begin[lang=lisp](code)
  (setf *quantity-name-en->dim-loaded* nil)
@end(code)
"
  (unless *quantity-name-en->dim-loaded*
    (loop :for nd :in *nd-list* :do
         (loop :for i :in nd :do
           (setf
            (gethash  (<nd>-quantity-name-en i) *quantity-name-en->dim*)
            (<vd>-dims (<nd>-value i)))))
    (print-hash-table *quantity-name-en->dim*)
    (setf *quantity-name-en->dim-loaded* t)))

#+nil (defun load-dim->quantity-name-ru ())
#+nil (defun load-quantity-name-ru->dim ())
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun load-si-units ()
  (populate-m-coeff-ht)
  (load-nm-vl)
  (load-nm-vl-ru->en)
  (load-nm-vl-en->ru)
  (load-dim->unit-symbol-en)
  (load-unit-symbol-en->dim)
  (load-dim->unit-symbol-ru)
  (load-unit-symbol-ru->dim)
  (load-dim->quantity-name-en)
  (load-quantity-name-en->dim)
  )

(load-si-units)
