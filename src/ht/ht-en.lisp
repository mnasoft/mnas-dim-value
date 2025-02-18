;;;; ./mnas-dim-value/src/ht/ht-en.lisp

(defpackage :mnas-dim-value/ht-en
  (:use #:cl
        #:mnas-hash-table
        #:mnas-dim-value/func
        #:mnas-dim-value/class
        #:mnas-dim-value/tbl-en ;;;; Main difference
        #:mnas-dim-value/ht/core
        )
  (:export *nd-named*
           *nd-list*
           )
  (:export *m-coeff*
           )
  (:export *nm->value*
           *nm-value-loaded*
           )
  (:export *dim->unit-symbol*
           *dim->unit-symbol-loaded*
           *unit-symbol->dim*
           *unit-symbol->dim-loaded*
           )
  (:export *dim->quantity*
           *dim->quantity-loaded*
           *quantity->dim*
           *quantity->dim-loaded*
           )
  (:export populate
           repopulate
           ))

(in-package :mnas-dim-value/ht-en)

(defparameter *nd-named* nil
  "Определяет величины, имеющие собственные названия.")

(defparameter *nd-list* nil
  "Определяет величины, имеющие и не имеющие собственные названия.")

(defvar *m-coeff* (make-hash-table :test 'equal)
  "Хеш- таблица *m-coeff* содержит международные множителные приставки
системы СИ

Ключами являются строки.
Значаниями являются числа.
Пример использования:
  (gethash \"M\" *m-coeff*) => 1000000
  (gethash \"\"  *m-coeff*) => 1
  (gethash \"m\" *m-coeff*) => 1/1000000")

(progn 
  (defparameter *nm-value-loaded* nil)
  (defparameter *dim->unit-symbol-loaded* nil)
  (defparameter *unit-symbol->dim-loaded* nil)
  (defparameter *dim->quantity-loaded* nil)
  (defparameter *quantity->dim-loaded* nil)

  (defvar *nm->value* (make-hash-table :test #'equal)
    "Задает соответствие сроки, обозначающей размерность, значению.

 @b(Пример использования:)
@begin[lang=lisp](code)
 (alexandria:hash-table-alist *nm->value*)
@end(code)")
  
  (defvar *dim->unit-symbol*
    (make-hash-table :test #'equal)
    "Задает соответствие размерности величины строке.")

  (defvar *unit-symbol->dim* (make-hash-table :test #'equal)
    "Задает соответствие строки обозначающей размернсть списку размерностей
@b(Пример использования:)
@begin[lang=lisp](code)
 (print-hash-table  *unit-symbol->dim*)
@end(code)")

  (defvar *dim->quantity*
    (make-hash-table :test #'equal)
    "Задает соответствие размерности наименованию величины.")

  (defvar *quantity->dim*
    (make-hash-table :test #'equal)
    "Задает соответствие строки обозначающей размернсть списку размерностей")
  )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun populate-nm->vl (m-coeff nm-vl)
  (unless *nm-value-loaded*
    (loop :for nd :in *nd-list* :do 
      (loop :for i :in nd :do
        (add-multiplid-values i m-coeff nm-vl)))
    (print-hash-table nm-vl)
    (setf *nm-value-loaded* t)))

(defun populate-dim->unit-symbol ()
  "
 @b(Пример использования:)
@begin[lang=lisp](code)
  (setf *dim->unit-symbol-loaded* nil)
@end(code)
"
  (unless *dim->unit-symbol-loaded*
    (loop :for nd :in *nd-list* :do
      (loop :for i :in nd :do
        (setf (gethash
               (<vd>-dims (<nd>-value i)) *dim->unit-symbol*)
              (<nd>-unit-symbol i))))
    (setf
     (gethash '(0 1 0 0 0 0 0 0 0) *dim->unit-symbol*)
     "kg")
    (print-hash-table *dim->unit-symbol*)
    (setf *dim->unit-symbol-loaded* t)))

(defun populate-unit-symbol->dim ()
  "
 @b(Пример использования:)
@begin[lang=lisp](code)
  (setf *unit-symbol->dim-loaded* nil)
@end(code)
"
  (unless *unit-symbol->dim-loaded*
    (loop :for nd :in *nd-list* :do
         (loop :for i :in nd :do
           (setf
            (gethash (<nd>-unit-symbol i) *unit-symbol->dim*)
            (<vd>-dims (<nd>-value i)))))
    (remhash "g" *unit-symbol->dim*)
    (setf
     (gethash "kg" *unit-symbol->dim*)
     '(0 1 0 0 0 0 0 0 0))
    (print-hash-table *unit-symbol->dim*)
    (setf *unit-symbol->dim-loaded* t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun populate-dim->quantity ()
  (unless *dim->quantity-loaded*
    "
 @b(Пример использования:)
@begin[lang=lisp](code)
  (setf *dim->quantity-loaded* nil)
@end(code)
"
    (loop :for nd :in *nd-list* :do
      (loop :for i :in nd :do
        (setf
         (gethash (<vd>-dims (<nd>-value i)) *dim->quantity*)
         (<nd>-quantity i))))
    (print-hash-table *dim->quantity*)
    (setf *dim->quantity-loaded* t)))

(defun populate-quantity->dim ()
  "
 @b(Пример использования:)
@begin[lang=lisp](code)
  (setf *quantity->dim-loaded* nil)
@end(code)
"
  (unless *quantity->dim-loaded*
    (loop :for nd :in *nd-list* :do
         (loop :for i :in nd :do
           (setf
            (gethash  (<nd>-quantity i) *quantity->dim*)
            (<vd>-dims (<nd>-value i)))))
    (print-hash-table *quantity->dim*)
    (setf *quantity->dim-loaded* t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun populate (table-7 m-coeff nm->value)
  (setf *nd-named*
        (reverse
         (list *table-2*
               *table-4*)))
  (setf *nd-list*
        (reverse
         (list *table-2*
               *table-4*
               *table-5*
               *table-6*
               *table-8*
               *table-9*)))
  (populate-m-coeff-ht table-7 m-coeff)
  (populate-nm->vl m-coeff nm->value)
  (populate-dim->unit-symbol)
  (populate-unit-symbol->dim)
  (populate-dim->quantity)
  (populate-quantity->dim))

(defun clean ()
  (loop :for i :in '(*nm-value-loaded*
                     *dim->unit-symbol-loaded*
                     *unit-symbol->dim-loaded* 
                     *dim->quantity-loaded*
                     *quantity->dim-loaded*)
        :do (set i nil))
  (loop :for i :in (list *m-coeff*
                         *nm->value*
                         *dim->unit-symbol*
                         *unit-symbol->dim*
                         *dim->quantity*
                         *quantity->dim*
                         )
        :do
           (clrhash i)))

(defun repopulate (table-7 m-coeff nm->value)
  (clean)
  (populate table-7 m-coeff nm->value))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(populate *table-7* *m-coeff* *nm->value*)

#+nil
(progn 
  (print-hash-table *nm->value*)
  (print-hash-table *dim->unit-symbol*)
  (print-hash-table *unit-symbol->dim*)
  (print-hash-table *dim->quantity*)
  (print-hash-table *quantity->dim*))

#+nil 
(repopulate mnas-dim-value/tbl-en:*table-7* *m-coeff*)
