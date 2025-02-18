;;;; ./mnas-dim-value/src/ht/ht-core.lisp

(defpackage :mnas-dim-value/ht/core
  (:use #:cl
        #:mnas-dim-value/func
        #:mnas-dim-value/class
        #:mnas-hash-table
        )
  (:export populate-m-coeff-ht
           prefix-from->to
           add-multiplid-values
           ))

(in-package :mnas-dim-value/ht/core)

(defun populate-m-coeff-ht (table-7 m-coeff-ht)
  "@b(Описание:) функция @b(populate-m-coeff-ht)
наполняет хеш-таблицу m-coeff-ht данными из таблицы table-7.

 @b(Пример использования:)
@begin[lang=lisp](code)
 (populate-m-coeff-ht mnas-dim-value/tbl-en:*table-7* mnas-dim-value/ht-en::*m-coeff-en*)
@end(code)"
  (loop :for (base power unit-name unit-symbol) :in table-7
        :do
           (setf (gethash unit-symbol m-coeff-ht) (expt base power))))


(defun prefix-from->to (x str-prefix-from str-prefix-to m-coeff)
  "Перевод значения числа х, предваряемого приставкой str-prefix-from, в
число с приставкой str-prefix-to.

 @b(Пример использования:)
@begin[lang=lisp](code)
 (prefix-from->to 5.5 \"M\" \"k\" mnas-dim-value/ht-en:*m-coeff*) => 5500.0
 (prefix-from->to 5.5 \"\" \"k\"  mnas-dim-value/ht-en:*m-coeff*) => 0.0055
 (prefix-from->to 5.5 \"\" \"\"   mnas-dim-value/ht-en:*m-coeff*) => 5.5
@end(code)"
  (* x (/ (gethash str-prefix-from m-coeff)
	  (gethash str-prefix-to   m-coeff))))

(defun add-multiplid-values (var m-coeff nm-vl)
  (setf (gethash (<nd>-unit-symbol var) nm-vl)
	(<nd>-value var))
  (loop :for (prefix coeff) :in (to-list m-coeff)
        :collect
        (when (is-in-range coeff (<nd>-coeff var))
          (setf
           (gethash (concatenate 'string
                                 prefix
                                 (<nd>-unit-symbol var))
                    nm-vl)
           (make-instance
            '<vd>
            :val (* (<vd>-val (<nd>-value var)) coeff)
            :dims (<vd>-dims (<nd>-value var)))))))
