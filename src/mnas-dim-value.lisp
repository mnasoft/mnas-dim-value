;;;; /src/mnas-dim-value.lisp

(defpackage :mnas-dim-value
  (:nicknames "MDV")
  (:use #:cl
        #:mnas-dim-value/func
        #:mnas-dim-value/class
        #:mnas-dim-value/mk-class
        #:mnas-dim-value/tbl
        #:mnas-hash-table
        #:mnas-dim-value/ht
        #:mnas-dim-value/generic
        #:mnas-dim-value/method
        #:mnas-dim-value/const
        #:mnas-dim-value/macro
        )
  (:export <vd>
           <vd>-val
           <vd>-dims)
  (:export vd
           )
  (:export vd+ vd- vd* vd/
           )
  (:export vd-expt
           vd-sqrt
           )
  (:export |m| |kg| |s| |A| |K| |cd| |mol|
           |rad| |sr|
           )
  (:export |Hz| |N| |Pa| |J| |W|
           |C| |V| |F| |Ω| |S| |Wb|
           |Τ| |H| |lm| |lx| |Bq|
           |Gy| |Sv| |kat|
           )
;;; From :mnas-dim-value/const
  (:export |*g*| |*Gn*| |*C-0*| |*V-0*|
           |*R-0*| |*Na*| |*No*|
           |*k*| |*a-e-m*|
           |*m-e*| |*e*|
           |*F*| |*h*| |*c*| |*μ-0*| |*ε-0*|
           )
;;; From :mnas-dim-value/convert  
  (:export C->K K->C 
           M->K K->M 
           KGS/CM2->PA
           PA->KGS/CM2
           )
  (:export quantity
           )

  (:export 
   HELP
   
   DIM-STRING-BY-DIM-NAME
   DIM-NAME-LIST

   UNUSE-MNAS-DIM-VALUE

   USE-MNAS-DIM-VALUE

   QUANTITY-NAME
   ))

(in-package :mnas-dim-value)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun dim-string-by-dim-name ( d-type )
  "Пример использования:
;;;;(mnas-dim-value:dim-string-by-dim-name \"length\")
;;;;(mnas-dim-value:dim-string-by-dim-name \"specific entropy\")
;;;;(mnas-dim-value:dim-string-by-dim-name \"capacitance\")
;;;;(mnas-dim-value:dim-string-by-dim-name \"mass density\")
" 
  (loop :for i :in mnas-dim-value/tbl:*nd-tables*
        :when (string= (<nd>-quantity-name-en i) d-type)
        :collect (<nd>-unit-symbol-en i)
        ))


(dim-string-by-dim-name "length")

mnas-dim-value/class:*vd-language*


(defun dim-name-list ()
  "Возвращает список наименований величин
Пример использования 
;;;;(dim-name-list) 
;;;;(dim-name-list :en-ru #'second)
"
  (let ((q-name
          (if (eq mnas-dim-value/class:*vd-language* :ru) 
              #'<nd>-quantity-name-ru
              #'<nd>-quantity-name-en
              )))
    (loop :for i :in mnas-dim-value/tbl:*nd-tables*
          :append
          (loop :for part :in (cl-ppcre:split "," (funcall q-name i))
                :collect (string-trim '(#\Space #\Tab) part))
            :into rez
          :finally (return (delete-duplicates (sort rez #'string<) :test #'equal)))))

(setf mnas-dim-value/class:*vd-language* :ru)
(dim-name-list)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *help*
  " 
 (in-package :mnas-dim-value)                         ;;;; Загрузка пакета
 (quantity 220 \"V\" 15 \"A\")                            ;;;; Пример 1
 (quantity 101325 \"Pa\" + ( 2.0 *g* / (1 * \"cm\" ^ 2))) ;;;; Пример 2
 (quantity 36 *d + 15 *m  + 45.23 *s)
 ...
 (quit)                                               ;;;; Выход без сохранения

;;;; Выход с сохранением
 *home*
 (defparameter *home* \"d:\\\")                          ;;;; Каталог для записи
 (slad)                                               ;;;; Сохранить состояние и завершить работу
")



(defun help ()
"help"
  (format t *help* ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
