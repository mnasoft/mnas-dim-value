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
  (:export vd-expt
           vd-sqrt
           )
  (:export vd~+                         ; Сложение
           vd~-                         ; Вычирание
           vd~*                         ; Умножение
           vd~/                         ; Деление
           vd~pow
           vd~root 
           vd~exp
           vd~expt
           vd~ln
           vd~log
           vd~sin
           vd~cos
           vd~tan
           vd~asin
           vd~acos
           vd~atan
           vd~sinh
           vd~cosh
           vd~tanh
           vd~asinh
           vd~acosh
           vd~atanh
           vd~abs
           vd~equal
           vd~equalp
           )
  (:export vd~=
           vd~/=
           vd~<
           vd~<=
           vd~>
           vd~>=)  
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
   dim-name-list ;; Возвращает список наименований величин.

   UNUSE-MNAS-DIM-VALUE

   USE-MNAS-DIM-VALUE

   QUANTITY-NAME
   ))
  


(in-package :mnas-dim-value)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun dim-string-by-dim-name ( d-type )
  "
@b(Описание:) функция@b(dim-string-by-dim-name)

 @b(Пример использования:)
@begin[lang=lisp](code)
 (dim-string-by-dim-name \"length\")
 (dim-string-by-dim-name \"specific entropy\")
 (dim-string-by-dim-name \"capacitance\")
 (dim-string-by-dim-name \"mass density\")
@end(code)
" 
  (loop :for i :in mnas-dim-value/tbl-en:*nd-tables*
        :when (string= (<nd>-quantity-name i) d-type)
        :collect (<nd>-unit-symbol i)
        ))


(defun dim-name-list ()
  "@b(Описание:) функция @b(dim-name-list) возвращает список наименований
величин.

 @b(Пример использования:)
@begin[lang=lisp](code)
 (progn 
   (setf mnas-dim-value/class:*vd-language* :en)
   (dim-name-list))
  (progn 
    (setf mnas-dim-value/class:*vd-language* :ru)
    (dim-name-list)) 
@end(code)
"
  (let ((q-name
          (if (eq mnas-dim-value/class:*vd-language* :ru) 
              #'<nd>-quantity
              #'<nd>-quantity ;; Что-то нужно сделать
              )))
    (loop :for i :in mnas-dim-value/tbl-en:*nd-tables*
          :append
          (loop :for part :in (cl-ppcre:split "," (funcall q-name i))
                :collect (string-trim '(#\Space #\Tab) part))
            :into rez
          :finally (return (delete-duplicates (sort rez #'string<) :test #'equal)))))
<nd>
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

(vd-convert "20°20'30.56\"")
