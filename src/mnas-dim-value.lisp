;;;; /src/mnas-dim-value.lisp

(mnas-macro:mnas-defpackage
 :mnas-dim-value
 (:nicknames "MDV")
 (:use #:cl)
 (:use-and-export #:mnas-dim-value/convert
                  #:mnas-dim-value/func
                  #:mnas-dim-value/vars
                  #:mnas-dim-value/class
                  #:mnas-dim-value/mk-class
                  #:mnas-dim-value/method
                  #:mnas-dim-value/const
                  #:mnas-dim-value/macro
                  )
 (:use #:mnas-dim-value/tbl
       #:mnas-hash-table
       #:mnas-dim-value/ht/core
       #:mnas-dim-value/generic
       #:mnas-dim-value/method
       #:mnas-dim-value/macro)
 (:export 
  help
  unit-symbol-by-quantity-name
  quantity-names             ; Возвращает список наименований величин.
  find-quantity
  nd-tables
  ))

(in-package :mnas-dim-value)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun nd-tables ()
  (if (eq (get-env "LANGUAGE" *variable-set*) :ru)
      mnas-dim-value/tbl-ru:*nd-tables*
      mnas-dim-value/tbl-en:*nd-tables*
      ))



(defun unit-symbol-by-quantity-name ( d-type )
  "
@b(Описание:) функция@b(unit-symbol-by-quantity-name)

 @b(Пример использования:)
@begin[lang=lisp](code)
 (unit-symbol-by-quantity-name \"length\")
 (unit-symbol-by-quantity-name \"temperature\")
 (unit-symbol-by-quantity-name \"mass\")
 (unit-symbol-by-quantity-name \"specific entropy\")
 (unit-symbol-by-quantity-name \"capacitance\")
 (unit-symbol-by-quantity-name \"mass density\")
@end(code)
"
  (let ((regexp (ppcre:create-scanner (concatenate 'string ".*" d-type ".*"))))
    (loop :for i :in (nd-tables)
          :when (ppcre:scan regexp (<nd>-quantity i))
            :collect (<nd>-unit-symbol i)
          )))

(defun quantity-names ()
  "@b(Описание:) функция @b(quantity-names) возвращает список наименований
величин для текущего языка.

 @b(Пример использования:)
@begin[lang=lisp](code)
 (progn
   (set-env :en \"LANGUAGE\" *variable-set*)
   (quantity-names))
 (progn 
   (set-env :ru \"LANGUAGE\" *variable-set*)
   (quantity-names))
@end(code)
"
  (loop :for i :in (nd-tables)
          :append
          (loop :for part :in (cl-ppcre:split "," (<nd>-quantity i))
                :collect (string-trim '(#\Space #\Tab) part))
            :into rez
        :finally (return (delete-duplicates (sort rez #'string<) :test #'equal))))

(defun find-quantity (regexp)
  "@b(Описание:) функция @b(find-quantity) возвраащает список
наименований величин, имена которых соответствуют регулярному
выражению."
  (let ((regexp (ppcre:create-scanner regexp)))
    (loop :for i :in (quantity-names)
          :when (ppcre:scan regexp i)
            :collect i)))
    
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
