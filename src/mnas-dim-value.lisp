;;;; /src/mnas-dim-value.lisp

(defmacro mnas-defpackage (name &rest options)
  "Расширение defpackage с поддержкой :use-and-export, сортировкой символов и печатью в нижнем регистре."
  (let ((import-all-exported-packages '())
        (defpackage-options '()))
    ;; Разбираем опции и обрабатываем :use-and-export
    (dolist (option options)
      (cond
        ((and (listp option)
              (eq (first option) :use-and-export))
         (setq import-all-exported-packages
               (append import-all-exported-packages (rest option))))
        (t
         (push option defpackage-options))))
    ;; Генерируем :import-from для каждого пакета из :import-all-exported
    (dolist (pkg import-all-exported-packages)
      (let* ((package (find-package pkg))
             (symbols (when package
                        (sort
                         (loop for sym being the external-symbols of package
                               collect sym)
                         #'string< :key #'symbol-name))))
        (when symbols
          (push `(:use ,pkg) defpackage-options)
          #+ nil (push `(:import-from ,pkg) defpackage-options)
          (push `(:export ,@symbols) defpackage-options))))
    ;; Создаём окончательное выражение defpackage
    (let ((defpackage-form `(defpackage ,name ,@(nreverse defpackage-options))))
      ;; Устанавливаем *print-case* в :downcase на время pprint
      (let ((*print-case* :downcase))
        ;; Выводим сгенерированный код на печать
        (pprint defpackage-form)
        (format t "~2%"))
      ;; Возвращаем nil, чтобы предотвратить оценивание
      defpackage-form)))

(mnas-defpackage
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
       #:mnas-dim-value/ht
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
