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
 (:use-and-export #:mnas-dim-value/func
                  #:mnas-dim-value/vars
                  #:mnas-dim-value/class
                  #:mnas-dim-value/mk-class
                  #:mnas-dim-value/method
                  #:mnas-dim-value/const)
 (:use #:mnas-dim-value/tbl
       #:mnas-hash-table
       #:mnas-dim-value/ht
       #:mnas-dim-value/generic
       #:mnas-dim-value/method
       #:mnas-dim-value/macro)
 
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
  QUANTITY-NAME))

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
          (if (eq (get-env "LANGUAGE" *variable-set*) :ru)
              #'<nd>-quantity
              #'<nd>-quantity ;; Что-то нужно сделать
              )))
    (loop :for i :in mnas-dim-value/tbl-en:*nd-tables*
          :append
          (loop :for part :in (cl-ppcre:split "," (funcall q-name i))
                :collect (string-trim '(#\Space #\Tab) part))
            :into rez
          :finally (return (delete-duplicates (sort rez #'string<) :test #'equal)))))

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
