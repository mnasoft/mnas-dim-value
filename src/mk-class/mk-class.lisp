(defpackage :mnas-dim-value/mk-class
  (:use       #:cl #:mnas-dim-value/class)
  (:export    vd
              ))

(in-package :mnas-dim-value/mk-class)

(defun vd (x &key (m 0) (kg 0) (s 0) (A 0) (K 0) (cd 0) (mol 0) (rad 0) (sr 0))
"@b(Описание:) функция vd создает число с размерностью (ЧсР)

 @b(Пример использования:)
@begin[lang=lisp](code)

@end(code)
"
  (make-instance '<vd>
                 :val x
                 :dims (list m kg s A K cd mol rad sr) ))
