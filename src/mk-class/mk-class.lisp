(defpackage :mnas-dim-value/mk-class
  (:use #:cl
        #:mnas-dim-value/class)
  (:export vd)
  (:export make-nd-items
           make-nd-form-list-el
           )
  (:export nd
           nd-get
           nd-clear
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
                 :dims (list m kg s A K cd mol rad sr)))

(let ((nd-list nil))
  (defun nd (q-en q-ru dim u-en u-ru s-en s-ru val &optional (coeff '((-30 30))))
    (let ((item
            (make-instance '<nd>
		   :quantity-name-en q-en
		   :quantity-name-ru q-ru
		   :dimension-symbol dim
		   :unit-name-en     u-en
		   :unit-name-ru     u-ru 
		   :unit-symbol-en   s-en 
		   :unit-symbol-ru   s-ru
		   :value            val
                   :coeff            coeff)))
    
    (setf nd-list (cons item nd-list))))
;;;;
  (defun nd-clear ()
    (setf nd-list nil))
;;;;  
  (defun nd-get ()
    (reverse nd-list)))

(defun make-nd-items (lst)
  (loop :for (q-en q-ru dim u-en u-ru s-en s-ru val coeff) :in lst
        :collect
        (nd q-en q-ru dim u-en u-ru s-en s-ru val coeff)))

