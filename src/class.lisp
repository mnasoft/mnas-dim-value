;;;; mnas-dim-value.lisp

(in-package :mnas-dim-value)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-nd-items (lst)
  (mapcar
   #'(lambda (el)
       (make-instance '<nd>
		      :quantity-name-en (first     el)
		      :quantity-name-ru (second    el)
		      :dimension-symbol (third     el)
		      :unit-name-en     (fourth    el)
		      :unit-name-ru     (fifth     el)
		      :unit-symbol-en   (sixth     el)
		      :unit-symbol-ru   (seventh   el)
		      :value            (car (last el))))
   lst))

(defun make-nd-form-list-el (lst)
  (mapcar
   #'(lambda (el)
       (make-instance '<nd>
		      :quantity-name-en (first   el)
		      :quantity-name-ru (second  el)
		      :dimension-symbol (third   el)
		      :unit-name-en     (fourth  el)
		      :unit-name-ru     (fifth   el)
		      :unit-symbol-en   (sixth   el)
		      :unit-symbol-ru   (seventh el)
		      :value            (eighth  el)
		      :coeff            (ninth   el)))
   lst))
