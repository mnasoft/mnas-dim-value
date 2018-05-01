;;;; test.lisp

(in-package #:mnas-dim-value)

(defparameter *nd-list*
  (reverse
   (append
    *nd-si-main-units*
    *nd-si-derived-units-tbl-02*
    *nd-si-derived-units-tbl-03*
    *nd-si-derived-units-tbl-04*
    *nd-not-si-units-tbl-05*
    *nd-not-si-units-tbl-07*)))

(defun quantity-name (value &key (vd-language *vd-language*))
  "Возвращает наименование величины.
Пример использования:
;;;; (quantity-name (vd/ |kg| |m| |m| |m|) :vd-language :en) => (\"density\" \"mass density\")
;;;; (quantity-name (vd/ (vd* |kg| *g*) (vd-expt (vd* 0.01 |m|) 2) 1000))
"
  (let ((rez nil)
	(item nil)
	(quantity-name-language
	 (cond
	   ((eq vd-language :en) #'nd-quantity-name-en)
	   ((eq vd-language :ru) #'nd-quantity-name-ru))))
    (mapc
     #'(lambda (el)
	 (when (equal (vd-dims (nd-value el))
		      (vd-dims value))
	   (setf item (funcall quantity-name-language el))
	   (cond
	     ((stringp item) (push item rez))
	     ((listp item) (setf rez (append item rez))))))
     *nd-list*)
    (remove-duplicates rez :test #'equal)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun do-symbols->list (package)
  "Пример использования 
;;;; (do-symbols->list 'mnas-dim-value)"
  (let ((lst ()))
    (do-symbols (s (find-package package)) (push s lst))
    lst))

(do-symbols->list 'mnas-dim-value)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(quantity-from-string-not-eval "25 kg + 783.565 g"       )

(quantity-from-string "70*kgf/(70*cm^2)"                 )

(quantity-from-string "(0-1)*(55 m^2+45 mm^2)kgf/cm^2"   )

(quantity-from-string "(1/kg^2)*(m*s^3)/(N^2*m^3)"       )

(quantity-from-string "3600 r/h"                         )

(quantity-from-string "2°+10'+55.4\""                    ) 
