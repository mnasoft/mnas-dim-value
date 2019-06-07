;;;; test.lisp

(in-package #:mnas-dim-value)





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

(quantity-from-string "(25 kgf + 783.565 gf) / cm^2")

(quantity-from-string "70*kgf/(70*cm^2)"                 )

(quantity-from-string "(0-1)*(55 m^2+45 mm^2)kgf/cm^2"   )

(quantity-from-string "(1/kg^2)*(m*s^3)/(N^2*m^3)"       )

(quantity-from-string "3600 r/h"                         )

(quantity-from-string "2°+10'+55.4\""                    ) 

(dimensionp "kgf/mm^2")

(quantity-from-string (concatenate 'string "10" "*" "kgf/m^2"))

(unit-name (dimensionp "kgf/mm^2") nil)

(print-hash-table enter-box::*nm-vl*)

(vd* 1.0 "cal")

(vd/ 50000.0 (vd* 1.0 "cal"))
