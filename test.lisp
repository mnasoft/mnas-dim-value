;;;; test.lisp

(in-package #:cl-user)

(annot:enable-annot-syntax)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :mdv)






  
(setf  (vd-names-ru *vv*) '("m" "kg" "s" "A" "K" "cd" "mol"  "rad" "sr"))
		       ((eq *vd-language* :ru) '("м" "кг" "с" "А" "К" "кд" "моль" "рад" "ср"))))
(defparameter *vv* (quantity-from-string "(25 kgf + 783.565 gf) / s cm^2"))

(quantity-from-string "70*kgf/(70*cm^2)"                 )

(quantity-from-string "(0-1)*(55 m^2+45 mm^2)kgf/cm^2"   )

(quantity-from-string "(1/kg^2)*(m*s^3)/(N^2*m^3)"       )

(quantity-from-string "3600 r/h"                         )

(quantity-from-string "2°+10'+55.4\""                    ) 

(dimensionp "kgf*mm^2")

(dimensionp "kg*m*s^-2")

(dimensionp "N")

(quantity-from-string (concatenate 'string "10" "*" "kgf/m^2"))

(unit-name (dimensionp "kgf/mm^2") nil)

(print-hash-table enter-box::*nm-vl*)

(vd* 1.0 "cal"  |m|)

(quantity-from-string  "4.1868 * m^3*kgs^-2")

(vd/ 50000.0 (vd* 1.0 "cal" |m|)
