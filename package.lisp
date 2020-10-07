;;;; package.lisp

(defpackage #:mnas-dim-value)

(defpackage   #:mnas-dim-value
  (:nicknames "MDV")
  (:use       #:cl )
  (:export
   VD-VAL
   QUANTITY
   QI
   VD-
   *NM-VL-EN->RU*
   *NM-VL*
   F
   UNIT-NAME
   C->K
   K
   A
   *F*
   S
   *C-0*
   VD-DIMS
   *V-0*
   *R-0*
   PROMPT-READ-LINE
   N
   QUANTITY-FROM-STRING
   V
   HELP
   J
   VD+
   H
   K->C
   C
   DIMENSIONP
   VD/
   W
   *NM-VL-RU->EN*
   DIM-STRING-BY-DIM-NAME
   Ω
   VD-SQRT
   VD*
   DIM-NAME-LIST
   M->K
   QUANTITY-INTERACTIVE
   Τ
   K->M
   VD-EXPT
   UNUSE-MNAS-DIM-VALUE
   VD
   KGS/CM2->PA
   USE-MNAS-DIM-VALUE
   PA->KGS/CM2
   QUANTITY-NAME
   )
  (:export 
   |*μ-0*|
   |*g*|
   |*k*|
   |*h*|
   |*a-e-m*|
   |*No*|
   |Sv|
   |*Na*|
   |lx|
   |Pa|
   |rad|
   |m|
   |sr|
   |Bq|
   |mol|
   |*e*|
   |kat|
   |s|
   |Wb|
   |Hz|
   |lm|
   |*c*|
   |cd|
   |*ε-0*|
   |kg|
   |*Gn*|
   |*m-e*|
   |Gy|
   )
  )

;;;; (declaim (optimize (compilation-speed 0) (debug 3) (safety 0) (space 0) (speed 0)))
