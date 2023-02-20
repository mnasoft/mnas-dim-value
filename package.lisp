;;;; package.lisp

(defpackage #:mnas-dim-value
  (:nicknames "MDV")
  (:use       #:cl )
  (:export vd
           vd-val
           vd-dims)
  (:export vd+ vd- vd* vd/
           vd-expt vd-sqrt
           )
  (:export |m| |kg| |s| |A| |K| |cd| |mol|
           |rad| |sr|
           )
  (:export |Hz| |N| |Pa| |J| |W|
           |C| |V| |F| |Ω| |S| |Wb|
           |Τ| |H| |lm| |lx| |Bq|
           |Gy| |Sv| |kat|
           )
  (:export |*g*| |*Gn*| |*C-0*| |*V-0*|
           |*R-0*| |*Na*| |*No*|
           |*k*| |*a-e-m*|
           |*m-e*| |*e*|
           |*F*| |*h*| |*c*| |*μ-0*| |*ε-0*|
           )
  (:export C->K K->C)
  (:export M->K K->M)
  (:export QUANTITY
           qi
           *NM-VL-EN->RU*
           *NM-VL*
           UNIT-NAME)
  (:export PROMPT-READ-LINE
           QUANTITY-FROM-STRING
           HELP
           DIMENSIONP
           *NM-VL-RU->EN*
           DIM-STRING-BY-DIM-NAME
           DIM-NAME-LIST
           QUANTITY-INTERACTIVE
           UNUSE-MNAS-DIM-VALUE
           KGS/CM2->PA
           USE-MNAS-DIM-VALUE
           PA->KGS/CM2
           QUANTITY-NAME))

(in-package #:mnas-dim-value)


