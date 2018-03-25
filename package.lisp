;;;; package.lisp

(defpackage #:mnas-dim-value
  (:use    #:cl #:cl-ppcre)
  (:export K->C) ;; Перевод градусов кельвина в градусы цельсия
  (:export C->K) ;; Перевод градусов цельсия в градусы кельвина
  (:export k->M) ;; Перевод значения с приставкой кило в число с приставкой мега
  (:export M->k) ;; Перевод значения с приставкой мега в число с приставкой кило
  (:export kgs/cm2->Pa)			;
  (:export Pa->kgs/cm2)			;
  (:export *g* *Gn* *C-0* *V-0* *R-0* *Na* *No* *k* *a-e-m* *m-e* *e* *F* *h* *c* *μ-0* *ε-0*)
  (:export |m| |kg| |s| |A| |K| |cd| |mol| |rad| |sr| )
  (:export |Hz| |N| |Pa| |J| |W| |C| |V| |Ω| |S| |F| |Wb| |H| |Τ| |lm| |lx| |Bq| |Gy| |Sv| )
  (:export vd vd* vd/ vd- vd+) 
  )

;;;;(declaim (optimize (space 0) (compilation-speed 0)  (speed 0) (safety 3) (debug 3)))
