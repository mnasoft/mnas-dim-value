;;;; package.lisp

(defpackage #:mnas-dim-value
  (:use    #:cl #:cl-ppcre)
  (:export K->C) ;; Перевод градусов кельвина в градусы цельсия
  (:export C->K) ;; Перевод градусов цельсия в градусы кельвина
  (:export k->M) ;; Перевод значения с приставкой кило в число с приставкой мега
  (:export M->k) ;; Перевод значения с приставкой мега в число с приставкой кило
  (:export kgs/cm2->Pa)			;
  (:export Pa->kgs/cm2)			;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  
  (:export dim-name-list dim-string-by-dim-name)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  
  (:export *g* *Gn* *C-0* *V-0* *R-0* *Na* *No* *k* *a-e-m* *m-e* *e* *F* *h* *c* *μ-0* *ε-0*)
  (:export |m| |kg| |s| |A| |K| |cd| |mol| |rad| |sr| )
  (:export |Hz| |N| |Pa| |J| |W| |C| |V| |Ω| |S| |F| |Wb| |H| |Τ| |lm| |lx| |Bq| |Gy| |Sv| )
  (:export vd vd* vd/ vd- vd+)
  (:export vd-expt vd-sqrt)
  (:export vd-val vd-dims)
  (:export *si-main-units*    *si-derived-units-tbl-02*    *si-derived-units-tbl-03*    *si-derived-units-tbl-04*)
  (:export *nd-si-main-units* *nd-si-derived-units-tbl-02* *nd-si-derived-units-tbl-03* *nd-si-derived-units-tbl-04*)
  (:export *not-si-units-tbl-05*    *not-si-units-tbl-07*)
  (:export *nd-not-si-units-tbl-05* *nd-not-si-units-tbl-07*)
  (:export *nm-vl* *nm-vl-ru->en* *nm-vl-en->ru*)
  (:export *mult-nm-vl*)
  (:export *dimension->string* *string->dimension*)
  (:export *dimension->string-ru* *string->dimension-ru*)
  (:export *dimension->name* *name->dimension*)
  (:export *dimension->name-ru* *name->dimension-ru*)

  (:export dimensionp)
  )
