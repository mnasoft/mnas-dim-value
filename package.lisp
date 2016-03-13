;;;; package.lisp

(defpackage #:mnas-dim-value
  (:use #:cl #:cl-ppcre)
  (:export K->C)			; Перевод градусов кельвина в градусы цельсия
  (:export C->K)			; Перевод градусов цельсия в градусы кельвина
  (:export k->M)			; Перевод значения с приставкой кило в число с приставкой мега
  (:export M->k)			; Перевод значения с приставкой мега в число с приставкой кило
  (:export kgs/cm2->Pa)			;
  (:export Pa->kgs/cm2)			;
)

;;;;(declaim (optimize (space 0) (compilation-speed 0)  (speed 0) (safety 3) (debug 3)))
