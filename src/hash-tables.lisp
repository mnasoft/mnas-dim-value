;;;; hash-tables.lisp

(in-package :mnas-dim-value)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun load-dim->quantity-name-en ()
  (unless *dim->quantity-name-en-loaded*
    (flet ((dim->quantity-name-en (tbl) (mapc #'(lambda (el) (setf (gethash (<vd>-dims (<nd>-value el)) *dim->quantity-name-en*) (<nd>-quantity-name-en el))) (reverse tbl))))
      (mapc #'(lambda (el) (dim->quantity-name-en el)) (list *nd-si-derived-units-tbl-04* *nd-si-derived-units-tbl-03* *nd-si-derived-units-tbl-02* *nd-si-main-units*)))
    (print-hash-table *dim->quantity-name-en*)
    (setf *dim->quantity-name-en-loaded* t)))



(defun load-quantity-name-en->dim ()
  (unless *quantity-name-en->dim-loaded*
    (flet ((string->dimension (tbl) (mapc #'(lambda (el) (setf (gethash  (<nd>-quantity-name-en el) *quantity-name-en->dim*) (<vd>-dims (<nd>-value el)))) (reverse tbl))))
      (mapc #'(lambda (el) (string->dimension el)) (list *nd-si-derived-units-tbl-04* *nd-si-derived-units-tbl-03* *nd-si-derived-units-tbl-02* *nd-si-main-units*)))
    (print-hash-table *quantity-name-en->dim*)
    (setf *quantity-name-en->dim-loaded* t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun load-si-units ()
  (load-nm-vl)
  (load-nm-vl-ru->en)
  (load-nm-vl-en->ru)
  (load-dim->unit-symbol-en)
  (load-unit-symbol-en->dim)
  (load-dim->unit-symbol-ru)
  (load-unit-symbol-ru->dim)
  (load-dim->quantity-name-en)
  (load-quantity-name-en->dim))

(load-si-units)
