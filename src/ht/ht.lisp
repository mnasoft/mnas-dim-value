;;;; ./mnas-dim-value/src/ht/ht.lisp

(defpackage :mnas-dim-value/ht
  (:use #:cl
        ;;#:mnas-dim-value/ht-en
        ;;#:mnas-dim-value/ht-ru
        ;;#:mnas-dim-value/ht-uk        
        #:mnas-dim-value/vars
        #:mnas-dim-value/func
        #:mnas-dim-value/class
        #:mnas-hash-table
        )
  (:export nd-named                     ; *nd-named*
           nd-list                      ; *nd-list*
           m-coeff                      ; *m-coeff*
           nm->value                    ; *nm->value*
           dim->unit-symbol             ; *dim->unit-symbol*
           unit-symbol->dim             ; *unit-symbol->dim*
           dim->quantity                ; *dim->quantity*
           quantity->dim                ; *quantity->dim*
           ))
           
(in-package :mnas-dim-value/ht)

(defun nm->value ()
  (case (get-env "LANGUAGE" *variable-set*)
    (:en mnas-dim-value/ht-en:*nm->value*)
    (:ru mnas-dim-value/ht-ru:*nm->value*)
    (:uk mnas-dim-value/ht-uk:*nm->value*)
    (otherwise mnas-dim-value/ht-en:*nm->value*)))

(defun nd-list ()
    (case (get-env "LANGUAGE" *variable-set*)
    (:en mnas-dim-value/ht-en:*nd-list*)
    (:ru mnas-dim-value/ht-ru:*nd-list*)
    (:uk mnas-dim-value/ht-uk:*nd-list*)    
    (otherwise mnas-dim-value/ht-en:*nd-list*)))

(defun m-coeff ()
    (case (get-env "LANGUAGE" *variable-set*)
    (:en mnas-dim-value/ht-en:*m-coeff*)
    (:ru mnas-dim-value/ht-ru:*m-coeff*)
    (:uk mnas-dim-value/ht-uk:*m-coeff*)    
    (otherwise mnas-dim-value/ht-en:*m-coeff*)))

(defun nm->value ()
    (case (get-env "LANGUAGE" *variable-set*)
    (:en mnas-dim-value/ht-en:*nm->value*)
    (:ru mnas-dim-value/ht-ru:*nm->value*)
    (:uk mnas-dim-value/ht-uk:*nm->value*)    
    (otherwise mnas-dim-value/ht-en:*nm->value*)))

(defun dim->unit-symbol ()
    (case (get-env "LANGUAGE" *variable-set*)
    (:en mnas-dim-value/ht-en:*dim->unit-symbol*)
    (:ru mnas-dim-value/ht-ru:*dim->unit-symbol*)
    (:uk mnas-dim-value/ht-uk:*dim->unit-symbol*)
    (otherwise mnas-dim-value/ht-en:*dim->unit-symbol*)))

(defun unit-symbol->dim ()
    (case (get-env "LANGUAGE" *variable-set*)
    (:en mnas-dim-value/ht-en:*unit-symbol->dim*)
    (:ru mnas-dim-value/ht-ru:*unit-symbol->dim*)
    (:uk mnas-dim-value/ht-uk:*unit-symbol->dim*)
    (otherwise mnas-dim-value/ht-en:*unit-symbol->dim*)))

(defun dim->quantity ()
    (case (get-env "LANGUAGE" *variable-set*)
    (:en mnas-dim-value/ht-en:*dim->quantity*)
    (:ru mnas-dim-value/ht-ru:*dim->quantity*)
    (:uk mnas-dim-value/ht-uk:*dim->quantity*) 
    (otherwise mnas-dim-value/ht-en:*dim->quantity*)))

(defun quantity->dim ()
    (case (get-env "LANGUAGE" *variable-set*)
    (:en mnas-dim-value/ht-en:*quantity->dim*)
    (:ru mnas-dim-value/ht-ru:*quantity->dim*)
    (:uk mnas-dim-value/ht-uk:*quantity->dim*)    
    (otherwise mnas-dim-value/ht-en:*quantity->dim*)))

