;;;; package.lisp

(defpackage #:mnas-dim-value)

(defpackage #:mnas-dim-value
  (:nicknames "MDV")
  (:use    #:cl )
;;;; mnas-dim-value.lisp  
  (:export mnas-dim-value::K->C 
	   mnas-dim-value::C->K 
	   mnas-dim-value::k->M 
	   mnas-dim-value::M->k 
	   mnas-dim-value::kgs/cm2->Pa	
	   mnas-dim-value::Pa->kgs/cm2	
	   mnas-dim-value::dim-name-list
	   mnas-dim-value::dim-string-by-dim-name
	   mnas-dim-value::help	   
	   )
;;;; constants.lisp  
  (:export mnas-dim-value::|*g*|
	   mnas-dim-value::|*Gn*|
	   mnas-dim-value::|*C-0*|
	   mnas-dim-value::|*V-0*|
	   mnas-dim-value::|*R-0*|
	   mnas-dim-value::|*Na*|
	   mnas-dim-value::|*No*|
	   mnas-dim-value::|*k*|
	   mnas-dim-value::|*a-e-m*|
	   mnas-dim-value::|*m-e*|
	   mnas-dim-value::|*e*|
	   mnas-dim-value::|*F*|
	   mnas-dim-value::|*h*|
	   mnas-dim-value::|*c*|
	   mnas-dim-value::|*μ-0*|
	   mnas-dim-value::|*ε-0*| )
  (:export mnas-dim-value::|m|
	   mnas-dim-value::|kg|
	   mnas-dim-value::|s|
	   mnas-dim-value::|A|
	   mnas-dim-value::|K|
	   mnas-dim-value::|cd|
	   mnas-dim-value::|mol|
	   mnas-dim-value::|rad|
	   mnas-dim-value::|sr|)
  (:export mnas-dim-value::|Hz|
	   mnas-dim-value::|N|
	   mnas-dim-value::|Pa|
	   mnas-dim-value::|J|
	   mnas-dim-value::|W|
	   mnas-dim-value::|C|
	   mnas-dim-value::|V|
	   mnas-dim-value::|Ω|
	   mnas-dim-value::|S|
	   mnas-dim-value::|F|
	   mnas-dim-value::|Wb|
	   mnas-dim-value::|H|
	   mnas-dim-value::|Τ|
	   mnas-dim-value::|lm|
	   mnas-dim-value::|lx|
	   mnas-dim-value::|Bq|
	   mnas-dim-value::|Gy|
	   mnas-dim-value::|Sv|
	   mnas-dim-value::|kat|)
  (:export mnas-dim-value::vd
	   mnas-dim-value::vd*
	   mnas-dim-value::vd/
	   mnas-dim-value::vd-
	   mnas-dim-value::vd+)
  (:export mnas-dim-value::vd-expt
	   mnas-dim-value::vd-sqrt)
  (:export mnas-dim-value::vd-val
	   mnas-dim-value::vd-dims)
  (:intern  mnas-dim-value::*si-main-units*
	    mnas-dim-value::*si-derived-units-tbl-02*
	    mnas-dim-value::*si-derived-units-tbl-03*
	    mnas-dim-value::*si-derived-units-tbl-04*
	    mnas-dim-value::*not-si-units-tbl-05*
	    mnas-dim-value::*not-si-units-tbl-07*
	    mnas-dim-value::*other-units-tbl-b-01*
	    )
  
  (:intern mnas-dim-value::*nd-si-main-units*
	   mnas-dim-value::*nd-si-derived-units-tbl-02*
	   mnas-dim-value::*nd-si-derived-units-tbl-03*
	   mnas-dim-value::*nd-si-derived-units-tbl-04*
	   mnas-dim-value::*nd-not-si-units-tbl-05*
	   mnas-dim-value::*nd-not-si-units-tbl-07* 
	   mnas-dim-value::*nd-other-units-tbl-b-01*)
  
  (:export mnas-dim-value::*nm-vl*
	   mnas-dim-value::*nm-vl-ru->en*
	   mnas-dim-value::*nm-vl-en->ru*)
  (:export mnas-dim-value::^)
;;;; methods.lisp
  (:export mnas-dim-value::dimensionp)
;;;; marco.lisp  
  (:export mnas-dim-value::quantity )
;;;; values.lisp
  (:export mnas-dim-value::quantity-from-string
	   mnas-dim-value::prompt-read-line
	   mnas-dim-value::quantity-interactive
	   mnas-dim-value::qi)
;;;; cl-user-funcs.lisp  
  (:export mnas-dim-value::use-mnas-dim-value
	   mnas-dim-value::unuse-mnas-dim-value)
  (:export mnas-dim-value::quantity-name mnas-dim-value::unit-name)
  )

;;;; (declaim (optimize (compilation-speed 0) (debug 3) (safety 0) (space 0) (speed 0)))
