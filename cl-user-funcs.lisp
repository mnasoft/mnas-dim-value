;;;; cl-user-funcs.lisp

(in-package #:mnas-dim-value )

(defun mnas-dim-value-import-symbols ()
  (import '(mnas-dim-value:quantity-interactive 
	    mnas-dim-value:qi 
	    mnas-dim-value:quantity-from-string 
	    mnas-dim-value:quantity 
	    mnas-dim-value:vd 
	    mnas-dim-value:^ 
	    mnas-dim-value:vd* 
	    mnas-dim-value:vd/ 
	    mnas-dim-value:vd+ 
	    mnas-dim-value:vd- 
	    mnas-dim-value:vd-expt 
	    mnas-dim-value:|m| 
	    mnas-dim-value:|kg| 
	    mnas-dim-value:|s| 
	    mnas-dim-value:|A| 
	    mnas-dim-value:|K| 
	    mnas-dim-value:|cd| 
	    mnas-dim-value:|mol| 
	    mnas-dim-value:|rad| 
	    mnas-dim-value:|sr| 
	    mnas-dim-value:|Hz| 
	    mnas-dim-value:|N| 
	    mnas-dim-value:|Pa| 
	    mnas-dim-value:|J| 
	    mnas-dim-value:|W| 
	    mnas-dim-value:|C| 
	    mnas-dim-value:|V| 
	    mnas-dim-value:|F| 
	    mnas-dim-value:|Ω| 
	    mnas-dim-value:|S| 
	    mnas-dim-value:|Wb| 
	    mnas-dim-value:|Τ| 
	    mnas-dim-value:|H| 
	    mnas-dim-value:|lm| 
	    mnas-dim-value:|lx| 
	    mnas-dim-value:|Bq| 
	    mnas-dim-value:|Gy| 
	    mnas-dim-value:|Sv|
	    mnas-dim-value:|kat|)
	  (find-package :cl-user)))
