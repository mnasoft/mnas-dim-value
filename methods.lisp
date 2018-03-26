;;;; methods.lisp

(in-package #:mnas-dim-value)

(defmethod print-object :before ((x vd) s) (format s "#vd" ))

(defmethod print-object         ((x vd) s)
  (format s "(val=~S [ " (vd-val x))
  (mapc #'(lambda (no str)
	    (if (/= (nth no (d-lst x)) 0)
		(format s
			(concatenate 'string str "^~A ")
			(nth no (d-lst x)))))
	'( 0    1   2   3   4    5     6     7    8)
	'("m" "kg" "s" "A" "K" "cd" "mol" "rad" "sr"))
  (format s "])"))

;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod print-object         ((x vd) s)
  (cond
    ((same-dimension |m|   x) (format s "~S[~A]" (vd-val x) "m"))
    ((same-dimension |kg|  x) (format s "~S[~A]" (vd-val x) "kg"))
    ((same-dimension |s|   x) (format s "~S[~A]" (vd-val x) "s"))
    ((same-dimension |A|   x) (format s "~S[~A]" (vd-val x) "A"))
    ((same-dimension |K|   x) (format s "~S[~A]" (vd-val x) "K"))
    ((same-dimension |cd|  x) (format s "~S[~A]" (vd-val x) "cd"))
    ((same-dimension |mol| x) (format s "~S[~A]" (vd-val x) "mol"))
    ((same-dimension |rad| x) (format s "~S[~A]" (vd-val x) "rad"))
    ((same-dimension |sr|  x) (format s "~S[~A]" (vd-val x) "sr"))

    ((same-dimension |Hz|  x) (format s "~S[~A]" (vd-val x) "Hz"))
    ((same-dimension |N|   x) (format s "~S[~A]" (vd-val x) "N"))
    ((same-dimension |Pa|  x) (format s "~S[~A]" (vd-val x) "Pa"))
    ((same-dimension |J|   x) (format s "~S[~A]" (vd-val x) "J"))
    ((same-dimension |W|   x) (format s "~S[~A]" (vd-val x) "W"))
    ((same-dimension |C|   x) (format s "~S[~A]" (vd-val x) "C"))
    ((same-dimension |V|   x) (format s "~S[~A]" (vd-val x) "V"))
    ((same-dimension |F|   x) (format s "~S[~A]" (vd-val x) "F"))
    ((same-dimension |Ω|   x) (format s "~S[~A]" (vd-val x) "Ω"))
    ((same-dimension |S|   x) (format s "~S[~A]" (vd-val x) "S"))
    ((same-dimension |Wb|  x) (format s "~S[~A]" (vd-val x) "Wb"))
    ((same-dimension |Τ|   x) (format s "~S[~A]" (vd-val x) "Τ"))
    ((same-dimension |H|   x) (format s "~S[~A]" (vd-val x) "H"))
    ((same-dimension |lm|  x) (format s "~S[~A]" (vd-val x) "lm"))
    ((same-dimension |lx|  x) (format s "~S[~A]" (vd-val x) "lx"))
    ((same-dimension |Bq|  x) (format s "~S[~A]" (vd-val x) "Bq"))
    ((same-dimension |Gy|  x) (format s "~S[~A]" (vd-val x) "Gy"))
    ((same-dimension |Sv|  x) (format s "~S[~A]" (vd-val x) "Sv"))
    ((same-dimension |kat| x) (format s "~S[~A]" (vd-val x) "kat"))
    (t
     (format s "(val=~S [ " (vd-val x))
     (mapc #'(lambda (no str)
	       (if (/= (nth no (d-lst x)) 0)
		   (format s
			   (concatenate 'string str "^~A ")
			   (nth no (d-lst x)))))
	   '( 0    1   2   3   4    5     6     7    8)
	   '("m" "kg" "s" "A" "K" "cd" "mol" "rad" "sr"))
     (format s "])"))
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
