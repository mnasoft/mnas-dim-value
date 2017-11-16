;;;; mnas-dim-value.lisp

(in-package #:mnas-dim-value)

;;; "mnas-dim-value" goes here. Hacks and glory await!

;;;;m kg s A K cd mol rad sr

(defclass vd()
  ((val :accessor vd-val :initarg :val :initform 0.0)
   (d-lst :accessor d-lst :initarg :d-lst :initform (list 0 0 0  0 0 0  0 0 0) )
   ))

(defmethod print-object :before ((x vd) s) (format s "#vd" ))

(defmethod print-object         ((x vd) s)
  (format s "(val=~S [ " (vd-val x))
  (mapc #'(lambda (no str)
	    (if (/= (nth no (d-lst x)) 0)
		(format s
			(concatenate 'string str "^~A ")
			(nth no (d-lst x)))))
	'(0 1 2 3 4 5 6 7 8)
	'("m" "kg" "s" "A" "K" "cd" "mol" "rad" "sr"))
  (format s "])"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod mult ((x number) (y vd) )
  (make-instance 'vd :d-lst (d-lst y) :val (* x (vd-val y))))

(defmethod mult ( (y vd) (x number) )
  (make-instance 'vd :d-lst (d-lst y) :val (* x (vd-val y))))

(defmethod mult ((x vd) (y vd) )
  (let
      ((rez (make-instance 'vd)))
    (setf (vd-val rez) (* (vd-val x) (vd-val y))
	  (d-lst rez) (mapcar #'+ (d-lst x) (d-lst y)))
    rez))

(defmethod m-mult ((x vd) &rest args)
  (let
      ((rez x ))
    (dolist (y args)
      (setf rez (mult rez y))
      )
    rez))

(defmethod div ((x vd) (y vd) )
  (let
      ((rez (make-instance 'vd)))
    (setf (vd-val rez) (/ (vd-val x) (vd-val y))
	  (d-lst rez) (mapcar #'- (d-lst x) (d-lst y)))
    rez))

(defmethod m-div ((x vd) &rest args)
  (let
      ((rez x ))
    (dolist (y args)
      (setf rez (div rez y))
      )
    rez))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *nm-vl* (make-hash-table :test #'equal)
  "Задает соответствие сроки, обозначающей размерность значению."
  )

(mapcar
 #'(lambda (el)
     (setf (gethash (first el) *nm-vl*) (eval (second el))))
 '(
   ("m"   (make-instance 'vd :d-lst '( 1  0  0  0  0  0  0  0  0 ) :val 1))
   ("g"   (make-instance 'vd :d-lst '( 0  1  0  0  0  0  0  0  0 ) :val 1/1000))
   ("s"   (make-instance 'vd :d-lst '( 0  0  1  0  0  0  0  0  0 ) :val 1))
   ("A"   (make-instance 'vd :d-lst '( 0  0  0  1  0  0  0  0  0 ) :val 1))
   ("K"   (make-instance 'vd :d-lst '( 0  0  0  0  1  0  0  0  0 ) :val 1))
   ("cd"  (make-instance 'vd :d-lst '( 0  0  0  0  1  0  0  0  0 ) :val 1))
   ("mol" (make-instance 'vd :d-lst '( 0  0  0  0  0  0  1  0  0 ) :val 1))
   ("rad" (make-instance 'vd :d-lst '( 0  0  0  0  0  0  0  1  0 ) :val 1))
   ("sr"  (make-instance 'vd :d-lst '( 0  0  0  0  0  0  0  0  1 ) :val 1))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ("Hz"  (make-instance 'vd :d-lst '( 0  0 -1  0  0  0  0  0  0 ) :val 1))
   ("N"   (make-instance 'vd :d-lst '( 1  1 -2  0  0  0  0  0  0 ) :val 1))
   ("Pa"  (make-instance 'vd :d-lst '(-1  1 -2  0  0  0  0  0  0 ) :val 1))
   ("J"   (make-instance 'vd :d-lst '( 2  1 -2  0  0  0  0  0  0 ) :val 1))
   ("W"   (make-instance 'vd :d-lst '( 2  1 -3  0  0  0  0  0  0 ) :val 1))
   ("C"   (make-instance 'vd :d-lst '( 0  0  1  1  0  0  0  0  0 ) :val 1))
   ("V"   (make-instance 'vd :d-lst '( 2  1 -3 -1  0  0  0  0  0 ) :val 1))
   ("Ω"   (make-instance 'vd :d-lst '( 2  1 -3 -2  0  0  0  0  0 ) :val 1))
   ("S"   (make-instance 'vd :d-lst '(-2 -1  3  2  0  0  0  0  0 ) :val 1))
   ("F"   (make-instance 'vd :d-lst '(-2 -1  4  2  0  0  0  0  0 ) :val 1))
   ("Wb"  (make-instance 'vd :d-lst '( 2  1 -2 -1  0  0  0  0  0 ) :val 1))
   ("H"   (make-instance 'vd :d-lst '( 2  1 -2  0 -2  0  0  0  0 ) :val 1))
   ("T"   (make-instance 'vd :d-lst '( 0  1 -2 -1  0  0  0  0  0 ) :val 1))
   ("lm"  (make-instance 'vd :d-lst '( 0  0  0  0  0  1  0  0  1 ) :val 1))
   ("lx"  (make-instance 'vd :d-lst '(-2  0  0  0  0  1  0  0  1 ) :val 1))
   ("Bq"  (make-instance 'vd :d-lst '( 0  0 -1  0  0  0  0  0  0 ) :val 1))
   ("Gy"  (make-instance 'vd :d-lst '( 2  0 -2  0  0  0  0  0  0 ) :val 1))
   ("Sv"  (make-instance 'vd :d-lst '( 2  0 -2  0  0  0  0  0  0 ) :val 1))

   ))

(defparameter *nm-vl-ru* (make-hash-table :test #'equal)
  "Задает соответствие сроки, обозначающей размерность значению."
  )

(mapcar
 #'(lambda (el)
     (setf (gethash (first el) *nm-vl-ru*) (gethash (second el) *nm-vl*) ))
 '(("м" "m")
   ("г" "g")
   ("с" "s")
   ("А" "A")
   ("К" "K")
   ("кд" "cd")
   ("моль" "mol")
   ("рад" "rad")
   ("ср" "sr")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;   
   ("Гц" "Hz")
   ("Н"  "N")
   ("Па" "Pa")
   ("Дж" "J")
   ("Вт" "W")
   ("Кл" "C")
   ("В"  "V")
   ("Ом" "Ω")
   ("См" "S")
   ("Ф"  "F")
   ("Вб" "Wb")
   ("Гн" "H")
   ("Тл" "T")
   ("лм" "lm")
   ("лк" "lx")
   ("Бк" "Bq")
   ("Гр" "Gy")
   ("Зв" "Sv")
   ))



'(("tf"    (mult 9806.65 (gethash "N" *nm-vl*)))
  ("gf"    (mult 9.80665e-3 (gethash "N" *nm-vl*)))
  ("cal"   (mult 4.1868 (gethash "J" *nm-vl*)))
  ("St"    (make-instance 'vd :val 1.0e-4 :d-lst ( 2  0  -1  0  0  0  0  0  0 )))

  ("m_Hg"  (mult 133322.0 (gethash "Pa" *nm-vl*)))
  ("m_H2O" (mult 9806.65 (gethash "Pa" *nm-vl*)))
  ("bar"  (mult 100000 (gethash "Pa" *nm-vl*)))
  ("atm"  (mult 101325 (gethash "Pa" *nm-vl*)))
  ("Torr" (mult 101325/760 (gethash "Pa" *nm-vl*)))

  ("ua"    (mult 1.49597870e11 (gethash "m" *nm-vl*)))
  ("Å"     (mult 1.0e-10 (gethash "m" *nm-vl*)))
  ("t"     (mult (* 1000 1000) (gethash "g" *nm-vl*)))
  ("u"     (mult (* 1000 1.6605402e-27) (gethash "g" *nm-vl*)))
  ("eV"    (mult 1.60217733e-19 (gethash "J" *nm-vl*)))
  ("min"   (mult 60    (gethash "s" *nm-vl*)))
  ("h"     (mult 3600  (gethash "s" *nm-vl*)))
  ("d"     (mult 86400 (gethash "s" *nm-vl*)))
  ("a"     (mult 100   (m-mult (gethash "m" *nm-vl*)(gethash "m" *nm-vl*))))
  ("b"     (mult 1e-28 (m-mult (gethash "m" *nm-vl*)(gethash "m" *nm-vl*))))

  ("knot" (mult 1852/3600 (make-instance 'vd :val 1 :d-lst '(1 0 -1 0 0 0 0 0 0))))
  ("Gal"  (mult 1.0e-2    (make-instance 'vd :val 1 :d-lst '(1 0 -2 0 0 0 0 0 0))))

  ("kat" (make-instance 'vd :val 1 :d-lst '(0 0 -1 0 0 0 1 0 0))))



 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defparameter v1 (make-instance 'vd :val 5.0 :d-lst (list 1 0 0 0 0 0 0 0 0) ))
(defparameter v2 (make-instance 'vd :val 6.0 :d-lst (list 0 0 1 0 0 0 0 0 0) ))

(m-mult v1 v1 v1)
(m-div v1 v2 v2)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun K->C(K)
  "Перевод градусов значения, заданного в градусах Кельвина, в градусы Цельсия."
  (- K 273.15))

(defun C->K(C)
  "Перевод градусов значения, заданного в градусах Цельсия, в градусы Кельвина."
  (+ C 273.15))

(defun k->M (k)
"Перевод значения с приставкой кило в число с приставкой мега"
(* 0.001 k))

(defun M->k (M)
  "Перевод значения с приставкой мега в число с приставкой кило"
  (* 1000.0 M))

(defun kgs/cm2->Pa (kgs/cm2)
  "Переводит значение давления, заданное в kgs/cm2, в Pa."
  (* 9.8065 10000.0 kgs/cm2))

(defun Pa->kgs/cm2 (Pa)
  "Переводит значение давления, заданное в Pa, в kgs/cm2."
  (/ Pa 9.8065 10000.0))
