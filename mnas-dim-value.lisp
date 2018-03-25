;;;; mnas-dim-value.lisp

(in-package #:mnas-dim-value)

;;; "mnas-dim-value" goes here. Hacks and glory await!

;;;;m kg s A K cd mol rad sr

(defclass vd ()
  ((val   :accessor vd-val :initarg :val :initform 0.0)
   (d-lst :accessor d-lst :initarg :d-lst :initform (list 0 0 0  0 0 0  0 0 0) )
   ))

(defun vd (x &key (m 0) (kg 0) (s 0) (A 0) (K 0) (cd 0) (mol 0) (rad 0) (sr 0))
  (make-instance 'vd :val x :d-lst (list m kg s A K cd mol rad sr) ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod mult ((x number) (y vd)) (mult (vd x) y))

(defmethod mult ((y vd) (x number)) (mult (vd x) y))

(defmethod mult ((x vd) (y vd) )
  (let
      ((rez (make-instance 'vd)))
    (setf (vd-val rez) (* (vd-val x) (vd-val y))
	  (d-lst rez) (mapcar #'+ (d-lst x) (d-lst y)))
    rez))

(defmethod vd* ((x number) &rest args)
  (let ((rez (vd x) ))
    (dolist (y args)
      (setf rez (mult rez y)))
    rez))

(defmethod vd* ((x vd) &rest args)
  (let ((rez x))
    (dolist (y args)
      (setf rez (mult rez y)))
    rez))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod div ((x number) (y vd) ) (div (make-instance 'vd :val x) y))

(defmethod div ((y vd) (x number) ) (div y (make-instance 'vd :val x)))

(defmethod div ((x vd) (y vd) )
  (let ((rez (make-instance 'vd)))
    (setf (vd-val rez) (/ (vd-val x) (vd-val y))
	  (d-lst rez) (mapcar #'- (d-lst x) (d-lst y)))
    rez))

(defmethod vd/ ((x vd) &rest args)
  (if args
      (let ((rez x ))
	(dolist (y args)
	  (setf rez (div rez y)))
	rez)
      (div (vd 1) x)))

(defmethod vd/ ((x number) &rest args)
  (if args
      (let ((rez (vd x)))
	(dolist (y args)
	  (setf rez (div rez y)))
	rez)
      (div (vd 1) x)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod sum ((x number) (y vd) ) (sum (make-instance 'vd :val x) y))
 
(defmethod sum ((y vd) (x number) ) (sum y (make-instance 'vd :val x)))

(defmethod sum ((x vd) (y vd) )
  (let ((rez (make-instance 'vd)))
    (setf (vd-val rez) (+ (vd-val x) (vd-val y))
	  (d-lst rez) (d-lst x))
    rez))

(defmethod vd+ ((x number) &rest args)
  (let ((rez (vd x) ))
    (dolist (y args)
      (setf rez (sum rez y)))
    rez))

(defmethod vd+ ((x vd) &rest args)
  (let ((rez  x))
    (dolist (y args)
      (setf rez (sum rez y)))
    rez))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod diff ((x number) (y vd) ) (diff (make-instance 'vd :val x) y))
 
(defmethod diff ((y vd) (x number) ) (diff y (make-instance 'vd :val x)))

(defmethod diff ((x vd) (y vd) )
  (let ((rez (make-instance 'vd)))
    (setf (vd-val rez) (- (vd-val x) (vd-val y))
	  (d-lst rez) (d-lst x))
    rez))

(defmethod vd- ((x number) &rest args)
  (if args
    (let ((rez (vd x)))
      (dolist (y args)
	(setf rez (diff rez y)))
      rez)
    (vd (- x))))
  
(defmethod vd- ((x vd) &rest args)
  (if args
    (let ((rez x))
      (dolist (y args)
	(setf rez (diff rez y)))
      rez)
    (make-instance 'vd :val (- (vd-val x)) :d-lst (d-lst x))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(progn
  (defvar |m|   (vd 1 :m   1) "метр")
  (defvar |kg|  (vd 1 :kg  1) "килограмм")
  (defvar |s|   (vd 1 :s   1) "секунда")
  (defvar |A|   (vd 1 :A   1) "ампер")
  (defvar |K|   (vd 1 :K   1) "кельвин") 
  (defvar |cd|  (vd 1 :cd  1) "кандела")
  (defvar |mol| (vd 1 :mol 1) "моль")
  (defvar |rad| (vd 1 :rad 1) "радиан")
  (defvar |sr|  (vd 1 :sr  1) "стерадиан"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(progn
  (defparameter |Hz|  (vd/ |s|)               "герц")
  (defparameter |N|   (vd 1 :m 1 :kg 1 :s -2) "ньютон")
  (defparameter |Pa|  (vd/ |N| |m| |m|)       "паскаль")
  (defparameter |J|   (vd* |N| |m|)           "джоуль")
  (defparameter |W|   (vd/ |J| |s|)           "ватт")
  (defparameter |C|   (vd* |A| |s|)           "кулон")
  (defparameter |V|   (vd/ |W| |A|)           "вольт")
  (defparameter |F|   (vd/ |C| |V|)           "фарад")
  (defparameter |Ω|   (vd/ |V| |A|)           "ом")
  (defparameter |S|   (vd/ |Ω|)               "сименс")

  (defparameter |Wb|  (vd* |V| |s|)           "вебер")
  (defparameter |Τ|   (vd/ |Wb| |m| |m|)      "тесла")
  (defparameter |H|   (vd/ |Wb| |A|)          "генри")

  (defparameter |lm|  (vd* |cd| |sr|)         "люмен")
  (defparameter |lx|  (vd/ |lm| |m| |m|)      "люкс")
  (defparameter |Bq|  |Hz|                    "беккерель")
  (defparameter |Gy|  (vd/ |J| |kg|)          "грэй")
  (defparameter |Sv|  (vd/ |J| |kg|)          "зиверст")
  (defparameter |kat| (vd/ |mol| |s|)         "катал")
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *nm-vl* (make-hash-table :test #'equal)
  "Задает соответствие сроки, обозначающей размерность значению."
  )

(mapcar
 #'(lambda (el)
     (setf (gethash (first el) *nm-vl*) (eval (second el))))
 '(
   ("m"   |m|)
   ("g"   (vd/ |kg| 1000))
   ("s"   |s|)
   ("A"   |A|)
   ("K"   |K|)
   ("cd"  |cd|)
   ("mol" |mol|)
   ("rad" |rad|)
   ("sr"  |sr|)
   
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ("Hz" |Hz|) ("N"  |N|) ("Pa" |Pa|) ("J"  |J|)  ("W"  |W|)  ("C"  |C|)   ("V"   |V|)   ("Ω"   |Ω|)    ("S"  |S|)
   ("F"  |F|)  ("Wb" |Wb|) ("H"  |H|) ("T"  |Τ|)  ("lm" |lm|) ("lx" |lx|)  ("Bq" |Bq|)   ("Gy"  |Gy|)   ("Sv" |Sv|)))

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
   ("Гц" "Hz") ("Н"  "N")  ("Па" "Pa") ("Дж" "J") ("Вт" "W")  ("Кл" "C")  ("В"  "V")  ("Ом" "Ω")  ("См" "S")
   ("Ф"  "F")  ("Вб" "Wb") ("Гн" "H")  ("Тл" "T") ("лм" "lm") ("лк" "lx") ("Бк" "Bq") ("Гр" "Gy") ("Зв" "Sv")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
))


'(
  ("ua"    (mult 1.49597870e11 |m|))
  ("in"    (mult 0.0254 |m|))
  ("in"    (mult 0.0254 |m|))
  ("Å"     (mult 1.0e-10       |m|))

  ("t"     (mult 1000 |kg|))
  ("u"     (mult 1.6605402e-27 |kg|))
  ("lb"    (mult 0.45359237             |kg|))

  ("tf"    (mult 1000   (m-mult *g* |kg|)))
  ("gf"    (mult 1/1000 (m-mult *g* |kg|)))
  ("lbf"   (mult 0.45359237 (m-mult *g* |kg| )))
  
  ("cal"   (mult 4.1868 |J|))
  
  ("St"    (make-instance 'vd :val 1/10000 :d-lst '( 2  0  -1  0  0  0  0  0  0 )))

  ("m_Hg"  (mult 133322.0   |Pa|))
  ("m_H2O" (mult 9806.65    |Pa|))
  ("bar"   (mult 100000     |Pa|))
  ("atm"   (mult 101325     |Pa|))
  ("Torr"  (mult (/ 101325.0 760.0) |Pa|))
  ("psi"   (m-div (mult 0.45359237 (m-mult *g* |kg| )) (mult 0.0254 |m|) (mult 0.0254 |m|)))
  
  ("eV"    (mult 1.60217733e-19 |J|))
  
  ("min"   (mult 60    |s|))
  ("h"     (mult 3600  |s|))
  ("d"     (mult 86400 |s|))
  
  ("a"     (mult 100   (m-mult |m| |m|)))
  ("b"     (mult 1e-28 (m-mult |m| |m|)))

  ("knot" (mult (/ 1852.0 3600) (m-div |m| |s|)))
  
  ("Gal"  (mult 1.0e-2    (make-instance 'vd :val 1 :d-lst '(1 0 -2 0 0 0 0 0 0))))

  ("kat" (make-instance 'vd :val 1 :d-lst '(0 0 -1 0 0 0 1 0 0))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
