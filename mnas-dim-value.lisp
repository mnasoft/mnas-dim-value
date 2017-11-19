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
  (let ((rez (make-instance 'vd)))
    (setf (vd-val rez) (/ (vd-val x) (vd-val y))
	  (d-lst rez) (mapcar #'- (d-lst x) (d-lst y)))
    rez))

(defmethod m-div ((x vd) &rest args)
  (let ((rez x ))
    (dolist (y args)
      (setf rez (div rez y)))
    rez))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(progn
  (defvar |m|   (make-instance 'vd :d-lst '( 1  0  0  0  0  0  0  0  0 ) :val 1) "метр")
  (defvar |kg|  (make-instance 'vd :d-lst '( 0  1  0  0  0  0  0  0  0 ) :val 1) "килограмм")
  (defvar |s|   (make-instance 'vd :d-lst '( 0  0  1  0  0  0  0  0  0 ) :val 1) "секунда")
  (defvar |A|   (make-instance 'vd :d-lst '( 0  0  0  1  0  0  0  0  0 ) :val 1) "ампер")
  (defvar |K|   (make-instance 'vd :d-lst '( 0  0  0  0  1  0  0  0  0 ) :val 1) "кельвин") 
  (defvar |cd|  (make-instance 'vd :d-lst '( 0  0  0  0  0  1  0  0  0 ) :val 1) "кандела")
  (defvar |mol| (make-instance 'vd :d-lst '( 0  0  0  0  0  0  1  0  0 ) :val 1) "моль")
  (defvar |rad| (make-instance 'vd :d-lst '( 0  0  0  0  0  0  0  1  0 ) :val 1) "радиан")
  (defvar |sr|  (make-instance 'vd :d-lst '( 0  0  0  0  0  0  0  0  1 ) :val 1) "стерадиан"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(progn
   (defvar |Hz| (make-instance 'vd :d-lst '( 0  0 -1  0  0  0  0  0  0 ) :val 1) "герц")
   (defvar |N|  (make-instance 'vd :d-lst '( 1  1 -2  0  0  0  0  0  0 ) :val 1) "ньютон")
   (defvar |Pa| (make-instance 'vd :d-lst '(-1  1 -2  0  0  0  0  0  0 ) :val 1) "паскаль")
   (defvar |J|  (make-instance 'vd :d-lst '( 2  1 -2  0  0  0  0  0  0 ) :val 1) "джоуль")
   (defvar |W|  (make-instance 'vd :d-lst '( 2  1 -3  0  0  0  0  0  0 ) :val 1) "ватт")
   (defvar |C|  (make-instance 'vd :d-lst '( 0  0  1  1  0  0  0  0  0 ) :val 1) "кулон")
   (defvar |V|  (make-instance 'vd :d-lst '( 2  1 -3 -1  0  0  0  0  0 ) :val 1) "вольт")
   (defvar |Ω|  (make-instance 'vd :d-lst '( 2  1 -3 -2  0  0  0  0  0 ) :val 1) "ом")
   (defvar |S|  (make-instance 'vd :d-lst '(-2 -1  3  2  0  0  0  0  0 ) :val 1) "сименс")
   (defvar |F|  (make-instance 'vd :d-lst '(-2 -1  4  2  0  0  0  0  0 ) :val 1) "фарад")
   (defvar |Wb| (make-instance 'vd :d-lst '( 2  1 -2 -1  0  0  0  0  0 ) :val 1) "вебер")
   (defvar |H|  (make-instance 'vd :d-lst '( 2  1 -2  0 -2  0  0  0  0 ) :val 1) "генри")
   (defvar |Τ|  (make-instance 'vd :d-lst '( 0  1 -2 -1  0  0  0  0  0 ) :val 1) "тесла")
   (defvar |lm| (make-instance 'vd :d-lst '( 0  0  0  0  0  1  0  0  1 ) :val 1) "люмен")
   (defvar |lx| (make-instance 'vd :d-lst '(-2  0  0  0  0  1  0  0  1 ) :val 1) "люкс")
   (defvar |Bq| (make-instance 'vd :d-lst '( 0  0 -1  0  0  0  0  0  0 ) :val 1) "беккерель")
   (defvar |Gy| (make-instance 'vd :d-lst '( 2  0 -2  0  0  0  0  0  0 ) :val 1) "грэй")
   (defvar |Sv| (make-instance 'vd :d-lst '( 2  0 -2  0  0  0  0  0  0 ) :val 1) "зиверст"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(progn 
  (defvar *g*   (mult 9.80665 (m-div |m| |s| |s|))                                      "Ускорение свободного падения")
  (defvar *Gn*  (mult 6.6740831d-11 (m-div (m-mult |m| |m| |m|) (m-mult |kg| |s| |s|))) "Гравитационная постоянная")
  (defvar *C-0* (mult 273.15d0 |K|)                                                     "Ноль шкалы температур Цельсия")
  (defvar *V-0* (mult 22.41396213d-3 (div (m-mult |m| |m| |m|) |mol|))                  "Молекулярный объем идеального газа при нормальных условиях")
  (defvar *R-0* (mult 8.314459848d0 (m-div |J| |mol| |K|))                              "Универсальная газовая постоянная, Дж/моль*К")
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *nm-vl* (make-hash-table :test #'equal)
  "Задает соответствие сроки, обозначающей размерность значению."
  )

(mapcar
 #'(lambda (el)
     (setf (gethash (first el) *nm-vl*) (eval (second el))))
 '(
   ("m"   |m|)
   ("g"   (mult 1/1000 |kg|))
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
  ("in"    (mult 0.0254 |m|))("in"  (mult 0.0254 |m|))
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
