;;;; mnas-dim-value.lisp

(in-package #:mnas-dim-value)

;;; "mnas-dim-value" goes here. Hacks and glory await!

;;;;m kg s A K cd mol rad sr

(defclass vd ()
  ((val   :accessor vd-val :initarg :val :initform 0.0)
   (d-lst :accessor d-lst :initarg :d-lst :initform (list 0 0 0  0 0 0  0 0 0) )
   ))

;;;;;;;;;;;;;;;;;;;;;;;;;

(defun vd (x &key (m 0) (kg 0) (s 0) (A 0) (K 0) (cd 0) (mol 0) (rad 0) (sr 0))
  (make-instance 'vd :val x :d-lst (list m kg s A K cd mol rad sr) ))

(defmethod same-dimension ((x vd) (y vd))
  "Проверяет два числа с размерностью на совпадение"
  (equal (d-lst x) (d-lst y)))

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
  (defparameter |m|   (vd 1 :m   1) "метр")
  (defparameter |kg|  (vd 1 :kg  1) "килограмм")
  (defparameter |s|   (vd 1 :s   1) "секунда")
  (defparameter |A|   (vd 1 :A   1) "ампер")
  (defparameter |K|   (vd 1 :K   1) "кельвин") 
  (defparameter |cd|  (vd 1 :cd  1) "кандела")
  (defparameter |mol| (vd 1 :mol 1) "моль")
  (defparameter |rad| (vd 1 :rad 1) "радиан")
  (defparameter |sr|  (vd 1 :sr  1) "стерадиан"))

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

