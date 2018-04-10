;;;; mnas-dim-value.lisp

(in-package #:mnas-dim-value)

;;; "mnas-dim-value" goes here. Hacks and glory await!

;;;;m kg s A K cd mol rad sr

(defclass vd ()
  ((val   :accessor vd-val  :initarg :val  :initform 0.0                        :documentation "Численное значение величины")
   (dims  :accessor vd-dims :initarg :dims :initform (list 0 0 0  0 0 0  0 0 0) :documentation "Список степеней размерности"))
  (:documentation "Число с размерностью."))

;;;;;;;;;;;;;;;;;;;;;;;;;

(defun vd (x &key (m 0) (kg 0) (s 0) (A 0) (K 0) (cd 0) (mol 0) (rad 0) (sr 0))
  (make-instance 'vd :val x :dims (list m kg s A K cd mol rad sr) ))

(defmethod same-dimension ((x vd) (y vd))
  "Проверяет два числа с размерностью на совпадение"
  (equal (vd-dims x) (vd-dims y)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod mult ((x number) (y vd)) (mult (vd x) y))

(defmethod mult ((y vd) (x number)) (mult (vd x) y))

(defmethod mult ((x vd) (y vd) )
  (let
      ((rez (make-instance 'vd)))
    (setf (vd-val rez) (* (vd-val x) (vd-val y))
	  (vd-dims rez) (mapcar #'+ (vd-dims x) (vd-dims y)))
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
	  (vd-dims rez) (mapcar #'- (vd-dims x) (vd-dims y)))
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
	  (vd-dims rez) (vd-dims x))
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
	  (vd-dims rez) (vd-dims x))
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
    (make-instance 'vd :val (- (vd-val x)) :dims (vd-dims x))))

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
  (defparameter |Τ|   (vd/ |Wb| |m| |m|)      "тесла") ;;;; τ
  (defparameter |H|   (vd/ |Wb| |A|)          "генри")

  (defparameter |lm|  (vd* |cd| |sr|)         "люмен")
  (defparameter |lx|  (vd/ |lm| |m| |m|)      "люкс")
  (defparameter |Bq|  |Hz|                    "беккерель")
  (defparameter |Gy|  (vd/ |J| |kg|)          "грэй")
  (defparameter |Sv|  (vd/ |J| |kg|)          "зиверст")
  (defparameter |kat| (vd/ |mol| |s|)         "катал")
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass nd ()
  ((quantity-name-en :accessor nd-quantity-name-en  :initarg :quantity-name-en :initform "" :documentation "Наименование величины английское. Например: длина")
   (quantity-name-ru :accessor nd-quantity-name-ru  :initarg :quantity-name-ru :initform "" :documentation "Наименование величины английское. Например: length")
   (unit-name-en     :accessor nd-unit-name-en      :initarg :unit-name-en     :initform "" :documentation "Наименование единицы английское. Например: metre") 
   (unit-name-ru     :accessor nd-unit-name-ru      :initarg :unit-name-ru     :initform "" :documentation "Наименование единицы русское. Например: метр") 
   (unit-symbol-en   :accessor nd-unit-symbol-en    :initarg :unit-symbol-en   :initform "" :documentation "Обозначение единицы английское. Например: m")
   (unit-symbol-ru   :accessor nd-unit-symbol-ru    :initarg :unit-symbol-ru   :initform "" :documentation "Обозначение единицы русское. Например: м")
   (dimension-symbol :accessor nd-dimension-symbol  :initarg :dimension-symbol :initform "" :documentation "Символ размерности. Например: L")
   (value            :accessor nd-value             :initarg :value            :initform 1  :documentation "Значение, выраженное в единицах СИ. Например: (vd 1 :m 1)"))
  (:documentation "Число с размерностью."))





