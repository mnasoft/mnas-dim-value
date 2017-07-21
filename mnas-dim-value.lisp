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

(defvar |*vd-m*| (make-instance 'vd :val 1 :d-lst  '(1 0 0 0 0 0 0 0 0)) "Возвращает число равное 1[m]. Пример использования: *vd-m*")

(defvar |*vd-mm*| (make-instance 'vd :val 1/1000 :d-lst  '(1 0 0 0 0 0 0 0 0)) "Возвращает число равное 1[mm]. Пример использования: *vd-mm*")

(defvar |*vd-km*| (make-instance 'vd :val 1000 :d-lst  '(1 0 0 0 0 0 0 0 0)) "Возвращает число равное 1[km]. Пример использования: *vd-km*")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar |*vd-kg*|  (make-instance 'vd :val 1 :d-lst  '( 0  1  0  0  0  0  0  0  0 ))  "Возвращает число равное 1[kg]. Пример использования: |*vd-kg*|")

(defvar |*vd-g*| (make-instance 'vd :val 1/1000 :d-lst  '( 0  1  0  0  0  0  0  0  0 ))  "Возвращает число равное 1[g]. Пример использования: |*vd-g*|")

(defvar |*vd-t*| (make-instance 'vd :val 1000 :d-lst  '( 0  1  0  0  0  0  0  0  0 ))  "Возвращает число равное 1[t]. Пример использования: |*vd-t*|")
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar |*vd-s*| (make-instance 'vd :val 1 :d-lst  '( 0  0  1  0  0  0  0  0  0 ))  "Возвращает число равное 1[s]. Пример использования: |*vd-s*|")

(defvar |*vd-min*| (make-instance 'vd :val 60 :d-lst  '( 0  0  1  0  0  0  0  0  0 ))  "Возвращает число равное 1[s]. Пример использования: |*vd-min*|")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun |get_A| ()
  "Возвращает число равное 1[A].
Пример использования:
(|get_A|)"
  (make-instance 'vd :val 1.0 :d-lst  '( 0  0  0  1  0  0  0  0  0 )))

(defun |get_K| ()
  "Возвращает число равное 1[K].
Пример использования:
(|get_K|)"
  (make-instance 'vd :val 1.0 :d-lst  '( 0  0  0  0  1  0  0  0  0 )))

(defun |get_cd| ()
  "Возвращает число равное 1[cd].
Пример использования:
(|get_cd|)"
  (make-instance 'vd :val 1.0 :d-lst  '( 0  0  0  0  0  1  0  0  0 )))

(defun |get_mol| ()
  "Возвращает число равное 1[mol].
Пример использования:
(|get_mol|)"
  (make-instance 'vd :val 1.0 :d-lst  '( 0  0  0  0  0  0  1  0  0 )))

(defun |get_rad| ()
  "Возвращает число равное 1[rad].
Пример использования:
(|get_rad|)"
  (make-instance 'vd :val 1.0 :d-lst  '( 0  0  0  0  0  0  0  1  0 )))

(defun |get_sr| ()
  "Возвращает число равное 1[rad].
Пример использования:
(|get_sr|)"
  (make-instance 'vd :val 1.0 :d-lst  '( 0  0  0  0  0  0  0  0  1 )))

(defun |get_Hz| ()
  "Возвращает число равное 1[Hz].
Пример использования:
(|get_Hz|)"
  (make-instance 'vd :val 1.0 :d-lst  '( 0  0  -1  0  0  0  0  0  0 )))

(defun |get_N| ()
  "Возвращает число равное 1[N].
Пример использования:
(|get_N|)"
  (make-instance 'vd :val 1.0 :d-lst  '( 1  1  -2  0  0  0  0  0  0 )))

(defun |get_Pa| ()
  "Возвращает число равное 1[Pa].
Пример использования:
(|get_Pa|)"
  (make-instance 'vd :val 1.0 :d-lst  '( -1  1  -2  0  0  0  0  0  0 )))

(defun |get_J| ()
  "Возвращает число равное 1[J].
Пример использования:
(|get_J|)"
  (make-instance 'vd :val 1.0 :d-lst  '( 2  1  -2  0  0  0  0  0  0 )))

(defun |get_W| ()
  "Возвращает число равное 1[W].
Пример использования:
(|get_W|)"
  (make-instance 'vd :val 1.0 :d-lst  '( 2  1  -3  0  0  0  0  0  0 )))

(defun |get_C| ()
  "Возвращает число равное 1[C].
Пример использования:
(|get_C|)"
  (make-instance 'vd :val 1.0 :d-lst  '( 0  0  1  1  0  0  0  0  0 )))


(defun |get_V| ()
  "Возвращает число равное 1[V].
Пример использования:
(|get_V|)"
  (make-instance 'vd :val 1.0 :d-lst  '( 2  1  -3  -1  0  0  0  0  0 )))

(defun |get_Om| ()
  "Возвращает число равное 1[Om].
Пример использования:
(|get_Om|)"
  (make-instance 'vd :val 1.0 :d-lst  '( 2  1  -3  -2  0  0  0  0  0 )))

(defun |get_S| ()
  "Возвращает число равное 1[S].
Пример использования:
(|get_S|)"
  (make-instance 'vd :val 1.0 :d-lst  '( -2  -1  3  2  0  0  0  0  0 )))

(defun |get_F| ()
  "Возвращает число равное 1[F].
Пример использования:
(|get_F|)"
  (make-instance 'vd :val 1.0 :d-lst  '( -2  -1  4  2  0  0  0  0  0 )))

(defun |get_Wb| ()
  "Возвращает число равное 1[Wb].
Пример использования:
(|get_Wb|)"
  (make-instance 'vd :val 1.0 :d-lst  '( 2  1  -2  -1  0  0  0  0  0 )))

(defun |get_H| ()
  "Возвращает число равное 1[H].
Пример использования:
(|get_H|)"
  (make-instance 'vd :val 1.0 :d-lst  '( 2  1  -2  0  -2  0  0  0  0 )))

(defun |get_T| ()
  "Возвращает число равное 1[T].
Пример использования:
(|get_T|)"
  (make-instance 'vd :val 1.0 :d-lst  '( 0  1  -2  -1  0  0  0  0  0 )))

(defun |get_lm| ()
  "Возвращает число равное 1[lm].
Пример использования:
(|get_lm|)"
  (make-instance 'vd :val 1.0 :d-lst  '( 0  0  0  0  0  1  0  0  1 )))

(defun |get_lx| ()
  "Возвращает число равное 1[lx].
Пример использования:
(|get_lx|)"
  (make-instance 'vd :val 1.0 :d-lst  '( -2  0  0  0  0  1  0  0  1 )))

(defun |get_Bq| ()
  "Возвращает число равное 1[Bq].
Пример использования:
(|get_Bq|)"
  (make-instance 'vd :val 1.0 :d-lst  '( 0  0  -1  0  0  0  0  0  0 )))

(defun |get_Gy| ()
  "Возвращает число равное 1[Gy].
Пример использования:
(|get_Gy|)"
  (make-instance 'vd :val 1.0 :d-lst  '( 2  0  -2  0  0  0  0  0  0 )))

(defun |get_Sv| ()
  "Возвращает число равное 1[Sv].
Пример использования:
(|get_Sv|)"
  (make-instance 'vd :val 1.0 :d-lst  '( 2  0  -2  0  0  0  0  0  0 )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


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
