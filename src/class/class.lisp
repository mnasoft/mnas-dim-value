;;;; mnas-dim-value.lisp

(defpackage :mnas-dim-value/class
  (:use       #:cl )
  (:export <vd>
           <vd>-val
           <vd>-dims)
  (:export <nd>
           <nd>-quantity-name-en
           <nd>-quantity-name-ru
           <nd>-unit-name-en
           <nd>-unit-name-ru
           <nd>-unit-symbol-en
           <nd>-unit-symbol-ru
           <nd>-dimension-symbol
           <nd>-value
           <nd>-coeff 
           ))

(in-package :mnas-dim-value/class)

(defclass <vd> ()
  ((val      :accessor <vd>-val      :initarg :val  :initform 0.0                        :documentation "Численное значение величины")
   (dims     :accessor <vd>-dims     :initarg :dims :initform (list 0 0 0  0 0 0  0 0 0) :documentation "Список степеней размерности"))
  (:documentation "Число с размерностью (ЧсР)."))

(defparameter *vd-language* :en
  "Язык (member :en :ru)")

(defvar +vd-names-en+ '("m" "kg" "s" "A" "K" "cd" "mol"  "rad" "sr"))

(defvar +vd-names-ru+ '("м" "кг" "с" "А" "К" "кд" "моль" "рад" "ср"))

(defun vd-names ()
  (cond
    ((eq *vd-language* :ru) +vd-names-ru+)
    (t                      +vd-names-en+)))

(defmethod print-object ((x <vd>) o-s)
  (progn (format o-s "~S " (<vd>-val x))
	       (let ((st+ nil)
		     (st- nil))
		 (map nil
		      #'(lambda (v d)
			  (cond
			    ((< 1  v) (push (format nil "~A^~A" d v      ) st+))
			    ((= 1  v) (push (format nil "~A"    d        ) st+))
     			    ((= v -1) (push (format nil "~A"    d        ) st-))
			    ((< v -1) (push (format nil "~A^~A" d (abs v)) st-))))
		      (<vd>-dims x) (vd-names))
		 (cond 
		   ((and st+ (null st-)) (format o-s "[~{~A~^*~}]"           (nreverse st+) ))
		   ((and st+ st-)        (format o-s "[~{~A~^*~}/~{~A~^*~}]" (nreverse st+) (nreverse st-)))
		   ((and (null st+) st-) (format o-s "[1/~{~A~^*~}]"         (nreverse st-)))))))

#+nil
(defmethod print-object ((x <vd>) o-s)
  (multiple-value-bind (dimens find) (gethash (<vd>-dims x) (dim->unit-symbol))
    (if find
	(format o-s "~S [~A]" (<vd>-val x) dimens)
	(progn (format o-s "~S " (<vd>-val x))
	       (let ((st+ nil)
		     (st- nil))
		 (map nil
		      #'(lambda (v d)
			  (cond
			    ((< 1  v) (push (format nil "~A^~A" d v) st+))
			    ((= 1  v) (push (format nil "~A"    d  ) st+))
     			    ((= v -1) (push (format nil "~A"    d  ) st-))
			    ((< v -1) (push (format nil "~A^~A" d v) st-))))
		      (<vd>-dims x) (vd-names))
		 (cond 
		   ((and st+ (null st-)) (format o-s "[~{~A~^*~}]"           (nreverse st+) ))
		   ((and st+ st-)        (format o-s "[~{~A~^*~}/~{~A~^*~}]" (nreverse st+) (nreverse st-)))
		   ((and (null st+) st-) (format o-s "[1/~{~A~^*~}]"         (nreverse st-)))))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass <nd> ()
  ((quantity-name-en :accessor <nd>-quantity-name-en  :initarg :quantity-name-en :initform "" :documentation "Наименование величины английское. Например: length")
   (quantity-name-ru :accessor <nd>-quantity-name-ru  :initarg :quantity-name-ru :initform "" :documentation "Наименование величины русское. Например: длина")
   (unit-name-en     :accessor <nd>-unit-name-en      :initarg :unit-name-en     :initform "" :documentation "Наименование единицы английское. Например: metre") 
   (unit-name-ru     :accessor <nd>-unit-name-ru      :initarg :unit-name-ru     :initform "" :documentation "Наименование единицы русское. Например: метр") 
   (unit-symbol-en   :accessor <nd>-unit-symbol-en    :initarg :unit-symbol-en   :initform "" :documentation "Обозначение единицы английское. Например: m")
   (unit-symbol-ru   :accessor <nd>-unit-symbol-ru    :initarg :unit-symbol-ru   :initform "" :documentation "Обозначение единицы русское. Например: м")
   (dimension-symbol :accessor <nd>-dimension-symbol  :initarg :dimension-symbol :initform "" :documentation "Символ размерности. Например: L")
   (value            :accessor <nd>-value             :initarg :value            :initform 1  :documentation "Значение, выраженное в единицах СИ. Например: (vd 1 :m 1)")
   (coeff            :accessor <nd>-coeff             :initarg :coeff :initform '((-24 24))   :documentation "Список диапазонов разрешенных степеней множителей для данной величины системы СИ"))
  (:documentation "Величина с размерностью."))

(defmethod print-object ((obj <nd>) o-s)
  (print-unreadable-object (obj o-s :type t :identity nil)
    (format o-s "~&~4t q-en:~A" (<nd>-quantity-name-en obj))
    (format o-s "~&~4t q-ru:~A" (<nd>-quantity-name-ru obj))
    (format o-s "~&~4t u-en:~A" (<nd>-unit-name-en     obj))
    (format o-s "~&~4t u-ru:~A" (<nd>-unit-name-ru     obj))
    (format o-s "~&~4t s-en:~A" (<nd>-unit-symbol-en   obj))
    (format o-s "~&~4t s-ru:~A" (<nd>-unit-symbol-ru   obj))
    (format o-s "~&~4t dim:~A"  (<nd>-dimension-symbol obj))
    (format o-s "~&~4t val:~A"  (<nd>-value            obj))
    (format o-s "~&~4t c:~A"    (<nd>-coeff            obj))))
