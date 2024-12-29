;;;; test.lisp

(in-package :mdv)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Operations ------------------------------------------------------------------------

;; Q+
(defqop q+ (&rest numbers)
  (if numbers
      (let ((converted-numbers (append (list (first numbers))
                                       (loop for n in (rest numbers) collect
                                            (convert-unit n (unit (first numbers)))))))
        (make-quantity% :value (apply #'+ (mapcar #'value converted-numbers))
                        :error (apply #'py+ (mapcar #'aerr converted-numbers))
                        :unit (copy-unit (unit (first converted-numbers)))))
      (make-quantity%)))
(export 'q+)

;; Q-
(defqop q- (number &rest more-numbers)
  (let ((converted-numbers (append (list number)
                                   (loop for n in more-numbers collect (convert-unit n (unit number))))))
    (make-quantity% :value (apply #'- (mapcar #'value converted-numbers))
                    :error (apply #'py+ (mapcar #'aerr converted-numbers))
                    :unit (copy-unit (unit (first converted-numbers))))))
(export 'q-)

;; Q*
(defqop q* (&rest numbers)
  (if numbers
      (let ((value (apply #'* (mapcar #'value numbers))))
        (make-quantity% :value value
                        :error (if (zerop value) 0 (* (abs value) (apply #'py+ (mapcar #'rerr numbers))))
                        :unit (apply #'multiply-units (mapcar #'unit numbers))))
      (make-quantity% :value 1)))
(export 'q*)

;; Q/
(defqop q/ (number &rest more-numbers)
  (let ((value (apply #'/ (value number) (mapcar #'value more-numbers))))
    (make-quantity% :value value
                    :error (if (zerop value) 0 (* (abs value) (apply #'py+ (rerr number) (mapcar #'rerr more-numbers))))
                    :unit (apply #'divide-units (unit number) (mapcar #'unit more-numbers)))))
(export 'q/)

;; QPOW: Power function (integer exponent, real result)
(defqop qpow (base power!u)
  (unless (errorlessp power)
    (restart-case (f-error operation-undefined-error () "Operation (QPOW BASE POWER) undefined if POWER is quantity with uncertainty/error.")
      (drop-uncertainty () :report "Drop the error/uncertainty from POWER." (setf power (make-quantity% :value (value power))))))
  (unless (integerp (value power))
    (f-error operation-undefined-error () "Operation (QPOW BASE POWER) undefined if POWER has non-integer value."))
  (make-quantity% :value (expt (value base) (value power)) :error (error-propagation base (if (zerop power) 0 (* power (expt base (1- power)))) power 0) :unit (power-unit (unit base) (value power))))
(export 'qpow)

;; QROOT: Nth root (integer degree, real result)
(defqop qroot (radicand degree!u)
  (unless (errorlessp degree)
    (restart-case (f-error operation-undefined-error () "Operation (QROOT RADICAND DEGREE) is undefined if DEGREE is quantity with uncertainty/error.")
      (drop-uncertainty () :report "Drop the error/uncertainty from DEGREE." (setf degree (make-quantity% :value (value degree))))))
  (unless (plusp (value degree))
    (f-error operation-undefined-error () "Operation (QROOT RADICAND DEGREE) undefined unless DEGREE is positive."))
  (when (and (minusp (value radicand)) (evenp (value degree)))
     (f-error operation-undefined-error () "Operation (QROOT RADICAND DEGREE) undefined if RADICAND has negative value and DEGREE is even."))
  (handler-case (multiple-value-bind (unit conv) (root-unit (unit radicand) (value degree))
                  (if (and (minusp (value radicand)) (oddp (value degree)))
                      (let ((val (* conv (- (expt (- (value radicand)) (/ (value degree)))))))
                        (make-quantity% :value val :error (error-propagation radicand (if (zerop radicand) 0 (/ val radicand degree)) degree 0) :unit unit))
                      (let ((val (* conv (if (= (value degree) 2) (sqrt (value radicand)) (expt (value radicand) (/ (value degree)))))))
                        (make-quantity% :value val :error (error-propagation radicand (if (zerop radicand) 0 (/ val radicand degree)) degree 0) :unit unit))))
    (operation-undefined-error () (f-error invalid-unit-operation-error () "The unit of RADICAND in operation (QROOT RADICAND DEGREE) must have a power ~
                                                                            that is a multiple of DEGREE." degree (str-unit (unit radicand))))))
(export 'qroot)

;; QSQRT: Square root (real result)
(defun qsqrt (quantity)
  "Computes the square root of a given quantity. Result must always be real."
  (qroot quantity 2))
(export 'qsqrt)

;; QEXP: Exponentiation, base e
(defqop qexp (exponent!u)
  (let ((val (exp (value exponent))))
    (make-quantity% :value val :error (abs (* val (aerr exponent))))))
(export 'qexp)

;; Exponentiation, base given
(defqop qexpt (base exponent!u)
  (when (and (has-error-p exponent) (has-unit-p base))
    (restart-case (f-error invalid-unit-operation-error () "BASE in operation (QEXPT BASE EXPONENT) must be unitless if EXPONENT is quantity with uncertainty/error.")
      (drop-uncertainty () :report "Drop the error/uncertainty from EXPONENT." (setf exponent (make-quantity% :value (value exponent))))
      (drop-unit () :report "Drop the unit from BASE." (setf base (make-quantity% :value (value base) :error (error-direct base))))))
  (when (and (has-unit-p base) (floatp (value exponent)))
    (restart-case (f-error invalid-unit-operation-error () "BASE in operation (QEXPT BASE EXPONENT) must be unitless if EXPONENT has a floating point value.")
      (drop-unit () :report "Drop the unit from BASE." (setf base (make-quantity% :value (value base) :error (error-direct base))))))
  (when (and (minusp (value base)) (has-error-p exponent))
    (restart-case (f-error error-propagation-error () "Error propagation undefined for operation (QEXPT BASE EXPONENT)~
                                                      if BASE has negative value and EXPONENT is quantity with uncertainty/error.")
      (drop-uncertainty () :report "Drop the error/uncertainty from EXPONENT." (setf exponent (make-quantity% :value (value exponent))))))
  (cond
    ((and (integerp (value exponent)) (errorlessp exponent)) (qpow base exponent))
    ((and (typep (value exponent) 'ratio) (errorlessp exponent)) (qroot (qpow base (numerator (value exponent))) (denominator (value exponent))))
    (t (let ((val (expt (value base) (value exponent))))
         (make-quantity% :value val :error (error-propagation base (if (zerop base) 0 (* exponent (expt base (1- exponent)))) exponent (if (zerop base) 0 (/ val (log base)))))))))
(export 'qexpt)

(defqop qln (number!u)
  (make-quantity% :value (log (value number)) :error (error-propagation number (/ number))))
(export 'qln)

(defqop qlog (number!u base!u)
  (make-quantity% :value (log (value number) (value base)) :error (error-propagation number (/ 1 number (log base)) base (/ (log number) base (expt (log base) 2)))))
(export 'qlog)

(defqop qsin (number!u)
  (make-quantity% :value (sin (value number)) :error (error-propagation number (cos number))))
(export 'qsin)

(defqop qcos (number!u)
  (make-quantity% :value (cos (value number)) :error (error-propagation number (sin number))))
(export 'qcos)

(defqop qtan (number!u)
  (make-quantity% :value (tan (value number)) :error (error-propagation number (/ (expt (cos number) 2)))))
(export 'qtan)

(defqop qasin (number!u)
  (make-quantity% :value (asin (value number)) :error (error-propagation number (/ (sqrt (- 1 (expt number 2)))))))
(export 'qasin)

(defqop qacos (number!u)
  (make-quantity% :value (acos (value number)) :error (error-propagation number (/ -1 (sqrt (- 1 (expt number 2)))))))
(export 'qacos)

(defqop qatan (number!u)
  (make-quantity% :value (atan (value number)) :error (error-propagation number (/ (1+ (expt number 2))))))
(export 'qatan)

(defqop qsinh (number!u)
  (make-quantity% :value (sinh (value number)) :error (error-propagation number (cosh number))))
(export 'qsinh)

(defqop qcosh (number!u)
  (make-quantity% :value (cosh (value number)) :error (error-propagation number (sinh number))))
(export 'qcosh)

(defqop qtanh (number!u)
  (make-quantity% :value (tanh (value number)) :error (error-propagation number (/ (expt (cosh number) 2)))))
(export 'qtanh)

(defqop qasinh (number!u)
  (make-quantity% :value (asinh (value number)) :error (error-propagation number (/ (sqrt (1+ (expt number 2)))))))
(export 'qasinh)

(defqop qacosh (number!u)
  (make-quantity% :value (acosh (value number)) :error (error-propagation number (/ 1 (sqrt (1+ number)) (sqrt (1- number))))))
(export 'qacosh)

(defqop qatanh (number!u)
  (make-quantity% :value (atanh (value number)) :error (error-propagation number (/ (- 1 (expt number 2))))))
(export 'qatanh)

(defqop qabs (number)
  (when (and (has-error-p number) (zerop (value number)))
    (restart-case (f-error error-propagation-error () "Error propagation undefined for operation (QABS NUMBER)~
                                                      if NUMBER has value zero with non-zero uncertainty/error.")
      (drop-uncertainty ()
        :report "Drop the error/uncertainty from NUMBER."
        (setf number (make-quantity% :value (value number) :unit (unit number))))))
  (make-quantity% :value (abs (value number)) :error (error-direct number) :unit (copy-unit (unit number))))
(export 'qabs)

;; Predicates/Tests -----------------------------------------------------------------

(defqop qequal (x y)
  (and (equal (value x) (value y)) (equal (error-direct x) (error-direct y)) (units-equal (unit x) (unit y))))
(export 'qequal)

(defqop qequalp (x y)
  (and (equalp (value x) (value y))
       (or (equalp (error-direct x) (error-direct y))
           (equalp (aerr x) (aerr y))
           (equalp (rerr x) (rerr y)))
       (units-equalp (unit x) (unit y))))
(export 'qequalp)

(defgeneric q= (x y &optional p-value) (:documentation "Determines whether the value of two quantities are equal."))
(defmethod q= ((x real) (y real) &optional p-value)
  (declare (ignore p-value))
  (= x y))
(defmethod q= ((x quantity) (y real) &optional (p-value 95/100))
  (unless (unitlessp x)
    (f-error invalid-unit-conversion-error () "The units ~a and ~a are incompatible under operation Q=." (str-unit (unit x)) (str-unit y)))
  ;; Quantity is unitless
  (if (errorlessp x)
      ;; Quantity has no uncertainty
      (= (value x) y)
      ;; Quantity has uncertainty
      (let* ((max-delta (* (aerr x) (confidence-interval p-value :two-sided t))) (delta (abs (- (value x) y))))
        ;; delta: the difference between the two values
        ;; max-delta: the range within which delta is acceptable
        (< delta max-delta))))
(defmethod q= ((x real) (y quantity) &optional (p-value 95/100))
  (q= y x p-value))
(defmethod q= ((x quantity) (y quantity) &optional (p-value 95/100))
  ;; Calculate the difference between the quantities. This may raise an error.
  (unless (units-convertible (unit x) (unit y))
    (f-error invalid-unit-conversion-error () "The units ~a and ~a are incompatible under operation Q=." (str-unit (unit x)) (str-unit (unit y))))
  (let ((val (q- y x)))
    (if (errorlessp val)
        ;; Difference has no uncertainty
        (= (value val) 0)
        ;; Difference has uncertainty
        (let ((confidence-interval (* (aerr val) (confidence-interval p-value :two-sided t))))
          (< (abs (value val)) confidence-interval)))))
(export 'q=)

(defun q/= (x y &optional (p-value 0.95))
  (not (q= x y p-value)))
(export 'q/=)

(defgeneric q< (x y &optional p-value) (:documentation "Determines whether the one quantity is less than another quantity"))
(defmethod q< ((x real) (y real) &optional p-value)
  (declare (ignore p-value))
  (< x y))
(defmethod q< ((x quantity) (y real) &optional (p-value 0.95))
  (unless (unitlessp x)
    (f-error invalid-unit-conversion-error () "The units ~a and ~a are incompatible under operation Q<." (str-unit (unit x)) (str-unit y)))
  ;; Quantity is unitless
  (if (errorlessp x)
      ;; Quantity has no uncertainty
      (< (value x) y)
      ;; Quantity has uncertainty
      (< (+ (value x) (* (aerr x) (confidence-interval p-value :two-sided nil))) y)))
(defmethod q< ((x real) (y quantity) &optional (p-value 0.95))
  (unless (unitlessp y)
    (f-error invalid-unit-conversion-error () "The units ~a and ~a are incompatible under operation Q<." (str-unit x) (str-unit (unit y))))
  ;; Quantity is unitless
  (if (errorlessp y)
      ;; Quantity has no uncertainty
      (< x (value y))
      ;; Quantity has uncertainty
      (< x (- (value y) (* (aerr y) (confidence-interval p-value :two-sided nil))))))
(defmethod q< ((x quantity) (y quantity) &optional (p-value 0.95))
  (unless (units-convertible (unit x) (unit y))
    (f-error invalid-unit-conversion-error () "The units ~a and ~a are incompatible under operation Q<." (str-unit (unit x)) (str-unit (unit y))))
  (let ((delta (q- y x)))
    (> (value delta) (* (aerr delta) (confidence-interval p-value :two-sided nil)))))
(export 'q<)

(defgeneric q<= (x y &optional p-value) (:documentation "Determines whether the one quantity is less or equal to another quantity"))
(defmethod q<= ((x real) (y real) &optional p-value)
  (declare (ignore p-value))
  (<= x y))
(defmethod q<= ((x quantity) (y real) &optional (p-value 0.95))
  (unless (unitlessp x)
    (f-error invalid-unit-conversion-error () "The units ~a and ~a are incompatible under operation Q<=." (str-unit (unit x)) (str-unit y)))
  ;; Quantity is unitless
  (if (errorlessp x)
      ;; Quantity has no uncertainty
      (<= (value x) y)
      ;; Quantity has uncertainty
      (<= (+ (value x) (* (aerr x) (confidence-interval p-value :two-sided nil))) y)))
(defmethod q<= ((x real) (y quantity) &optional (p-value 0.95))
  (unless (unitlessp y)
    (f-error invalid-unit-conversion-error () "The units ~a and ~a are incompatible under operation Q<=." (str-unit x) (str-unit (unit y))))
  ;; Quantity is unitless
  (if (errorlessp y)
      ;; Quantity has no uncertainty
      (<= x (value y))
      ;; Quantity has uncertainty
      (<= x (- (value y) (* (aerr y) (confidence-interval p-value :two-sided nil))))))
(defmethod q<= ((x quantity) (y quantity) &optional (p-value 0.95))
  (unless (units-convertible (unit x) (unit y))
    (f-error invalid-unit-conversion-error () "The units ~a and ~a are incompatible under operation Q<=." (str-unit (unit x)) (str-unit (unit y))))
  (let ((delta (q- y x)))
    (>= (value delta) (* (aerr delta) (confidence-interval p-value :two-sided nil)))))
(export 'q<=)

(defgeneric q> (x y &optional p-value) (:documentation "Determines whether the one quantity is greater than another quantity"))
(defmethod q> ((x real) (y real) &optional p-value)
  (declare (ignore p-value))
  (> x y))
(defmethod q> ((x quantity) (y real) &optional (p-value 0.95))
  (unless (unitlessp x)
    (f-error invalid-unit-conversion-error () "The units ~a and ~a are incompatible under operation Q>." (str-unit (unit x)) (str-unit y)))
  ;; Quantity is unitless
  (if (errorlessp x)
      ;; Quantity has no uncertainty
      (> (value x) y)
      ;; Quantity has uncertainty
      (> (- (value x) (* (aerr x) (confidence-interval p-value :two-sided nil))) y)))
(defmethod q> ((x real) (y quantity) &optional (p-value 0.95))
  (unless (unitlessp y)
    (f-error invalid-unit-conversion-error () "The units ~a and ~a are incompatible under operation Q>." (str-unit x) (str-unit (unit y))))
  ;; Quantity is unitless
  (if (errorlessp y)
      ;; Quantity has no uncertainty
      (> x (value y))
      ;; Quantity has uncertainty
      (> x (+ (value y) (* (aerr y) (confidence-interval p-value :two-sided nil))))))
(defmethod q> ((x quantity) (y quantity) &optional (p-value 0.95))
  (unless (units-convertible (unit x) (unit y))
    (f-error invalid-unit-conversion-error () "The units ~a and ~a are incompatible under operation Q>." (str-unit (unit x)) (str-unit (unit y))))
  (let ((delta (q- x y)))
    (> (value delta) (* (aerr delta) (confidence-interval p-value :two-sided nil)))))
(export 'q>)

(defgeneric q>= (x y &optional p-value) (:documentation "Determines whether the one quantity is greater or equal to another quantity"))
(defmethod q>= ((x real) (y real) &optional p-value)
  (declare (ignore p-value))
  (>= x y))
(defmethod q>= ((x quantity) (y real) &optional (p-value 0.95))
  (unless (unitlessp x)
    (f-error invalid-unit-conversion-error () "The units ~a and ~a are incompatible under operation Q>=." (str-unit (unit x)) (str-unit y)))
  ;; Quantity is unitless
  (if (errorlessp x)
      ;; Quantity has no uncertainty
      (>= (value x) y)
      ;; Quantity has uncertainty
      (>= (- (value x) (* (aerr x) (confidence-interval p-value :two-sided nil))) y)))
(defmethod q>= ((x real) (y quantity) &optional (p-value 0.95))
  (unless (unitlessp y)
    (f-error invalid-unit-conversion-error () "The units ~a and ~a are incompatible under operation Q>=." (str-unit x) (str-unit (unit y))))
  ;; Quantity is unitless
  (if (errorlessp y)
      ;; Quantity has no uncertainty
      (>= x (value y))
      ;; Quantity has uncertainty
      (>= x (+ (value y) (* (aerr y) (confidence-interval p-value :two-sided nil))))))
(defmethod q>= ((x quantity) (y quantity) &optional (p-value 0.95))
  (unless (units-convertible (unit x) (unit y))
    (f-error invalid-unit-conversion-error () "The units ~a and ~a are incompatible under operation Q>=." (str-unit (unit x)) (str-unit (unit y))))
  (let ((delta (q- x y)))
    (>= (value delta) (* (aerr delta) (confidence-interval p-value :two-sided nil)))))
(export 'q>=)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

pq:convert-unit
