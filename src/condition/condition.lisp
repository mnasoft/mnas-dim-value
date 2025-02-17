(defpackage :mnas-dim-value/condition
  (:use #:cl
        )
  (:export unit-definition-error
           unit-definition-syntax-error
           unit-definition-semantic-error
           unit-definition-conflict-error
           )
  (:export operation-undefined-error
           )
  (:export invalid-unit-error
           invalid-unit-operation-error
           invalid-unit-conversion-error
           invalid-unit-reference-error
           )
  (:export error-propagation-error
           )
  (:export unit-syntax-error)
  (:export f-error)
  )

(in-package :mnas-dim-value/condition)

(define-condition physical-quantities-error (simple-error) ()
  (:documentation "Generic error for the physical quantities library."))

(define-condition unit-definition-error (physical-quantities-error) ()
  (:documentation "Generic error for unit/prefix definitions."))
(define-condition unit-definition-syntax-error (unit-definition-error) ()
  (:documentation "Syntax error in the definition of a unit/prefix"))
(define-condition unit-definition-semantic-error (unit-definition-error) ()
  (:documentation "Semantic error in the definition of a unit/prefix"))
(define-condition unit-definition-conflict-error (unit-definition-error) ()
  (:documentation "Name conflict for the definition of a unit/prefix"))

(define-condition operation-undefined-error (physical-quantities-error) ()
  (:documentation "Error for situations in which an operation is undefined"))

(define-condition invalid-unit-error (physical-quantities-error) ()
  (:documentation "Generic unit error"))
(define-condition invalid-unit-operation-error (invalid-unit-error) ()
  (:documentation "Error for situations in which the unit is invalid in a given operation"))
(define-condition invalid-unit-conversion-error (invalid-unit-error) ()
  (:documentation "Error when converting between incompatible units"))
(define-condition invalid-unit-reference-error (invalid-unit-error) ()
  (:documentation "Unit lookup error"))

(define-condition error-propagation-error (physical-quantities-error) ()
  (:documentation "Error for situations in which the propagation of uncertainty is undefined"))


(define-condition quantity-definition-error (physical-quantities-error) ()
  (:documentation "Generic error for quantity definitions."))
(define-condition quantity-definition-syntax-error (quantity-definition-error) ()
  (:documentation "Syntax error in the definition of a quantity/unit"))
(define-condition quantity-definition-semantic-error (quantity-definition-error) ()
  (:documentation "Semantic error in the definition of a quantity/unit"))

(define-condition unit-definition-error (physical-quantities-error) ()
  (:documentation "Generic error for unit/prefix definitions."))

(defmacro f-error (type (&rest initargs) control &rest args)
  "Like (error ...), but allows the condition type to be specified (which is required to inherit from simple-condition)."
  `(error ',type ,@initargs :format-control ,control :format-arguments (list ,@args)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+nil
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

#+nil
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
