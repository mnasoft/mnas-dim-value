;;;; ./src/method/method.lisp

(defpackage :mnas-dim-value/method
  (:use #:cl
        #:mnas-dim-value/vars
        #:mnas-dim-value/func
        #:mnas-dim-value/class
        #:mnas-dim-value/mk-class
        #:mnas-dim-value/tbl
        #:mnas-hash-table
        #:mnas-dim-value/ht
        #:mnas-dim-value/generic
        )
  (:export dim->unit-symbol
           dimensionp
           same-dimension
           vd-convert
           unit-name
           quantity-name
           )
  (:export vd~simplify               ; Удаление радианов и стерадианов
           )
  (:export vd~+                   ; Сложение
           vd~-                   ; Вычирание
           vd~*                   ; Умножение
           vd~/                   ; Деление
           vd~pow                 ; Возведение в целочисленную степень
           vd~root                ; Извлечение корня степени
           vd~sqrt                ; Извлечение квадратного
           vd~exp
           vd~expt
           vd~ln
           vd~log
           vd~sin
           vd~cos
           vd~tan
           vd~asin
           vd~acos
           vd~atan
           vd~sinh
           vd~cosh
           vd~tanh
           vd~asinh
           vd~acosh
           vd~atanh
           vd~abs
           vd~equal
           vd~equalp
           )
  (:export vd~=                         ; Группа пока не определена
           vd~/=
           vd~<
           vd~<=
           vd~>
           vd~>=)
  (:export print-object
           vd-print
           )
  (:intern angle-string
           solid-angle-string)
  )

(in-package :mnas-dim-value/method)

(defun dim->unit-symbol ()
  (cond ((eq (get-env "LANGUAGE" *variable-set*) :ru) mnas-dim-value/ht-ru:*dim->unit-symbol*)
	(t mnas-dim-value/ht-en:*dim->unit-symbol*)))

;;(alexandria:hash-table-values mnas-dim-value/ht-en:*dim->unit-symbol*)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(let ((+format+ :f-01)
      (+unit-symbol+ '("m" "kg" "s" "A" "K" "cd" "mol" "rad" "sr"))
      )
  (defun vd-print-format (fmt)
    (setf +format+ fmt))

  (defmethod vd-print ((x <vd>) &optional (o-stream t) )
  (cond
    ((eq +format+ :f-01)
     (format o-stream "(vd ~S " (<vd>-val x))
     (loop :for u-s :in +unit-symbol+
           :for u-v :in (<vd>-dims x)
           :when (/= u-v 0)
             :do (format o-stream ":~A ~A " u-s u-v))
     (format o-stream ")")
     )
    
    (t
     (format o-stream "~S ~S" (<vd>-val x) (<vd>-dims x)))))
  )

;;;(vd-print-format :f-02)
;;;(vd-print (vd~* 101.325 1000.0 "Pa"))
;;;"m" "kg" "s" "A" "K" "cd" "mol" "rad" "sr"

(defmethod vd-print ((s string) &optional (o-stream t) &aux (x (vd-convert s)))
  (vd-print x o-stream))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun vd~+ (&rest args)
  "Операция сложения для физических величин."
  (let* ((lst (mapcar #'vd-convert args))
         (val (apply #'+ (mapcar #'<vd>-val lst))))
    (make-instance '<vd>
                   :dims (copy-list (<vd>-dims (first lst)))
                   :val val)))

(defun vd~- (&rest args)
  "Возвращает список наименований величин, которым соответствует
размерность числа x (\"acceleration\")."
  (let* ((lst (mapcar #'vd-convert args))
         (val (apply #'- (mapcar #'<vd>-val lst))))
    (make-instance '<vd>
                   :dims (copy-list (<vd>-dims (first lst)))
                   :val val)))

(defun vd~* (&rest args)
"Перемножение чисел с размерностью."
  (let* ((lst (mapcar #'vd-convert args))
         (val (apply #'* (mapcar #'<vd>-val lst)))
         (dim (mapcar #'<vd>-dims lst)))
    (make-instance '<vd>
                   :dims (apply #'mapcar #'+ dim)
                   :val val)))

(defun vd~/ (&rest args)
"Перемножение чисел с размерностью."
  (let* ((lst (mapcar #'vd-convert args))
         (val (apply #'/ (mapcar #'<vd>-val lst)))
         (dim (mapcar #'<vd>-dims lst)))
    (make-instance '<vd>
                   :dims (apply #'mapcar #'- dim)
                   :val val)))

(defun vd~pow (base power)
  "Возведение в целочисленную степень."
  (let* ((vd (vd-convert base))
         (val (<vd>-val  vd))
         (dim (<vd>-dims vd)))
    (make-instance '<vd>
                   :dims (mapcar
                          #'(lambda (el)
                              (if (/= power 0) (* el power) 0))
                          dim)
                   :val  (expt val power))))

(defun vd~root (radicand degree)
  "Извлечение корня."
  (let* ((vd (vd-convert radicand))
         (val (<vd>-val  vd))
         (dim (<vd>-dims vd)))
    (make-instance '<vd>
                   :dims (mapcar
                          #'(lambda (el)
                              (if (/= degree 0)
                                  (/ el degree)
                                  0))
                          dim)
                   :val  (expt val (/ degree)))))

(defun vd~sqrt (x)
  (vd~root x 2))

(defun vd~exp (power)
  "Экспонента."
  (let* ((vd (vd-convert power))
         (val (<vd>-val  vd))
         (dim (<vd>-dims vd)))
    (make-instance '<vd>
                   :dims (copy-list dim)
                   :val  (exp val))))

(defun vd~expt (base power)
  "Возведение в произвольную степень."
  (let* ((vd (vd-convert base))
         (val (<vd>-val  vd))
         (dim (<vd>-dims vd)))
    (make-instance '<vd>
                   :dims (copy-list dim)
                   :val  (expt val power))))

(defun vd~ln (x)
  "Натуратьный логарифм."
  (let* ((vd (vd-convert x))
         (val (<vd>-val  vd))
         (dim (<vd>-dims vd)))
    (make-instance '<vd>
                   :dims (copy-list dim)
                   :val  (log val))))

(defun vd~log (x base)
  "Логарифм по основанию."
  (let* ((vd  (vd-convert x))
         (val (<vd>-val  vd))
         (dim (<vd>-dims vd)))
    (make-instance '<vd>
                   :dims (copy-list dim)
                   :val  (log val base))))

(defun vd~sin (x)
  "Синус."
  (let* ((vd  (vd~simplify (vd-convert x)))
         (val (<vd>-val  vd))
         (dim (<vd>-dims vd)))
    (make-instance '<vd>
                   :dims (copy-list dim)
                   :val  (sin val))))
(defun vd~cos (x)
  "Косинус."
  (let* ((vd (vd~simplify (vd-convert x)))
         (val (<vd>-val  vd))
         (dim (<vd>-dims vd)))
    (make-instance '<vd>
                   :dims (copy-list dim)
                   :val  (cos val))))

(defun vd~tan (x)
  "Тангенс."
  (let* ((vd (vd~simplify (vd-convert x)))
         (val (<vd>-val  vd))
         (dim (<vd>-dims vd)))
    (make-instance '<vd>
                   :dims (copy-list dim)
                   :val  (tan val))))

(defun vd~asin (x)
  "Арксинус."
  (let* ((vd (vd~simplify (vd-convert x)))
         (val (<vd>-val  vd))
         (dim (<vd>-dims vd)))
    (vd~* :rad
          (make-instance '<vd>
                         :dims (copy-list dim)
                         :val  (asin val)))))

(defun vd~acos (x)
  "Арккосинус."
  (let* ((vd (vd~simplify (vd-convert x)))
         (val (<vd>-val  vd))
         (dim (<vd>-dims vd)))
    (vd~* :rad
          (make-instance '<vd>
                         :dims (copy-list dim)
                         :val  (acos val)))))

(defun vd~atan (y &optional (x (vd 1)))
  "Арктангенс."
  (let* ((vd  (vd~simplify (vd~/ y x)))
         (val (<vd>-val  vd))
         (dim (<vd>-dims vd)))
    (vd~* :rad
          (make-instance '<vd>
                         :dims (copy-list dim)
                         :val  (atan val)))))

(defun vd~sinh (x)
  "Синус гиперболический."
  (let* ((vd (vd-convert x))
         (val (<vd>-val  vd))
         (dim (<vd>-dims vd)))
    (make-instance '<vd>
                   :dims (copy-list dim)
                   :val  (sinh val))))
(defun vd~cosh (x)
  "Косинус гиперболический."
  (let* ((vd (vd-convert x))
         (val (<vd>-val  vd))
         (dim (<vd>-dims vd)))
    (make-instance '<vd>
                   :dims (copy-list dim)
                   :val  (cosh val))))

(defun vd~tanh (x)
  "Тангенс гиперболический."
  (let* ((vd (vd-convert x))
         (val (<vd>-val  vd))
         (dim (<vd>-dims vd)))
    (make-instance '<vd>
                   :dims (copy-list dim)
                   :val  (tanh val))))

(defun vd~asinh (x)
  "Арксинус гиперболический."
  (let* ((vd (vd-convert x))
         (val (<vd>-val  vd))
         (dim (<vd>-dims vd)))
    (make-instance '<vd>
                   :dims (copy-list dim)
                   :val  (asinh val))))

(defun vd~acosh (x)
    "Арккосинус гиперболический."
  (let* ((vd (vd-convert x))
         (val (<vd>-val  vd))
         (dim (<vd>-dims vd)))
    (make-instance '<vd>
                   :dims (copy-list dim)
                   :val  (acosh val))))

(defun vd~atanh (x)
    "Арктангенс гиперболический."
  (let* ((vd (vd-convert x))
         (val (<vd>-val  vd))
         (dim (<vd>-dims vd)))
    (make-instance '<vd>
                   :dims (copy-list dim)
                   :val  (atanh val))))

(defun vd~abs (x)
    "Абсолютное значение."
  (let* ((vd (vd-convert x))
         (val (<vd>-val  vd))
         (dim (<vd>-dims vd)))
    (make-instance '<vd>
                   :dims (copy-list dim)
                   :val  (abs val))))

(defun vd~equal (x y)
    "Абсолютное значение."
  (let ((x-vd (vd-convert x))
        (y-vd (vd-convert y)))
    (and (equal (<vd>-val  x-vd) (<vd>-val  y-vd))
         (equal (<vd>-dims  x-vd) (<vd>-dims  y-vd)))))

(defun vd~equalp (x y)
    "Абсолютное значение."
  (let ((x-vd (vd-convert x))
        (y-vd (vd-convert y)))
    (and (equalp (<vd>-val  x-vd) (<vd>-val  y-vd))
         (equalp (<vd>-dims  x-vd) (<vd>-dims  y-vd)))))

