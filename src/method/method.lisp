;;;; ./src/method/method.lisp

(mnas-macro:mnas-defpackage
 :mnas-dim-value/method
 (:use #:cl
       #:mnas-dim-value/condition
       #:mnas-dim-value/vars
       #:mnas-dim-value/func
       #:mnas-dim-value/class
       #:mnas-dim-value/mk-class
       #:mnas-dim-value/tbl
       #:mnas-hash-table
       #:mnas-dim-value/ht
       )
 (:use-and-export #:mnas-dim-value/generic)
 (:export dim->unit-symbol
          dimensionp
          )
 (:export vd~simplify                ; Удаление радианов и стерадианов
          )
 (:export vd~+                    ; Сложение
          vd~-                    ; Вычитание
          vd~*                    ; Умножение
          vd~/                    ; Деление
          vd~pow                  ; Возведение в целочисленную степень
          vd~root                 ; Извлечение корня степени
          vd~sqrt                 ; Извлечение квадратного
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
 (:export vd~=                        
          vd~/=
          vd~<
          vd~<=
          vd~>
          vd~>=)
 (:export print-object
          )
 (:intern angle-string
          solid-angle-string
          time-string
          )
 )

(in-package :mnas-dim-value/method)

(defmethod vd~simplify ((vd <vd>))
  (setf (nth 7 (<vd>-dims vd)) 0)
  (setf (nth 8 (<vd>-dims vd)) 0)
  vd)

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

(defun vd~= (x y)
  "Операция проерки на равенство для физических величин."
  (let ((xx (vd-convert x))
        (yy (vd-convert y)))
  (unless (same-dimension xx yy)
    (f-error invalid-unit-conversion-error ()
             "The units ~s and ~s are incompatible under operation vd~~=."
             xx yy))
    (= (<vd>-val xx) (<vd>-val yy))))

(defun vd~/= (x y)
  "Операция проерки на равенство для физических величин."
  (let ((xx (vd-convert x))
        (yy (vd-convert y)))
  (unless (same-dimension xx yy)
    (f-error invalid-unit-conversion-error ()
             "The units ~s and ~s are incompatible under operation vd~~/=."
             xx yy))
    (/= (<vd>-val xx) (<vd>-val yy))))

(defun vd~< (x y)
  "Операция проерки на равенство для физических величин."
  (let ((xx (vd-convert x))
        (yy (vd-convert y)))
  (unless (same-dimension xx yy)
    (f-error invalid-unit-conversion-error ()
             "The units ~s and ~s are incompatible under operation vd~~<."
             xx yy))
    (< (<vd>-val xx) (<vd>-val yy))))

(defun vd~<= (x y)
  "Операция проерки на равенство для физических величин."
  (let ((xx (vd-convert x))
        (yy (vd-convert y)))
  (unless (same-dimension xx yy)
    (f-error invalid-unit-conversion-error ()
             "The units ~s and ~s are incompatible under operation vd~~<=."
             xx yy))
    (<= (<vd>-val xx) (<vd>-val yy))))

(defun vd~> (x y)
  "Операция проерки на равенство для физических величин."
  (let ((xx (vd-convert x))
        (yy (vd-convert y)))
  (unless (same-dimension xx yy)
    (f-error invalid-unit-conversion-error ()
             "The units ~s and ~s are incompatible under operation vd~>."
             xx yy))
    (> (<vd>-val xx) (<vd>-val yy))))

(defun vd~>= (x y)
  "Операция проерки на равенство для физических величин."
  (let ((xx (vd-convert x))
        (yy (vd-convert y)))
  (unless (same-dimension xx yy)
    (f-error invalid-unit-conversion-error ()
             "The units ~s and ~s are incompatible under operation vd~~>=."
             xx yy))
    (>= (<vd>-val xx) (<vd>-val yy))))
