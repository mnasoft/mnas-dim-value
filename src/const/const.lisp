;;;; constants.lisp

(defpackage :mnas-dim-value/const
  (:nicknames "MDV/CONST")
  (:use #:cl
        #:mnas-dim-value/func
        #:mnas-dim-value/class
        #:mnas-dim-value/mk-class
        #:mnas-dim-value/tbl
        #:mnas-hash-table
        #:mnas-dim-value/ht/core
        #:mnas-dim-value/generic
        #:mnas-dim-value/method
        )
  (:export |*g*|
           |*Gn*|
           |*C-0*| 
           |*V-0*| 
           |*R-0*| 
           |*Na*|  
           |*No*|  
           |*k*|   
           |*a-e-m*|
           |*m-e*|
           |*e*|  
           |*F*|  
           |*h*|  
           |*c*|  
           |*μ-0*|
           |*ε-0*|
           ))

(in-package :mnas-dim-value/const)

(progn 
  (defparameter |*g*|    (vd 9.80665d0 :m 1 :s -2)                 "Ускорение свободного падения")
  (defparameter |*Gn*|   (vd 6.6740831d-11 :m 3 :kg -1 :s -2)      "Гравитационная постоянная")
  (defparameter |*C-0*|  (vd~* 273.15d0 "K")                       "Ноль шкалы температур Цельсия")
  (defparameter |*V-0*|  (vd 22.41396213d-3 :m 3 :mol -1)          "Молекулярный объем идеального газа при нормальных условиях")
  (defparameter |*R-0*|  (vd~* 8.314459848d0 (vd~/ "J" "mol" "K")) "Универсальная газовая постоянная, Дж/моль*К")
  (defparameter |*Na*|   (vd 6.0221408211d23)                      "Число Авогадро, 1")
  (defparameter |*No*|   (vd~/ 2.686781115d25 "m" "m" "m")         "Число Лошмидта, 1/м3")
  (defparameter |*k*|    (vd~* 1.3806485279d-23 (vd~/ "J" "K"))    "Постоянная Больцмана, Дж/К")
  (defparameter |*a-e-m*|(vd 1.660540210d-27 :kg 1)                "Атомная единица массы, кг")
  (defparameter |*m-e*|  (vd 9.109382616d-31 :kg 1)                "Масса покоя электрона, кг")
  (defparameter |*e*|    (vd~* -1.602176620898d-19 "C")            "Заряд электрона, Кл")
  (defparameter |*F*|    (vd~* 96485.3328959d4  (vd~/ "C" "mol"))  "Число Фарадея, Кл/моль")
  (defparameter |*h*|    (vd~* 6.62607004081d-34 "J" "s")          "Постоянная Планка, Дж*с")
  (defparameter |*c*|    (vd 299792458 :m 1 :s -1)                 "Скорость света в вакууме, м/с")
  (defparameter |*μ-0*|  (vd~/ (vd~* 4 pi 1d-7 "H") "m")           "Магнитная постоянная, Гн/м")
  (defparameter |*ε-0*|  (vd~/ 1 |*μ-0*| |*c*| |*c*|)              "Электрическая постоянная, Ф/м"))

