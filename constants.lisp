;;;; constants.lisp

(in-package #:mnas-dim-value)

(export '|*g*|    )
(defparameter |*g*|    (vd 9.8065d0 :m 1 :s -2)                "Ускорение свободного падения")

(export '|*Gn*|   )
(defparameter |*Gn*|   (vd 6.6740831d-11 :m 3 :kg -1 :s -2)    "Гравитационная постоянная")

(export '|*C-0*|  )
(defparameter |*C-0*|  (vd* 273.15d0 |K|)                      "Ноль шкалы температур Цельсия")

(export '|*V-0*|  )
(defparameter |*V-0*|  (vd 22.41396213d-3 :m 3 :mol -1)        "Молекулярный объем идеального газа при нормальных условиях")

(export '|*R-0*|  )
(defparameter |*R-0*|  (vd* 8.314459848d0 (vd/ |J| |mol| |K|)) "Универсальная газовая постоянная, Дж/моль*К")

(export '|*Na*|   )
(defparameter |*Na*|   (vd 6.0221408211d23)                    "Число Авогадро, 1")

(export '|*No*|   )
(defparameter |*No*|   (vd/ 2.686781115d25 |m| |m| |m|)        "Число Лошмидта, 1/м3")

(export '|*k*|    )
(defparameter |*k*|    (vd* 1.3806485279d-23 (vd/ |J| |K|))    "Постоянная Больцмана, Дж/К")

(export '|*a-e-m*|)
(defparameter |*a-e-m*|(vd 1.660540210d-27 :kg 1)              "Атомная единица массы, кг")

(export '|*m-e*|  )
(defparameter |*m-e*|  (vd 9.109382616d-31 :kg 1)              "Масса покоя электрона, кг")

(export '|*e*|    )
(defparameter |*e*|    (vd* -1.602176620898d-19 |C|)           "Заряд электрона, Кл")

(export '|*F*|    )
(defparameter |*F*|    (vd* 96485.3328959d4  (vd/ |C| |mol|))  "Число Фарадея, Кл/моль")

(export '|*h*|    )
(defparameter |*h*|    (vd* 6.62607004081d-34 |J| |s|)         "Постоянная Планка, Дж*с")

(export '|*c*|    )
(defparameter |*c*|    (vd 299792458 :m 1 :s -1)               "Скорость света в вакууме, м/с")

(export '|*μ-0*|  )
(defparameter |*μ-0*|  (vd/ (vd* 4 pi 1d-7 |H|) |m|)           "Магнитная постоянная, Гн/м")

(export '|*ε-0*|  )
(defparameter |*ε-0*|  (vd/ 1 |*μ-0*| |*c*| |*c*|)             "Электрическая постоянная, Ф/м")

;;;; acceleration of gravity
