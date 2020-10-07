;;;; si-units-derived.lisp

(in-package #:mnas-dim-value)

;;@intern
(defparameter *si-derived-units-tbl-02*
  (list
   (list "area"                      "площадь"     nil
	 ""                          "квадратный метр"
	 "m^2"                       "м^2"
	 (vd 1 :m 2))
   (list "volume"                    "объём"       nil
	 ""                          "кубический метр"
	 "m^3"                       "м^3"
	 (vd 1 :m 3))
   (list "velocity"                  "скорость"    nil
	 ""                          "метр в секунду"
	 "m/s"                       "м/с"
	 (vd/ |m| |s|))
   (list "acceleration"              "ускорение"   nil
	 ""                          "метр на секунду в квадрате"
	 "m/s^2"                     "м/с^2"
	 (vd/ |m| |s| |s|))
   (list "wave number"               "волновое число" nil
	 ""                          "метр в минус первой степени"
	 "1/m"                       "1/м"
	 (vd 1 :m -1))
   (list '("density" "mass density") "плотность"   nil
	 ""                          "килограмм на кубический метр"
	 "kg/m^3"                    "кг/м^3"
	 (vd 1 :kg 1 :m -3))
   (list "specific volume"           "удельный объём" nil
	 ""                          "кубический метр на килограмм"
	 "m^3/kg"                    "м^3/кг"
	 (vd 1 :kg -1 :m 3))
   (list "current density"           "плотность электрического тока" nil
	 ""                          "ампер на квадратный метр"
	 "A/m^2"                     "А/м^2"
	 (vd 1 :A 1 :m -2))
   (list "magnetic field strength"   "напряжённость магнитного поля" nil
	 ""                          "ампер на метр"
	 "A/m"                       "А/м"
	 (vd 1 :A 1 :m -1))
   (list "molar concentration"       "молярная концентрация компонента" nil
	 ""                          "моль на кубический метр"
	 "mol/m^3"                   "моль/м^3"
	 (vd 1 :mol 1 :m -3)   )
   (list "luminance"                 "яркость" nil
	 ""                          "кандела на квадратный метр"
	 "cd/m^2"                    "кд/м^2"
	 (vd 1 :cd 1 :m -2)))
  "Производные единицы СИ, наименования и обозначения которых образованы с использованием
 наименований и обозначений основных единиц СИ.
Каждый подсписок состоит из следующих элементов:
1 - английское наименование величины
2 - русское наименование величины
3 - размерность
4 - международное обозначение единицы
см. ГОСТ 8.417-2002 таблица 2")

;;@intern
(defparameter *si-derived-units-tbl-03*
  (list
   (list "plane angle"                   "плоский угол" "L/L"
	 "radian"                        "радиан"
	 "rad"                           "рад"             "m^1*m^-1"            |rad|)
   (list "solid angle"                   "телесный угол" "L^2/L^2"
	 "steradian"                     "стерадиан"
	 "sr"                            "ср"              "m^2*m^-2=1"          |sr|)
   (list "frequency"                     "частота" nil
	 "hertz"                         "герц"
	 "Hz"                            "Гц"              "s^-1"                |Hz|)
   (list "force"                         "сила" nil
	 "newton"                        "ньютон"
	 "N"                             "Н"               "m*kg*s^-2"           |N|)
   (list "pressure"                      "давление" nil
	 "pascal"                        "паскаль"
	 "Pa"                            "Па"              "m^-1*kg*s^-2"        |Pa|)
   (list '("energy" "work" "quantity of heat") '("энергия" "работа" "количество теплоты") nil
	 "joule"                         "джоуль"
	 "J"                             "Дж"              "kg*m^2/s^2"          |J|)
   (list '("power" "radiant flux")       '("мощность" "поток излучения") nil
	 "watt"                          "ватт"
	 "W"                             "Вт"               "kg*m^2/s^3"         |W|)
   (list '("electric charge"
	   "quantity of electricity")  '("электрический заряд" "количество электричества") nil
	   "coulomb"                     "кулон"
	   "C"                           "Кл"               "s*A"                |C|)
   (list '("electric potential difference" "electromotive force")
	 '("электрическое напряжение" "электродвижущая сила") nil
	    "volt"                       "вольт"
	    "V"                          "В"                "m^2*kg*s^-3*A^-1"   |V|)
    (list "capacitance"                  "электрическая ёмкость" nil
	  "farad"                        "фарад"
	  "F"                            "Ф"                "m^-2*kg^-1*s^4*A^2" |F|)
    (list "electric resistance"          "электрическое сопротивление" nil
	  "ohm"                          "ом"
	  "Ω"                            "Ом"      "m^2*kg*s^-3*A^-2"            |Ω|)
    (list "electric conductance"         "электрическая проводимость" nil
	  "siemens"                      "сименс"
	  "S"                            "См"      "m^-2*kg^-1*s^3*A^2"          |S|)
    (list "magnetic flux"                "магнитный поток" nil
	  "weber"                        "вебер"
	  "Wb"                           "Вб"      "m^2*kg*s^-2*A^-1"            |Wb|)
    (list "magnetic flux density"        "магнитная индукция" nil
	  "tesla"                        "тесла"
	  "T"                            "Тл"      "kg*s^-2*A^-1"                |Τ|)
    (list "inductance"                   "индуктивность" nil
	  "henry"                        "генри"
	  "H"                            "Гн"      "m^2*kg*s^-2*A^-2"            |H|)
    (list "Celsius temperature"          "температура по Цельсию" nil
	  "degree Celsius"               "градус Цельсия"
	  "°C"                           "°С"      "K"                           |K|)
    (list "luminous flux"                "световой поток" nil
	  "lumen"                        "люмен"
	  "lm"                           "лм"      "m^2*m^-2*cd=cd"              |lm|)
    (list "illuminance"                  "освещенность" nil
	  "lux"                          "люкс"
	  "lx"                           "лк"      "m^2*m^-4*cd=m^-2*cd"         |lx|)
    (list "activity (referred to a radionuclide)" "активность (радионуклида)" nil
	  "becquerel"                    "беккерель"
	  "Bq"                           "Бк"      "s^-1"                        |Bq|)
    (list '("absorbed dose" "specific energy (imparted)" "kerma") '("поглощенная доза излучения" "показатель поглощенной дозы" "керма") nil
	  "gray"                         "грей"
	  "Gy"                           "Гр"      "m^2*s^-2"                    |Gy|)
    (list '("dose equivalent" "ambient dose equivalent" "directional dose equivalent" "personal dose equivalent" "organ equivalent dose")
	  '("эквивалентная доза ионизирующего излучения" "эффективная доза ионизирующего излучения") nil
	  "sievert"                      "зиверт"
	  "Sv"                           "Зв"      "m^2*s^-2"                    |Sv|)
    (list "catalytic activity"           "активность катализатора" nil
	  "katal"                        "катал"
	  "kat"                          "кат"     "s^-1*mol"                    |kat|))
  
  "Задает производные единицы СИ, имефщие специальные наименование и обозначения
Каждый подсписок состоит из следующих элементов:
1 - международное наименование величины
2 - русское наименование величины
3 - размерность
4 - международное наименование единицы
5 - русское наименование единицы
6 - международное обозначение единицы
7 - русское обозначение единицы
8 - размерность, выраженная в других единицах системы Си
9 - размерность, выраженная в основны единицах системы Си
см. ГОСТ 8.417-2002 таблица 3")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;@intern
(defparameter *si-derived-units-tbl-04*
  (list
   (list "moment of force"               "момент силы" nil
	 ""                              "ньютон-метр"
	 "N*m"                           "Н*м"               "m^2*kg*s^-2"       (vd* |N|  |m|))
   (list "surface tension"               "поверхностное натяжение" nil
	 ""                              "ньютон-метр"
	 "N/m"                           "Н*м"               "kg*s^-2"           (vd/ |N|  |m|))
   (list "dynamic viscosity"             "динамическая вязкость" nil
	 ""                              "паскаль-секунда"
	 "Pa*s"                          "Па*с"              "m^-1*kg*s^-1"      (vd* |Pa| |s|))
   (list "electric charge density" "пространственная плотность электрического заряда" nil
	 ""                              "кулон на кубический метр"
	 "C/m^3"                         "Кл/м^3"            "m^-3*s*A"          (vd/ |C|  |m| |m| |m|)) 
   (list "electric flux density"         "электрическое смещение" nil
	 ""                              "кулон на квадратный метр" 
	 "C/m^2"                         "Кл/м^2"            "m^-2*s*A"          (vd/ |C|  |m| |m|))
   (list "electric field strength"       "напряженность электрического поля" nil
	 ""                              "воль на метр"
	 "V/m"                           "В/м"               "m*kg*s^-3*A^-1"    (vd/ |V|  |m|))
   (list "permittivity"                  "диэлектрическая проницаемость" nil
	 ""                              "фарад на метр"
	 "F/m"                           "Ф/м"               "m^-3*kg-1*s^4*A^2" (vd/ |F|  |m|))
   (list "permeability"                  "магнитная проницаемость" nil
	 ""                              "генри на метр"
	 "H/m"                           "Гн/м"              "m*kg*s^-2*A^-2"    (vd/ |H|  |m|))
   (list "specific energy"               "удельная энергия" nil
	 ""                              "джоуль на килограмм"
	 "J/kg"                          "Дж/кг"             "m^2*s^-2"          (vd/ |J|  |kg|))
   (list '("heat capacity" "entropy")    '("теплоемкость системы" "энтропия системы") nil
	 ""                              "джоуль на кельвин"
	 "J/K"                           "Дж/К"              "kg*m^2/(s^2*K)"    (vd/ |J|  |K|))
   (list '("specific heat capacity" "specific entropy") '("удельная теплоёмкость" "удельная энтропия") nil
	 ""                              "джоуль на килограмм-кельвин"
	 "J/(kg*K)"                      "Дж/(кг*К)"         "m^2/(s^2*K)"       (vd/ |J| |kg| |K|))
   (list '("heat flux density" "irradiance")  "поверхностная плотность потока энергии"  nil
	 ""                              "ватт на квадратный метр"
	 "W/m^2"                         "Вт/м^2"            "kg*s^-3"           (vd/ |W| |m| |m|))
   (list "thermal conductivity"          "теплопроводность" nil
	 ""                              "ватт на метр-кельвин"
	 "W/(m*K)"                       "Вт/(м*К)"          "m*kg*s^-3*K^-1"    (vd/ |W| |m| |K|))
   (list "molar energy"                  "молярная внутренняя энергия" nil
	 ""                              "джоуль на моль"
	 "J/mol"                         "Дж/моль"      "m^2*kg*s^-2*mol^-1"     (vd/ |J| |mol|))
   (list '("molar entropy" "molar heat capacity") '("молярная энтропия" "молярная теплоёмкость") nil
	 ""                              "джоуль на моль-кельвин"
	 "J/(mol*K)"                     "Дж/(моль*К)" "m^2*kg*s^-2*K^-1*mol^-1" (vd/ |J| |mol| |K|))
   (list "exposure (x and γ rays)"       "экспозиционная доза фотонного излучения" nil
	 ""                              "кулон на килограмм"
	 "C/kg"                          "Кл/кг"       "kg^-1*s*A"               (vd/ |C| |kg|))
   (list "absorbed dose rate"            "мощность поглощённой дозы" nil
	 ""                              "грей в секунду"
	 "Gy/s"                          "Гр/с"        "m^2*s^-3"                (vd/ |Gy| |s|))
   (list "angular velocity"              "угловая скорость" nil
	 ""                              "радиан в секунду"
	 "rad/s"                         "рад/с"       "s^-1"                    (vd/ |rad| |s|))
   (list "angular acceleration"          "угловое ускорение" nil
	 ""                              "радиан на секунду в квадрате"
	 "rad/s^2"                       "рад/с^2"     "s^-2"                    (vd/ |rad| |s| |s|))
   (list "radiant intensity"             "сила излучения" nil
	 ""                              "ватт на стерадиан"
	 "W/sr"                          "Вт/ср"       "m^4*m^-2*kg*s^-3"        (vd/ |W| |sr|))
   (list "radiance"                      "энергетическая яркость" nil
	 ""                              "ватт на стерадан-кадратный метр"
	 "W/(sr*m^2)"                    "Вт/(ср*м^2)" "m^2*m^-2*kg*s^-3"        (vd/ |W| |sr| |m| |m|))
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
)

  "Задает производные единицы измерения системы SI
Каждый подсписок состоит из следующих элементов:
1 - английское наименование величины
2 - русское наименование величины
3 - размерность величины
4 - международное обозначене единицы
5 - единицы, вараженная через основные и производные единицы СИ
см. ГОСТ 8.417-2002, таблица 4")

(defparameter *nd-si-derived-units-tbl-02* (make-nd-items *si-derived-units-tbl-02*)  "Задает производные единицы измерения системы SI.")
(setf (documentation  '*nd-si-derived-units-tbl-02* 'variable) (documentation  '*si-derived-units-tbl-02* 'variable))

(defparameter *nd-si-derived-units-tbl-03* (make-nd-items *si-derived-units-tbl-03*) "Задает производные единицы СИ, имеющие специальные наименование и обозначения.")
(setf (documentation  '*nd-si-derived-units-tbl-03* 'variable) (documentation  '*si-derived-units-tbl-03* 'variable))

(defparameter *nd-si-derived-units-tbl-04* (make-nd-items    *si-derived-units-tbl-04*) "Задает производные единицы измерения системы SI")

(setf (documentation  '*nd-si-derived-units-tbl-04* 'variable) (documentation  '*si-derived-units-tbl-04* 'variable))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

   (list "momentum"                 '("импульс"
				"количество движения")                   nil             "kg*m/s"  (vd 1 :kg 1 :m 1 :s -1))
   (list "kinematic viscosity"      "кинематическая вязкость"            nil             "m^2/s"   (vd 1 :m 2 :s -1)      )
   (list "moment of inertia"        "момент инерции"                     nil             "kg*m^2"  (vd 1 :kg 1 :m 2)      )
   (list "mass flow rate"           "массовый расход"                    nil             "kg/s"    (vd 1 :kg 1 :s -1)     )
   (list "volumetric flow"          "объёмный расход"                    nil             "m^3/s"   (vd 1 :m 3 :s -1)      )
   (list "flow rate"                "молекулярный расход"                nil             "mol/s"   (vd 1 :mol 1 :s -1)    )
   (list "molar weight"             "молекулярный вес"                   nil             "kg/mol"  (vd 1 :kg 1 :mol -1)   )
   (list "energy density"           "плотноть энергии"                   nil             "J/m^3"   (vd/ |J| |m| |m| |m|)  )
   (list "force impulse"            "импульс силы"                       nil             "N*s"     (vd* |N| |s| )         )
