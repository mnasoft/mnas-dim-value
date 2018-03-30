;;;; si-units.lisp

(in-package #:mnas-dim-value)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *si-main-units*
  (list
   (list "length"              "длина"	                       "L" "meter"    "метр"      "m"   "м"    |m|   )
   (list "mass"                "масса"                         "M" "kilogram" "килограмм" "kg"  "кг"   |kg|  )
   (list "time"                "время"                         "T" "second"   "секунда"   "s"   "с"    |s|   )
   (list "electric current"    "сила тока электрического"      "I" "ampere"   "ампер"     "A"   "А"    |A|   )
   (list "temperature"         "температура термодинамическая" "Θ" "kelvin"   "кельвин"   "K"   "К"    |K|   )
   (list "amount of substance" "количество вещества"           "N" "mole"     "моль"      "mol" "моль" |mol| )
   (list "luminous intensity"  "сила света"                    "J" "candela"  "кандела"   "cd"  "кд"   |cd|  ))
  "si-main-units
Задает основные единицы измерения системы SI
Каждый подсписок состоит из следующих элементов:
1 - английксое наименование величины
2 - русское наименование величины
3 - размерность величины
4 - английское наименование единицы
5 - русское наименование единицы
6 - международное обозначение единицы
7 - русское обозначение единицы
см. ГОСТ 8.417-2002, таблица 1")

(defparameter *si-derived-units-tbl-02*
  (list
   (list "area"                      "площадь"                           nil             "m^2"      (vd 1 :m 2)           )
   (list "volume"                    "объём"                             nil             "m^3"      (vd 1 :m 3)           )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   (list "velocity"                  "скорость"                          nil             "m/s"      (vd/ |m| |s|)         )
   (list "acceleration"              "ускорение"                         nil             "m/s^2"    (vd/ |m| |s| |s|)     )

   (list "wave number"               "волновое число"                    nil             "1/m"      (vd 1 :m -1)          )

   (list '("density" "mass density") "плотность"                         nil             "kg/m^3"   (vd 1 :kg 1 :m -3)    )
   (list "specific volume"           "удельный объём"                    nil             "m^3/kg"   (vd 1 :kg -1 :m 3)    )

   (list "current density"           "плотность электрического тока"     nil             "A/m"      (vd 1 :A 1 :m -1)     )
   (list "magnetic field strength"   "напряжённость магнитного поля"     nil             "A/m"      (vd 1 :A 1 :m -1)     )

   (list "molar concentration"       "молярная концентрация компонента"  nil             "mol/m^3"  (vd 1 :mol 1 :m -3)   )
   (list "luminance"                 "яркость"                           nil             "cd/m^2"   (vd 1 :cd 1 :m -2)    )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   (list "momentum"                 '("импульс"
				"количество движения")                   nil             "kg*m/s"  (vd 1 :kg 1 :m 1 :s -1))
   (list "kinematic viscosity"      "кинематическая вязкость"            nil             "m^2/s"   (vd 1 :m 2 :s -1)      )
   (list "moment of inertia"        "момент инерции"                     nil             "kg*m^2"  (vd 1 :kg 1 :m 2)      )
   (list "mass flow rate"           "массовый расход"                    nil             "kg/s"    (vd 1 :kg 1 :s -1)     )
   (list "volumetric flow"          "объёмный расход"                    nil             "m^3/s"   (vd 1 :m 3 :s -1)      )
   (list "flow rate"                "молекулярный расход"                nil             "mol/s"   (vd 1 :mol 1 :s -1)    )
   (list "molar weight"             "молекулярный вес"                   nil             "kg/mol"  (vd 1 :kg 1 :mol -1))  )
  "si-derived-units-expressed-in-terms-of-base-units
Производные единицы СИ, наименования и обозначения которых образованы с использованием
 наименований и обозначений основных единиц СИ.
Каждый подсписок состоит из следующих элементов:
1 - английское наименование величины
2 - русское наименование величины
3 - размерность
4 - международное обозначение единицы
см. ГОСТ 8.417-2002 таблица 2")

(defparameter *si-derived-units-tbl-03*
  (list
    (list "plane angle"                   "плоский угол"                             "L/L"      "radian"       "радиан"     "rad" "рад" nil      "m^1*m^-1=1"          |rad| )
    (list "solid angle"                   "телесный угол"                            "L^2/L^2"  "steradian"    "стерадиан"  "sr"  "ср"  nil      "m^2*m^-2=1"          |sr|  )
    (list "frequency"                     "частота"                                   nil       "hertz"        "герц"       "Hz"  "Гц"  nil      "s^-1"                |Hz|  )
    (list "force"                         "сила"                                      nil       "newton"       "ньютон"     "N"   "Н"   nil      "m*kg*s^-2"           |N|   )
    (list '("pressure" "stress")          '("давление" "напряжение")                  nil       "pascal"       "паскаль"    "Pa"  "Па"  "N/m^2"  "m^-1*kg*s^-2"        |Pa|  )
    (list '("energy" "work" "quantity of heat") '("энергия" "работа" "количество теплоты") nil  "joule"        "джоуль"     "J"   "Дж"  "N*m"    "kg*m^2/s^2"          |J|   )
    (list '("power" "radiant flux")       '("мощность" "поток излучения")             nil       "watt"         "ватт"       "W"   "Вт"  "J/s"    "kg*m^2/s^3"          |W|   )
    (list '("electric charge" "quantity of electricity") '("электрический заряд" "количество электричества") nil "coulomb" "кулон" "C" "Кл" nil  "s*A"                 |C|   )
    (list '("electric potential difference"
      "electromotive force")              '("электрическое напряжение"
				            "электродвижущая сила")                   nil       "volt"         "вольт"      "V"   "В"   "W/A"    "m^2*kg*s^-3*A^-1"    |V| )
    (list "capacitance"                  "электрическая ёмкость"                      nil       "farad"        "фарад"      "F"   "Ф"   "C/V"    "m^-2*kg^-1*s^4*A^2"  |F| )
    (list "electric resistance"          "электрическое сопротивление"                nil       "ohm"          "ом"         "Ω"   "Ом"  "V/A"    "m^2*kg*s^-3*A^-2"    |Ω| )
    (list "electric conductance"         "электрическая проводимость"                 nil       "siemens"      "сименс"     "S"   "См"  "A/V"    "m^-2*kg^-1*s^3*A^2"  |S| )
    (list "magnetic flux"                "магнитный поток"                            nil       "weber"        "вебер"      "Wb"  "Вб"  "V*s"    "m^2*kg*s^-2*A^-1"    |Wb|)
    (list "magnetic flux density"        "магнитная индукция"                         nil       "tesla"        "тесла"      "T"   "Тл"  "Wb/m^2" "kg*s^-2*A^-1"        |Τ| )
    (list "inductance"                   "индуктивность"                              nil       "henry"        "генри"      "H"   "Гн"  "Wb/A"   "m^2*kg*s^-2*A^-2"    |H| )
    (list "Celsius temperature"          "температура по Цельсию"                     nil "degree Celsius" "градус Цельсия" "°C"  "°С"  nil      "K"                   |K| )
    (list "luminous flux"                "световой поток"                             nil       "lumen"        "люмен"      "lm"  "лм"  "cd*sr"  "m^2*m^-2*cd=cd"      |lm|)
    (list "illuminance"                  "освещенность"                               nil       "lux"          "люкс"       "lx"  "лк"  "lm/m^2" "m^2*m^-4*cd=m^-2*cd" |lx|)
    (list "activity (referred to a radionuclide)"
                                    "активность (радионуклида)"                       nil       "becquerel"    "беккерель"  "Bq"  "Бк"  nil      "s^-1"                |Bq|)
    (list '("absorbed dose"
      "specific energy (imparted)"
      "kerma")                      '("поглощенная доза излучения"
				     "показатель поглощенной дозы"
				     "керма")                                         nil       "gray"         "грей"       "Gy"  "Гр"  "J/kg"   "m^2*s^-2"            |Gy|)
    (list '("dose equivalent"
      "ambient dose equivalent"
      "directional dose equivalent"
      "personal dose equivalent"
      "organ equivalent dose")      '("эквивалентная доза ионизирующего излучения"
                                     "эффективная доза ионизирующего излучения")      nil "sievert" "зиверт"  "Sv"  "Зв"  "J/kg"   "m^2*s^-2"                          |Sv|)
    (list "catalytic activity"           "активность катализатора"                    nil       "katal"        "катал"       "kat" "кат" nil      "s^-1*mol"           |kat|))
  
  "si-derived-units-with-special-names-and-symbols
Задает производные единицы СИ, имефщие специальные наименование и обозначения
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

;;;;(mapcar #'(lambda(el) (string= (fourth el)(sixth el))) *si-derive*)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *si-derived-units-tbl-04*
  (list
   (list "moment of force"                    "момент силы"                             nil            "N*m"        "m^2*kg*s^-2"             (vd* |N|  |m|)         )
   (list "surface tension"                    "поверхностное натяжение"                 nil            "N/m"        "kg*s^-2"                 (vd/ |N|  |m|)         )     
   (list "dynamic viscosity"                  "динамическая вязкость"                   nil            "Pa*s"       "m^-1*kg*s^-1"            (vd* |Pa| |s|)         )
   (list "electric charge density" "пространственная плотность электрического заряда"   nil            "C/m^3"      "m^-3*s*A"                (vd/ |C|  |m| |m| |m|) ) 
   (list "electric flux density"              "электрическое смещение"                  nil            "C/m^2"      "m^-2*s*A"                (vd/ |C|  |m| |m|)     )
   (list "electric field strength"            "напряженность электрического поля"       nil            "V/m"        "m*kg*s^-3*A^-1"          (vd/ |V|  |m|)         )
   (list "permittivity"                       "диэлектрическая проницаемость"           nil            "F/m"        "m^-3*kg-1*s^4*A^2"       (vd/ |F|  |m|)         )
   (list "permeability"                       "магнитная проницаемость"                 nil            "H/m"        "m*kg*s^-2*A^-2"          (vd/ |H|  |m|)         )
   (list "specific energy"                   '("удельная энергия"
					      "теплотворная способность")               nil            "J/kg"       "m^2*s^-2"                (vd/ |J|  |kg|)        )
   (list '("heat capacity" "entropy")        '("теплоемкость системы"
					       "энтропия системы")                      nil            "J/K"        "kg*m^2/(s^2*K)"          (vd/ |J|  |K|)         )
   (list '("specific heat capacity"
	  "specific entropy")                '("удельная теплоёмкость"
                                               "удельная энтропия")                     nil            "J/(kg*K)"   "m^2/(s^2*K)"             (vd/ |J| |kg| |K|)     )
   (list '("heat flux density" "irradiance")  "поверхностная плотность потока энергии"  nil            "W/m^2"      "kg*s^-3"                 (vd/ |W| |m| |m|)      )
   (list "thermal conductivity"                "теплопроводность"                       nil            "W/(m*K)"    "m*kg*s^-3*K^-1"          (vd/ |W| |m| |K|)      )
   (list "molar energy"                        "молярная внутренняя энергия"            nil            "J/mol"      "m^2*kg*s^-2*mol^-1"      (vd/ |J| |mol|)        )
   (list '("molar entropy"
	  "molar heat capacity")             '("молярная энтропия"
                                               "молярная теплоёмкость")                 nil            "J/(mol*K)"  "m^2*kg*s^-2*K^-1*mol^-1" (vd/ |J| |mol| |K|))
   (list "exposure (x and γ rays)"            "экспозиционная доза"                     nil            "C/kg"       "kg^-1*s*A"               (vd/ |C| |kg|)         )
   (list "absorbed dose rate"                 "мощность поглощённой дозы"               nil            "Gy/s"       "m^2*s^-3"                (vd/ |Gy| |s|)         )
   (list "angular velocity"                   "угловая скорость"                        nil            "rad/s"      "s^-1"                    (vd/ |rad| |s|)        )
   (list "angular acceleration"               "угловое ускорение"                       nil            "rad/s^2"    "s^-2"                    (vd/ |rad| |s| |s|)    )
   (list "radiant intensity"                  "сила излучения"                          nil            "W/sr"       "m^4*m^-2*kg*s^-3"        (vd/ |W| |sr|)         )
   (list "radiance"                           "энергетическая яркость"                  nil            "W/(sr*m^2)" "m^2*m^-2*kg*s^-3"        (vd/ |W| |sr| |m| |m|) )
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   (list "energy density"                     "плотноть энергии"                        nil            "J/m^3"      "m^-1*kg^1*s^-2"          (vd/ |J| |m| |m| |m|)  )
   (list "force impulse"                      "импульс силы"                            nil            "N*s"        "m^-1*s^1*A^2"            (vd* |N| |s| )))

  "si-derived-units-whose-names-and-symbols-include-si-derived-units-with-special-names-and-symbols
Задает производные единицы измерения системы SI
Каждый подсписок состоит из следующих элементов:
1 - английское наименование величины
2 - русское наименование величины
3 - размерность величины
4 - международное обозначене единицы
5 - единицы, вараженная через основные и производные единицы СИ
см. ГОСТ 8.417-2002, таблица 4")

