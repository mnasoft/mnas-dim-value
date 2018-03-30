;;;; razmernost.lisp

(in-package #:mnas-dim-value)

(defparameter *mult-prefix*
  '(
    (24  "йотта" "Yotta" "И"  "Y")
    (21  "зетта" "Zetta" "З"  "Z")
    (18  "экса"  "Exa"   "Э"  "E")
    (15  "пета"  "Peta"  "П"  "P")
    (12  "тера"  "Tera"  "Т"  "T")
    (9 	 "гига"  "Giga"  "Г"  "G")
    (6 	 "мега"  "Mega"  "М"  "M")
    (3 	 "кило"  "kilo"  "к"  "k")
    (2 	 "гекто" "hecto" "г"  "h")
    (1 	 "дека"  "deca"  "да" "da")
    (0   ""      ""      ""   "")
    (-1  "деци"  "deci"  "д"  "d")
    (-2  "санти" "centi" "с"  "c")
    (-3  "милли" "milli" "м"  "m")
    (-6  "микро" "micro" "мк" "μ")
    (-9  "нано"  "nano"  "н"  "n")
    (-12 "пико"  "pico"  "п"  "p")
    (-15 "фемто" "femto" "ф"  "f")
    (-18 "атто"  "atto"  "а"  "a")
    (-21 "зепто" "zepto" "з"  "z")
    (-24 "йокто" "yocto" "и"  "y"))
  "*muti-prefix-data* содержит множителные приставки;
Каждый подсписок содержит описание одной множительной приставки в следующем формате:
1 - степень в которую необходимо возвести 10 для раскрытия приставки;
2 - наименование множителя русское;
3 - наименование множителя международное;
4 - обозначение множителя русское;
5 - обозначение множителя международное;"
  )


(defparameter *international-mut-prefix* (make-hash-table :test 'equal))

(mapc #'(lambda (el)
	  (setf (gethash (fifth el) *international-mut-prefix*)
		(expt 10 (first el))))
      *mult-prefix*)

;;;;(gethash "μ" *international-mut-prefix*)

(defun prefix-from->to(x str-prefix-from str-prefix-to)
  "Перевод значения числа х, предваряемого приставкой str-prefix-from,
в число с приставкой str-prefix-to
Пример использования:
5.5 ΜPa -> 5500 kPa
(prefix-from->to 5.5 \"M\" \"k\")
=> 5500.0
(prefix-from->to 5.5 \"\" \"k\")
=> 0.0055
(prefix-from->to 5.5 \"\" \"\")
=> 1.0
"
  (* x (/ (gethash str-prefix-from *international-mut-prefix*)
	  (gethash str-prefix-to *international-mut-prefix*))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *si-main*
  '(("length"              "длина"	                   "L" "meter"    "метр"      "m"   "м")
    ("mass"                "масса"                         "M" "kilogram" "килограмм" "kg"  "кг")
    ("time"                "время"                         "T" "second"   "секунда"   "s"   "с")
    ("electric current"    "сила тока электрического"      "I" "ampere"   "ампер"     "A"   "А")
    ("temperature"         "температура термодинамическая" "Θ" "kelvin"   "кельвин"   "K"   "К")
    ("amount of substance" "количество вещества"           "N" "mole"     "моль"      "mol" "моль")
    ("luminous intensity"  "сила света"                    "J" "candela"  "кандела"   "cd"  "кд")
    )
  
  "Задает основные единицы измерения системы SI
Каждый подсписок состоит из следующих элементов:
1 - английксое наименование величины
2 - русское наименование величины
3 - размерность величины
4 - английское наименование единицы
5 - русское наименование единицы
6 - международное обозначение единицы
7 - русское обозначение единицы
см. ГОСТ 8.417-2002, таблица 1")

(defparameter *SI-derived-units-expressed-in-terms-of-base-units*
  '(("area"                      "площадь"                           nil             "m^2"      )
    ("volume"                    "объём"                             nil             "m^3"      )

    ("velocity"                  "скорость"                          nil             "m/s"      )
    ("acceleration"              "ускорение"                         nil             "m/s^2"    )

    ("wave number"               "волновое число"                    nil             "1/m"      )

    (("density" "mass density")  "плотность"                         nil             "kg/m^3"   )
    ("specific volume"           "удельный объём"                    nil             "m^3/kg"   )

    ("current density"           "плотность электрического тока"     nil             "A/m"      )
    ("magnetic field strength"   "напряжённость магнитного поля"     nil             "A/m"      )

    ("molar concentration"       "молярная концентрация компонента"  nil             "mol/m^3")
    ("luminance"                 "яркость"                           nil             "cd/m^2")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ("momentum"                 ("импульс"
				 "количество движения")              nil             "kg*m/s")
    ("kinematic viscosity"      "кинематическая вязкость"            nil             "m^2/s")
    ("moment of inertia"        "момент инерции"                     nil             "kg*m^2" )
    ("mass flow rate"           "массовый расход"                    nil             "kg/s")
    ("volumetric flow"          "объёмный расход"                    nil             "m^3/s")
    ("flow rate"                "молекулярный расход"                nil             "mol/s")
    ("molar weight"             "молекулярный вес"                   nil             "kg/mol")
    



    )
  "Производные единицы СИ, наименования и обозначения которых образованы с использованием
 наименований и обозначений основных единиц СИ.
Каждый подсписок состоит из следующих элементов:
1 - английское наименование величины
2 - русское наименование величины
3 - размерность
4 - международное обозначение единицы
см. ГОСТ 8.417-2002 таблица 2")

(defparameter *SI-derived-units-with-special-names-and-symbols*
  '(("plane angle"                   "плоский угол"                             "L/L"      "radian"       "радиан"     "rad" "рад" nil      "m^1*m^-1=1")
    ("solid angle"                   "телесный угол"                            "L^2/L^2"  "steradian"    "стерадиан"  "sr"  "ср"  nil      "m^2*m^-2=1")
    ("frequency"                     "частота"                                   nil       "hertz"        "герц"       "Hz"  "Гц"  nil      "s^-1")
    ("force"                         "сила"                                      nil       "newton"       "ньютон"     "N"   "Н"   nil      "m*kg*s^-2")
    (("pressure"
      "stress")                     ("давление"
		                     "напряжение")                               nil       "pascal"       "паскаль"    "Pa"  "Па"  "N/m^2"  "m^-1*kg*s^-2")
    (("energy" "work"          
      "quantity of heat")           ("энергия" "работа"
			             "количество теплоты")                       nil       "joule"        "джоуль"     "J"   "Дж"  "N*m"    "kg*m^2/s^2")
    (("power"
      "radiant flux")               ("мощность"
			             "поток излучения")                          nil       "watt"         "ватт"       "W"   "Вт"  "J/s"    "kg*m^2/s^3")
    (("electric charge"
      "quantity of electricity")    ("электрический заряд"
				     "количество электричества")                 nil       "coulomb"      "кулон"      "C"   "Кл"  nil      "s*A")
    (("electric potential difference"
      "electromotive force")        ("электрическое напряжение"
				     "электродвижущая сила")                     nil       "volt"         "вольт"      "V"   "В"   "W/A"    "m^2*kg*s^-3*A^-1")
    ("capacitance"                  "электрическая ёмкость"                      nil       "farad"        "фарад"      "F"   "Ф"   "C/V"    "m^-2*kg^-1*s^4*A^2")
    ("electric resistance"          "электрическое сопротивление"                nil       "ohm"          "ом"         "Ω"   "Ом"  "V/A"    "m^2*kg*s^-3*A^-2")
    ("electric conductance"         "электрическая проводимость"                 nil       "siemens"      "сименс"     "S"   "См"  "A/V"    "m^-2*kg^-1*s^3*A^2")
    ("magnetic flux"                "магнитный поток"                            nil       "weber"        "вебер"      "Wb"  "Вб"  "V*s"    "m^2*kg*s^-2*A^-1")
    ("magnetic flux density"        "магнитная индукция"                         nil       "tesla"        "тесла"      "T"   "Тл"  "Wb/m^2" "kg*s^-2*A^-1")
    ("inductance"                   "индуктивность"                              nil       "henry"        "генри"      "H"   "Гн"  "Wb/A"   "m^2*kg*s^-2*A^-2")
    ("Celsius temperature"          "температура по Цельсию"                     nil "degree Celsius" "градус Цельсия" "°C"  "°С"  nil      "K")
    ("luminous flux"                "световой поток"                             nil       "lumen"        "люмен"      "lm"  "лм"  "cd*sr"  "m^2*m^-2*cd=cd")
    ("illuminance"                  "освещенность"                               nil       "lux"          "люкс"       "lx"  "лк"  "lm/m^2" "m^2*m^-4*cd=m^-2*cd")
    ("activity (referred to a radionuclide)"
                                    "активность (радионуклида)"                  nil       "becquerel"    "беккерель"  "Bq"  "Бк"  nil      "s^-1")
    (("absorbed dose"
      "specific energy (imparted)"
      "kerma")                      ("поглощенная доза излучения"
				     "показатель поглощенной дозы"
				     "керма")                                    nil       "gray"         "грей"       "Gy"  "Гр"  "J/kg"   "m^2*s^-2")
    (("dose equivalent"
      "ambient dose equivalent"
      "directional dose equivalent"
      "personal dose equivalent"
      "organ equivalent dose")      ("эквивалентная доза ионизирующего излучения"
                                     "эффективная доза ионизирующего излучения") nil "sievert" "зиверт"  "Sv"  "Зв"  "J/kg"   "m^2*s^-2")
    ("catalytic activity"           "активность катализатора"                    nil       "katal"        "катал"       "kat" "кат" nil      "s^-1*mol"))
  
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

;;;;(mapcar #'(lambda(el) (string= (fourth el)(sixth el))) *si-derive*)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defparameter *SI-derived-units-whose-names-and-symbols-include-SI-derived-units-with-special-names-and-symbols*
  '(("moment of force"                    "момент силы"                             nil            "N*m"        "m^2*kg*s^-2")
    ("surface tension"                    "поверхностное натяжение"                 nil            "N/m"        "kg*s^-2" )
    ("dynamic viscosity"                  "динамическая вязкость"                   nil            "Pa*s"       "m^-1*kg*s^-1")
    ("electric charge density" "пространственная плотность электрического заряда"   nil            "C/m^3"      "m^-3*s*A")
    ("electric flux density"              "электрическое смещение"                  nil            "C/m^2"      "m^-2*s*A")
    ("electric field strength"            "напряженность электрического поля"       nil            "V/m"        "m*kg*s^-3*A^-1")
    ("permittivity"                       "диэлектрическая проницаемость"           nil            "F/m"        "m^-3*kg-1*s^4*A^2")
    ("permeability"                       "магнитная проницаемость"                 nil            "H/m"        "m*kg*s^-2*A^-2")
    ("specific energy"                   ("удельная энергия"
					  "теплотворная способность")               nil            "J/kg"       "m^2*s^-2")
    (("heat capacity" "entropy")          ("теплоемкость системы"
					   "энтропия системы")                      nil            "J/K"        "kg*m^2/(s^2*K)")
    (("specific heat capacity"
      "specific entropy")                 ("удельная теплоёмкость"
					   "удельная энтропия")                     nil            "J/(kg*K)"   "m^2/(s^2*K)")
    (("heat flux density" "irradiance")    "поверхностная плотность потока энергии" nil            "W/m^2"      "kg*s^-3" )
    ("thermal conductivity"                "теплопроводность"                       nil            "W/(m*K)"    "m*kg*s^-3*K^-1")
    ("molar energy"                        "молярная внутренняя энергия"            nil            "J/mol"      "m^2*kg*s^-2*mol^-1")
    (("molar entropy"
      "molar heat capacity")              ("молярная энтропия"
					   "молярная теплоёмкость")                 nil            "J/(mol*K)"  "m^2*kg*s^-2*K^-1*mol^-1")
    ("exposure (x and γ rays)"            "экспозиционная доза"                     nil            "C/kg"       "kg^-1*s*A")
    ("absorbed dose rate"                 "мощность поглощённой дозы"               nil            "Gy/s"       "m^2*s^-3")
    ("angular velocity"                   "угловая скорость"                        nil            "rad/s"      "s^-1")
    ("angular acceleration"               "угловое ускорение"                       nil            "rad/s^2"    "s^-2")
    ("radiant intensity"                  "сила излучения"                          nil            "W/sr"       "m^4*m^-2*kg*s^-3")
    ("radiance"                           "энергетическая яркость"                  nil            "W/(sr*m^2)" "m^2*m^-2*kg*s^-3")
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ("energy density"                     "плотноть энергии"                        nil            "J/m^3"      "m^-1*kg^1*s^-2")
    ("force impulse"                      "импульс силы"                            nil            "N*s"        "m^-1*s^1*A^2")
    
    )

  "Задает производные единицы измерения системы SI
Каждый подсписок состоит из следующих элементов:
1 - английское наименование величины
2 - русское наименование величины
3 - размерность величины
4 - международное обозначене единицы
5 - единицы, вараженная через основные и производные единицы СИ
см. ГОСТ 8.417-2002, таблица 4")



(defparameter *aaa*
  '(

    ("магнитодвижушая сила"               "магнитодвижушая сила"              "ампер"         "A"        "A")

    ("mass fraction"                     "массовая доля"  "kg/kg")

    ("сила излучения" "radiant intensity")
    
    ("catalytic concentration" "catalytic concentration")
    
    ("магнитодвижушая сила" "магнитодвижушая сила")
    
    
    ("magnetic field strength" "напряжённость магнитного поля" )

    ("mass concentration" "kg/m^3")
    ("number concentration" "1/m^3")
    ("volume concentration" "m^3/m^3")
    ))
