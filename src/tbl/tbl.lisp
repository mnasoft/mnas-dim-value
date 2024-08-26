(defpackage :mnas-dim-value/tbl
  (:use #:cl
        #:mnas-dim-value/class
        #:mnas-dim-value/mk-class)
  (:export *si-main-units*
           *si-derived-units-tbl-02*
           *si-derived-units-tbl-03*
           *not-si-units-tbl-05*
           *not-si-units-tbl-07*
           *other-units-tbl-b-01*
           )
  )

(in-package :mnas-dim-value/tbl)

(defparameter *si-main-units*
  (list
   (list "unitless"            "безразмерный"                  "U" "ul"       "бр"        "ul"  "бр"   (vd 1            ) )
   (list "length"              "длина"	                       "L" "meter"    "метр"      "m"   "м"    (vd 1      :m   1) )
   #+nil
   (list "mass"                "масса"                         "M" "kilogram" "килограмм" "kg"  "кг"   |kg|   )
   (list "mass"                "масса"                         "M" "kilogram" "килограмм" "g"   "г"    (vd 1/1000 :kg   1) )
   (list "time"                "время"                         "T" "second"   "секунда"   "s"   "с"    (vd 1       :s   1) )
   (list "electric current"    "сила тока электрического"      "I" "ampere"   "ампер"     "A"   "А"    (vd 1       :A   1) )
   (list "temperature"         "температура термодинамическая" "Θ" "kelvin"   "кельвин"   "K"   "К"    (vd 1       :K   1) )
   (list "amount of substance" "количество вещества"           "N" "mole"     "моль"      "mol" "моль" (vd 1       :mol 1) )
   (list "luminous intensity"  "сила света"                    "J" "candela"  "кандела"   "cd"  "кд"   (vd 1       :cd  1) ))
  "Задает основные единицы измерения системы SI
Каждый подсписок состоит из следующих элементов:
@begin(enum)
@item(английксое наименование величины;)
@item(русское наименование величины;)
@item(размерность величины;)
@item(английское наименование единицы;)
@item(русское наименование единицы;)
@item(международное обозначение единицы;)
@item(русское обозначение единицы.)
@end(enum)
см. ГОСТ 8.417-2002, таблица 1")

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
	 (vd 1 :m 1 :s -1))
   (list "acceleration"              "ускорение"   nil
	 ""                          "метр на секунду в квадрате"
	 "m/s^2"                     "м/с^2"
	 (vd 1 :m 1 :s -2))
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
	 (vd 1 :mol 1 :m -3))
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

(defparameter *si-derived-units-tbl-03*
  (list
   (list "plane angle"                   "плоский угол" "L/L"
	 "radian"                        "радиан"
	 "rad"                           "рад"             "m^1*m^-1"            (vd 1 :rad 1))
   (list "solid angle"                   "телесный угол" "L^2/L^2"
	 "steradian"                     "стерадиан"
	 "sr"                            "ср"              "m^2*m^-2=1"          (vd 1 :sr 1))
   (list "frequency"                     "частота" nil
	 "hertz"                         "герц"
	 "Hz"                            "Гц"              "s^-1"                (vd 1 :s -1))
   (list "force"                         "сила" nil
	 "newton"                        "ньютон"
	 "N"                             "Н"               "m*kg*s^-2"           (vd 1 :m 1 :kg 1 :s -2))
   (list "pressure"                      "давление" nil
	 "pascal"                        "паскаль"
	 "Pa"                            "Па"              "m^-1*kg*s^-2"        (vd 1 :m -1 :kg 1 :s -2))
   (list '("energy" "work" "quantity of heat") '("энергия" "работа" "количество теплоты") nil
	 "joule"                         "джоуль"
	 "J"                             "Дж"              "kg*m^2/s^2"          (vd 1 :m 2 :kg 1 :s -2))
   (list '("power" "radiant flux")       '("мощность" "поток излучения") nil
	 "watt"                          "ватт"
	 "W"                             "Вт"               "kg*m^2/s^3"         (vd 1 :m 2 :kg 1 :s -3))
   (list '("electric charge" "quantity of electricity")
         '("электрический заряд" "количество электричества") nil
	   "coulomb"                     "кулон"
	   "C"                           "Кл"               "s*A"                (vd 1 :s 1 :A 1))
   (list '("electric potential difference" "electromotive force")
	 '("электрическое напряжение" "электродвижущая сила") nil
	    "volt"                       "вольт"
	    "V"                          "В"                "m^2*kg*s^-3*A^-1"   (vd 1 :m 2 :kg 1 :s -3 :A -1))
    (list "capacitance"                  "электрическая ёмкость" nil
	  "farad"                        "фарад"
	  "F"                            "Ф"                "m^-2*kg^-1*s^4*A^2" (vd 1 :m -2 :kg -1 :s 4 :A 2) )
    (list "electric resistance"          "электрическое сопротивление" nil
	  "ohm"                          "ом"
	  "Ω"                            "Ом"      "m^2*kg*s^-3*A^-2"            (vd 1 :m 2 :kg 1 :s -3 :A -2))
    (list "electric conductance"         "электрическая проводимость" nil
	  "siemens"                      "сименс"
	  "S"                            "См"      "m^-2*kg^-1*s^3*A^2"          (vd 1 :m -2 :kg -1 :s 3 :A 2))
    (list "magnetic flux"                "магнитный поток" nil
	  "weber"                        "вебер"
	  "Wb"                           "Вб"      "m^2*kg*s^-2*A^-1"            (vd 1 :m 2 :kg 1 :s -2 :A -1))
    (list "magnetic flux density"        "магнитная индукция" nil
	  "tesla"                        "тесла"
	  "T"                            "Тл"      "kg*s^-2*A^-1"                (vd 1 :kg 1 :s -2 :A -1))
    (list "inductance"                   "индуктивность" nil
	  "henry"                        "генри"
	  "H"                            "Гн"      "m^2*kg*s^-2*A^-2"            (vd 1 :m 2 :kg 1 :s -2 :A -2))
    (list "Celsius temperature"          "температура по Цельсию" nil
	  "degree Celsius"               "градус Цельсия"
	  "°C"                           "°С"      "K"                           (vd 1 :K 1)) 
    (list "luminous flux"                "световой поток" nil
	  "lumen"                        "люмен"
	  "lm"                           "лм"      "m^2*m^-2*cd=cd"              (vd 1 :cd 1 :sr 1))
    (list "illuminance"                  "освещенность" nil
	  "lux"                          "люкс"
	  "lx"                           "лк"      "m^2*m^-4*cd=m^-2*cd"         (vd 1 :m -2 :cd 1 :sr 1 ))
    (list "activity (referred to a radionuclide)" "активность (радионуклида)" nil
	  "becquerel"                    "беккерель"
	  "Bq"                           "Бк"      "s^-1"                        (vd 1 :s -1))
    (list '("absorbed dose" "specific energy (imparted)" "kerma")
          '("поглощенная доза излучения" "показатель поглощенной дозы" "керма") nil
	  "gray"                         "грей"
	  "Gy"                           "Гр"      "m^2*s^-2"                    (vd 1 :m 2 :s -2))
    (list '("dose equivalent" "ambient dose equivalent" "directional dose equivalent" "personal dose equivalent" "organ equivalent dose")
	  '("эквивалентная доза ионизирующего излучения" "эффективная доза ионизирующего излучения") nil
	  "sievert"                      "зиверт"
	  "Sv"                           "Зв"      "m^2*s^-2"                    (vd 1 :m 2 :s -2))
    (list "catalytic activity"           "активность катализатора" nil
	  "katal"                        "катал"
	  "kat"                          "кат"     "s^-1*mol"                    (vd 1 :s -1 :mol 1)))
  
  "Задает производные единицы СИ, имеющие специальные наименование и обозначения
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

(defparameter *not-si-units-tbl-05*
  (list
   (list "mass"      "масса" nil
	 "ton"       "тонна"
	 "t"         "т"
         (vd 1000 :kg 1) '((0 24)))
   (list "mass"      "масса" nil
	 ""          "атомная единица массы"
	 "u"         "а.е.м."
         (vd 1.660540210d-27 :kg 1) '((-24 24)))
   (list "time"      "время" nil
	 "minute"    "минута"
	 "min"       "мин"
         (vd 60 :s 1) nil)
   (list "time"      "время" nil
	 "hour"      "час"
	 "h"         "ч"
         (vd 3600 :s 1) nil)
   (list "time"      "время" nil
	 "day"       "сутки"
	 "d"         "сут"
         (vd 86400 :s 1) nil)
   (list "plane angle" "плоский угол" nil
	 "degree"    "градус"
	 "°"         "°"
         (vd (/ pi 180) :rad 1) nil)
   (list "plane angle" "плоский угол" nil
	 "minute"    "минута"
	 "'"         "'"
         (vd (/ pi 180 60) :rad 1) nil)
   (list "plane angle" "плоский угол" nil
	 "second"    "секунда"
	 "\""        "\""
         (vd (/ pi 180 60 60) :rad 1) nil)
   (list "plane angle" "плоский угол" nil
	 "gon"       "град"
	 "gon"       "град"
         (vd  (/ pi 200) :rad 1) nil)
   (list "volume"    "объём" nil
	 "liter"     "литр"
	 "l"         "л"
         (vd 1/1000 :m 3) '((-3 -3) (0 3)))
   (list "length"    "длина" nil
	 "astronomical unit" "астрономическая единица"
	 "ua"        "а.е."
         (vd 1.495978706916d11  :m 1) nil) 
   (list "length"    "длина" nil
	 "light year" "световой год"
	 "ly"        "св.год"
         (vd 9.460730472580800d15 :m 1) nil)
   (list "length"    "длина" nil
	 "parsec"    "парсек"
	 "pc"        "пк"
         (vd 3.0856776d16  :m 1) nil)
   (list "optical force" "оптическая сила" nil
	 ""          "диоптрия"
	 "дптр"      "дптр"
         (vd 1 :m -1)  nil)
   (list "area"      "площадь" nil
	 "hectare"   "гектар"
	 "ha"        "га"
         (vd (* 100 100) :m 2)   nil)
   (list "area"      "площадь" nil
	 "are"       "aр"
	 "a"         "а"
         (vd 100 :m 2)   nil)
   (list "energy"    "энергия" nil
	 "electron-volt" "электрон-вольт"
	 "eV"        "эВ"
         (vd 1.60217733d-19 :m 2 :kg 1 :s -2) nil)
   (list "energy"    "энергия" nil
	 "kilowatt-hour" "киловатт-час"
	 "kW*h"      "кВт*ч"
         (vd (* 36/10 1000 1000) :m 2 :kg 1 :s -2) nil) 
   (list "full power" "полная мощность" nil
	 "volt-ampere" "вольт-ампер"
	 "V*A"       "В*А"
         (vd 1 :m 2 :kg 1 :s -3) nil)
   (list "reactive power" "рекативная мощность" nil
	 "var"       "вар"
	 "var"       "вар"
         (vd 1 :m 2 :kg 1 :s -3) nil)
   (list "electric charge" "электрический заряд" nil
	 "ampere hour" "ампер-час"
	 "A*h"       "А*ч"
         (vd (* 36/10 1000) :s 1 :A 1) nil)
   )
  "Внесистемные единицы, допустимые к применению наравне с единицами СИ")

(defparameter *not-si-units-tbl-07*
  (list 
   (list "length"           "длина"               nil
	 "nautical mile"    "морская миля"
	 "nmi"              "миля"
         (vd 1852 :m 1)   nil)
   (list "mass"             "масса"               nil
	 ""                 "карат"
	 "кар"              "кар"
         (vd 2/10000 :kg 1)   nil)
   (list "linear density"   "линейная плотность"  nil
	 ""                 "текс"
	 "tex"              "текс"
         (vd 1/1000000 :kg 1 :m -1)   nil)
   (list "velocity"         "скорость"            nil
	 "knot"             "узел"
	 "kn"               "уз"
         (vd (/ 1852 3600) :m 1 :s -1)   nil)
   (list "acceleration"     "ускорение"           nil
	 ""                 "гал"
	 "Gal"              "Гал"
         (vd 1/100 :m 1 :s -2)   nil)
   (list "rotational speed" "частота вращения"    nil
	 ""                 "оборот в секунду"
	 "r/s"              "об/с"
         (vd (* pi 2)  :rad 1  :s -1)   nil)
   (list "rotational speed" "частота вращения"    nil
	 ""                 "оборот в минуту"
	 "r/min"            "об/мин"
         (vd (* pi 2 1/60) :rad 1 :s -1)   nil)
   (list "pressure"         "давление"            nil
	 ""                 "бар"
	 "bar"              "бар"
         (vd (* 100 000) :m -1 :kg 1 :s -2)      nil)
   )
  "Внесистемные единицы, временно допустимые к применению")

(defparameter *other-units-tbl-b-01*
  (list
   (list "length"        "длина"                nil
	 "angstrom"      "ангстрем"          
	 "Å"             "Å"
         (vd 1d-10 :m 1) nil)
   (list "area"          "площадь"              nil
	 "barn"          "барн"
	 "b"             "б"
         (vd 1d-28 :m -2) nil)
   (list "mass"          "масса"                nil 
	 ""              "центнер"
	 "q"             "ц"
         (vd 100 :kg 1) nil)
   (list "solid angle"   "телесный угол"        nil
	 "square degree" "квадратный градус"
	 "□°"            "□°"
         (vd (/ (* pi pi) (* 180 180)) :sr 1)   nil) 
   (list "force"         "сила"                 nil
	 ""              "дина"
	 "dyn"           "дин"
         (vd  1/100000 :m 1 :kg 1 :s -2) nil)
   (list "force"         "сила"                 nil
	 ""	         "килограмм-сила"
	 "kgf"           "кгс"
         (vd 9.8065d0 :m 1 :s -2 :kg 1) nil)

   (list "force"         "сила"                 nil
	 ""	         "килопонд"
	 "kp"            "kp"
         (vd 9.8065d0 :m 1 :s -2 :kg 1) nil)
   
   (list "force"         "сила"                 nil
	 ""	         "грамм-сила"
	 "gf"            "гс"
         (vd (* 1/1000 9.8065d0) :m 1 :s -2 :kg 1) '((-24 3)))
   (list "force"         "сила"                 nil
	 ""	         "понд"
	 "p"             "p"
         (vd (* 1/1000 9.8065d0) :m 1 :s -2 :kg 1)
	 '((-24 24)))
   (list "force"         "сила"                 nil
	 ""              "тонна-сила"
	 "tf"            "тс"
         (vd (* 1000 9.8065d0) :m 1 :s -2 :kg 1)
	 '((0 24)))
   (list "pressure"      "давление"             nil 
	 ""              "килограмм-сила на квадратный сантиметр"
	 "kgf/cm^2"      "кгс/см^2"
         (vd (/ 9.8065d0 1/100 1/100) :m -1 :s -2 :kg 1) nil)
   (list "pressure"      "давление"             nil
	 ""              "килопонд на квадратный сантиметр"
	 "kp/cm^2"       "kp/cm^2"
         (vd (/ 9.8065d0 1/100 1/100) :m -1 :s -2 :kg 1)   nil)
   (list "pressure"      "давление"             nil
	 ""              "метр водяного столба"
	 "m_H2O"         "м вод. ст."
         (vd (* 1000 9.8065d0) :m -1 :kg 1 :s -2) '((-3 24)))
   (list "pressure"      "давление"             nil
	 ""              "метр ртутного столба"
	 "m_Hg"          "м_pт._ст."
         (vd  133322d0 :m -1 :kg 1 :s -2) '((-3 24)))
   (list "pressure"      "давление"             nil
	 ""              "торр"
	 "Torr"          "Торр"
         (vd 133.322d0 :m -1 :kg 1 :s -2) '((-24 24)))
   (list "stress"        "напряжение"           nil
	 ""              "килограмм-сила на квадратный миллиметр"
	 "kgf/mm^2"      "кгс/мм^2"
         (vd (/ 9.8065d0 1/1000 1/1000) :m -1 :s -2 :kg 1)
         nil)
   (list "stress"        "напряжение"           nil
	 ""              "килопонд на квадратный миллиметр"
	 "kp/mm^2"       "kp/mm^2"
         (vd (/ 9.8065d0 1/1000 1/1000) :m -1 :s -2 :kg 1) nil)
   (list '("energy" "work" "quantity of heat")
         '("работа" "энергия")   nil
	 ""              "эрг"
	 "erg"           "эрг"
         (vd (/ 1 10 1000 1000) :m 2 :kg 1 :s -2) '((-24 24)))
   (list "power"         "мощность"             nil
	 "horsepower"              "лошадиная сила"
	 "hp"            "л.с."
         (vd (* 75 9.8065d0) :kg 1 :m 2 :s -3 ) nil)

   (list "kinematic viscosity" "динамическая вязкость" nil
	 ""              "пуаз"
	 "P"             "П"
         (vd 1/10 :m -1 :kg 1 :s -1) '((-24 24)))
   (list "kinematic viscosity" "кинематическая вязкость" nil
	 ""              "стокс"
	 "St"            "Ст"
         (vd 1/10000 :m 2 :s -1) '((24 24)))
;;;; ....
   (list "quantity of heat"
         '("количество теплоты" "термодинамический потециал") nil
	 ""              "калория"
	 "cal"           "кал"
         (vd 4.1868 :kg 1 :m 2  :s -2) '((-24 24)))
   (list "quantity of heat"
         '("количество теплоты" "термодинамический потециал") nil
	 ""              "калория термохимическая"
	 "cal_{th}"      "кал_{тх}"
         (vd 4.1840 :kg 1 :m 2  :s -2) '((-24 24)))
   (list "heat of chemical reaction" '("теплота химической рекции") nil
	 ""              "калория  15-градусная"
	 "cal_{15}"      "кал_{15}"
         (vd 4.1855 :kg 1 :m 2  :s -2) '((-24 24)))
;;;; ....
   (list "length"        "длина" nil
	 ""              "микрон"
	 "μ"             "мк"
         (vd (/ 1 1000 1000) :m 1) nil)
   (list "angle of rotation" "угол поворота"    nil
	 ""              "оборот"
	 "r"             "об"
         (vd (* 2 pi) :rad 1) nil)
;;;; ....
   (list "area"          "площадь"              nil
	 ""              "ар"
	 "a"             "а"
         (vd 100 :m 2) '((0 2)))
   )
  "Соотношение некоторых внесистемных единиц с единицами СИ"
  )
