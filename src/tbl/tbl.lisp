(defpackage :mnas-dim-value/tbl
  (:use #:cl
        #:mnas-dim-value/class
        #:mnas-dim-value/mk-class
        )
  (:export *table-1-the-seven-defining-constants-of-the-si-and-the-seven-corresponding-units-they-define*
           *nd-table-2-si-base-units*
           *nd-table-4-the-22-si-units-with-special-names-and-symbols*
           *nd-table-5-examples-of-coherent-derived-units-in-the-si-expressed-in-terms-of-base-units*
           *nd-table-6-examples-of-si-coherent-derived-units-whose-names-and-symbols-include-si-coherent-derived-units-with-special-names-and-symbols*
           *table-7-si-prefixes*
           *nd-table-8-non-si-units-accepted-for-use-with-the-si-units*
           )
  (:export *nd-table-9-others*
           ))

(in-package :mnas-dim-value/tbl)

(defparameter *nd-table-2-si-base-units*           nil)
(defparameter *nd-table-4-the-22-si-units-with-special-names-and-symbols* nil)
(defparameter *nd-table-5-examples-of-coherent-derived-units-in-the-si-expressed-in-terms-of-base-units* nil)
(defparameter *nd-table-6-examples-of-si-coherent-derived-units-whose-names-and-symbols-include-si-coherent-derived-units-with-special-names-and-symbols* nil)
(defparameter *nd-table-8-non-si-units-accepted-for-use-with-the-si-units* nil)

(defparameter *table-7-si-prefixes* ; *mult-prefix*
  '((30  "кветта" "quetta" "Кв" "Q")
    (27  "ронна"  "ronna"  "Рн" "R")
    (24  "йотта"  "yotta"  "И"  "Y")
    (21  "зетта"  "zetta"  "З"  "Z")
    (18  "экса"   "exa"    "Э"  "E")
    (15  "пета"   "peta"   "П"  "P")
    (12  "тера"   "tera"   "Т"  "T")
    (9 	 "гига"   "giga"   "Г"  "G")
    (6 	 "мега"   "mega"   "М"  "M")
    (3 	 "кило"   "kilo"   "к"  "k")
    (2 	 "гекто"  "hecto"  "г"  "h")
    (1 	 "дека"   "deca"   "да" "da")
;;; (0   ""      ""        ""   "")
    (-1  "деци"   "deci"   "д"  "d")
    (-2  "санти"  "centi"  "с"  "c")
    (-3  "милли"  "milli"  "м"  "m")
    (-6  "микро"  "micro"  "мк" "μ")
    (-9  "нано"   "nano"   "н"  "n")
    (-12 "пико"   "pico"   "п"  "p")
    (-15 "фемто"  "femto"  "ф"  "f")
    (-18 "атто"   "atto"   "а"  "a")
    (-21 "зепто"  "zepto"  "з"  "z")
    (-24 "йокто"  "yocto"  "и"  "y")
    (-27 "ронто"  "ronto"  "рн" "r")
    (-30 "квекто" "quecto" "кв" "q")
    )
  "*table-7-si-prefixes* содержит множителные приставки;
Каждый подсписок содержит описание одной множительной приставки в
следующем формате:

1 - power - степень в которую необходимо возвести 10 для раскрытия приставки;
2 - nm-ru - наименование множителя русское;
3 - nm-en - наименование множителя международное;
4 - s-ru  - обозначение множителя русское;
5 - s-en  - обозначение множителя международное.")

(defparameter *table-1-the-seven-defining-constants-of-the-si-and-the-seven-corresponding-units-they-define* ; *si-defining-constants*
  `(("Defining constant"                    "Symbol"   "Numerical value" "Unit")
    ("hyperfine transition frequency of Cs" "Δv_{Cs}"  9192631770        ,(vd 1 :s -1))
    ("speed of light in vacuum"             "c"        299792458         ,(vd 1 :m 1  :s -1))
    ("Planck constant"                      "h"        6.62607015d-34    ,(vd 1 :m 2 :kg 1 :s -1))
    ("elementary charge"                    "e"        1.602176634d-19   ,(vd 1 :s 1 :A 1))
    ("Boltzmann constant"                   "k"        1.380649d-23      ,(vd 1 :m 2 :kg 1 :s -2 :K -1))
    ("Avogadro constant"                    "N_{A}"    6.02214076d+23    ,(vd 1 :mol -1))
    ("luminous efficacy"                    "Kcd"      683               ,(vd 1 :cd 1 :sr 1 :m -2 :kg -1 :s 3))
    )
  "Table 1. The seven defining constants of the SI and the seven
corresponding units they define")

(defparameter *si-base-units*
  `(
;;;;("q-nm-en"             "q-nm-ru"                       "dim" "u-nm-en" "u-nm-r"  "u-s-en" "u-s-ru" "val"                "coeff")
    ("unitless"            "безразмерный"                  "U"   "ul"      "бр"      "ul"     "бр"     ,(vd 1             ) )
    ("length"              "длина"	                   "L"   "meter"   "метр"    "m"      "м"      ,(vd 1       :m   1) ((-30 30)))
    ("mass"                "масса"                         "M"   "gram"    "грамм"   "g"      "г"      ,(vd 1/1000  :kg  1) ((-30 30)))
    ("time"                "время"                         "T"   "second"  "секунда" "s"      "с"      ,(vd 1       :s   1) ((-30 30)))
    ("electric current"    "сила тока электрического"      "I"   "ampere"  "ампер"   "A"      "А"      ,(vd 1       :A   1) ((-30 30)))
    ("temperature"         "температура термодинамическая" "Θ"   "kelvin"  "кельвин" "K"      "К"      ,(vd 1       :K   1) ((-30 30)))
    ("amount of substance" "количество вещества"           "N"   "mole"    "моль"    "mol"    "моль"   ,(vd 1       :mol 1) ((-30 30)))
    ("luminous intensity"  "сила света"                    "J"   "candela" "кандела" "cd"     "кд"     ,(vd 1       :cd  1) ((-30 30))))

  )

(block table-2-si-base-units ; si-base-units
  (nd-clear)
  (nd "unitless"            "безразмерный"                  "U"   "ul"      "бр"      "ul"     "бр"     (vd 1             )  nil)
  (nd "length"              "длина"	                    "L"   "meter"   "метр"    "m"      "м"      (vd 1       :m   1) )
  (nd "mass"                "масса"                         "M"   "gram"    "грамм"   "g"      "г"      (vd 1/1000  :kg  1) )
  (nd "time"                "время"                         "T"   "second"  "секунда" "s"      "с"      (vd 1       :s   1) )
  (nd "electric current"    "сила тока электрического"      "I"   "ampere"  "ампер"   "A"      "А"      (vd 1       :A   1) )
  (nd "temperature"         "температура термодинамическая" "Θ"   "kelvin"  "кельвин" "K"      "К"      (vd 1       :K   1) )
  (nd "amount of substance" "количество вещества"           "N"   "mole"    "моль"    "mol"    "моль"   (vd 1       :mol 1) )
  (nd "luminous intensity"  "сила света"                    "J"   "candela" "кандела" "cd"     "кд"     (vd 1       :cd  1) )
  (setf *nd-table-2-si-base-units* (nd-get))
  (setf (documentation  '*nd-table-2-si-base-units* 'variable) 
        "Задает основные единицы измерения системы SI.

(см. Таблица 1, ГОСТ 8.417-2002; Table 2. SI base units)"))

(block table-4-the-22-si-units-with-special-names-and-symbols
  (nd-clear)
  (nd "plane angle" "плоский угол" "L/L"
      "radian" "радиан" "rad" "рад"
      (vd 1 :rad 1) )                   ; "rad=m/m"
  (nd "solid angle" "телесный угол" "L^2/L^2"
      "steradian" "стерадиан" "sr" "ср"
      (vd 1 :sr 1))                     ; "sr=m^2/m^2"
  (nd "frequency" "частота" "1/T"
      "hertz" "герц" "Hz" "Гц"
      (vd 1 :s -1) )                    ; "Hz=s^-1"
  (nd "force" "сила" nil
      "newton" "ньютон" "N" "Н"
      (vd 1 :m 1 :kg 1 :s -2) )         ; "N=kg*m*s^-2"
  (nd "pressure, stress" "давление" nil
      "pascal" "паскаль" "Pa" "Па"
      (vd 1 :m -1 :kg 1 :s -2) )        ; "Pa=kg*m^−1*s^−2=N/m^2"
  (nd "energy, work, quantity of heat" "энергия, работа, количество теплоты" nil
      "joule" "джоуль" "J" "Дж"
      (vd 1 :m 2 :kg 1 :s -2) )         ; "J=kg*m^2*s^-2=N*m"
  (nd "power, radiant flux" "мощность, поток излучения" nil
      "watt" "ватт" "W" "Вт"
      (vd 1 :m 2 :kg 1 :s -3) )         ; "W=kg*m^2*s^−3=J/s"
  (nd "electric charge, quantity of electricity" "электрический заряд, количество электричества" nil
      "coulomb" "кулон" "C" "Кл"
      (vd 1 :s 1 :A 1) )                ; "C=A*s"
  (nd "electric potential difference, electromotive force" "электрическое напряжение, электродвижущая сила" nil
      "volt" "вольт" "V" "В" 
      (vd 1 :m 2 :kg 1 :s -3 :A -1) )   ; "V=kg*m^2*s^−3*A^−1=W/A"
  (nd "capacitance" "электрическая ёмкость" nil
      "farad" "фарад"  "F" "Ф"
      (vd 1 :m -2 :kg -1 :s 4 :A 2)  )  ; "F=kg−1 m−2 s4 A2=C/V"
  (nd "electric resistance" "электрическое сопротивление" nil
      "ohm" "ом" "Ω" "Ом" (vd 1 :kg 1 :m 2  :s -3 :A -2) ) ; "Ω = kg m2 s−3 A−2=V/A"
  (nd "electric conductance" "электрическая проводимость" nil
      "siemens" "сименс" "S" "См" 
      (vd 1 :m -2 :kg -1 :s 3 :A 2) )   ; "S = kg−1 m−2 s3 A2=A/V"
  (nd "magnetic flux" "магнитный поток" nil
      "weber" "вебер" "Wb" "Вб" 
      (vd 1 :m 2 :kg 1 :s -2 :A -1) )   ; "Wb = kg m2 s−2 A−1=V s"
  (nd "magnetic flux density" "магнитная индукция" nil
      "tesla" "тесла" "T" "Тл" 
      (vd 1 :kg 1 :s -2 :A -1) )        ; "T = kg s−2 A−1 Wb/m2"
  (nd "inductance" "индуктивность" nil
      "henry" "генри" "H" "Гн" 
      (vd 1 :m 2 :kg 1 :s -2 :A -2) )   ; "H = kg m2 s−2 A−2=Wb/A"
  (nd "Celsius temperature" "температура по Цельсию" nil
      "degree Celsius" "градус Цельсия" "°С" "°С" 
      (vd 1 :K 1) nil)                  ; "°С=K"
  (nd "luminous flux" "световой поток" nil
      "lumen" "люмен" "lm" "лм" 
      (vd 1 :cd 1 :sr 1) )              ; "lm = cd sr = cd sr"
  (nd "illuminance" "освещенность" nil
      "lux" "люкс" "lx" "лк" 
      (vd 1 :cd 1 :sr 1 :m -2) )        ; "lx = cd sr m−2=lm/m2"
  (nd "activity referred to a radionuclide" "активность радионуклида" nil
      "becquerel" "беккерель" "Bq" "Бк" 
      (vd 1 :s -1) )                    ; "Bq = s−1"
  (nd "absorbed dose, kerma"
      "поглощенная доза излучения, керма" nil
      "gray" "грей" "Gy" "Гр" 
      (vd 1 :m 2 :s -2) )               ; "Gy = m2 s−2 J/kg"
  (nd "dose equivalent" "эквивалентная доза ионизирующего излучения" nil
      "sievert" "зиверт" "Sv" "Зв" 
      (vd 1 :m 2 :s -2) )               ; "Sv = m2 s−2=J/kg"
  
  (nd "catalytic activity" "активность катализатора" nil
      "katal" "катал" "kat" "кат" 
      (vd 1 :s -1 :mol 1) )             ; "kat = mol s−1"
  (setf *nd-table-4-the-22-si-units-with-special-names-and-symbols* (nd-get))
  (setf (documentation  '*nd-table-4-the-22-si-units-with-special-names-and-symbols* 'variable)
        "Задает производные единицы СИ, имеющие специальные наименование и обозначения.

(см. Таблица 3, ГОСТ 8.417-2002; Table 4. The 22 SI units with special names and symbols)"))

(block table-5-examples-of-coherent-derived-units-in-the-si-expressed-in-terms-of-base-units 
  (nd-clear)
  (nd "area" "площадь" nil
      "square meter" "квадратный метр" "m^2" "м^2"
      (vd 1 :m 2) )                     ; "m2"
  (nd "volume" "объём" nil
      "cubic meter" "кубический метр" "m^3" "м^3"
      (vd 1 :m 3)  )                    ; "m3"
  (nd "speed, velocity" "скорость" nil
      "meter per second" "метр в секунду" "m/s" "м/с"
      (vd 1 :m 1 :s -1) )               ; "m s−1"
  (nd "acceleration" "ускорение"   nil
      "meter per second in a square" "метр на секунду в квадрате" "m/s^2" "м/с^2"
      (vd 1 :m 1 :s -2) )               ; "m s−2"
  (nd "wavenumber" "волновое число" nil
      "one per meter" "единица на метр" "1/m" "1/м"
      (vd 1 :m -1) nil)                 ; "m−1"
  (nd "density, mass density" "плотность" nil
      "kilogram per cubic meter" "килограмм на кубический метр" "kg/m^3" "кг/м^3"
      (vd 1 :kg 1 :m -3) )              ; "kg m−3"
  (nd "surface density" "поверхностная плотность" nil
      "kilogram per square meter" "килограмм на квадратный метр" "kg/m^2" "кг/м^2"
      (vd 1 :kg 1 :m -2) )              ; "kg m−2"
  (nd "specific volume" "удельный объём" nil
      "cubic meter per kilogram" "кубический метр на килограмм" "m^3/kg" "м^3/кг"
      (vd 1 :m 3 :kg -1) )              ; "m3 kg−1"
  (nd "current density" "плотность электрического тока" nil
      "ampere per square meter" "ампер на квадратный метр" "A/m^2" "А/м^2"
      (vd 1 :A 1 :m -2) )               ; "A m−2"
  (nd "magnetic field strength" "напряжённость магнитного поля" nil
      "ampere per meter" "ампер на метр" "A/m" "А/м"
      (vd 1 :A 1 :m -1) )               ; "A m−1"
  (nd "amount of substance concentration" "молярная концентрация компонента" nil
      "mole per cubic meter" "моль на кубический метр" "mol/m^3" "моль/м^3"
      (vd 1 :mol 1 :m -3) )             ; "mol m−3"
  (nd "mass concentration" "массовая концентрация"  nil
      "kilogram per cubic meter" "килограмм на кубический метр" "kg/m^3" "кг/м^3"
      (vd 1 :kg -1 :m 3) )              ; "kg m−3"
  (nd "luminance" "яркость" nil
      "candela per square meter" "кандела на квадратный метр" "cd/m^2" "кд/м^2"
      (vd 1 :cd 1 :m -2) )  ; "cd m−2"
;;;; Дополнительные примеры величин начало
  (nd "momentum" "импульс, количество движения" nil
      "kilogram meter per second" "килограмм-метр на секунду" "kg*m/s" "кг*м/с"
      (vd 1 :kg 1 :m 1 :s -1))
  (nd "kinematic viscosity" "кинематическая вязкость" nil
      "square meter per second" "квадратный метр на секунду" "m^2/s" "м^2/с"
      (vd 1 :m 2 :s -1))
  (nd "moment of inertia" "момент инерции" nil
      "kilogram meter squared" "килограмм-квадратный метр"
      "kg*m^2" "кг*м^2"
      (vd 1 :kg 1 :m 2))
  (nd "mass flow rate" "массовый расход" nil
      "kilogram per second" "килограмм в секунду" "kg/s" "кг/с"
      (vd 1 :kg 1 :s -1))
  (nd "volumetric flow" "объёмный расход" nil
      "cubic meter per second" "кубический метр в секунду" "m^3/s" "м^3/с"
      (vd 1 :m 3 :s -1))
  (nd "flow rate" "молекулярный расход" nil
      "mole per second" "моль в секунду" "mol/s"  "моль/с"
      (vd 1 :mol 1 :s -1))
  (nd "molar weight" "молекулярный вес" nil
      "kilogram per mole" "килограмм на моль" "kg/mol" "кг/моль"
      (vd 1 :kg 1 :mol -1))
;;;; Дополнительные примеры величин конец
  (setf *nd-table-5-examples-of-coherent-derived-units-in-the-si-expressed-in-terms-of-base-units* (nd-get))
  (setf (documentation '*nd-table-5-examples-of-coherent-derived-units-in-the-si-expressed-in-terms-of-base-units* 'variable)
        "Производные единицы СИ, наименования и обозначения которых
 образованы с использованием наименований и обозначений основных
 единиц СИ.

(см. Таблица 2, ГОСТ 8.417-2002; Table 5. Examples of coherent derived
units in the SI expressed in terms of base units)"))

(block table-6-examples-of-si-coherent-derived-units-whose-names-and-symbols-include-si-coherent-derived-units-with-special-names-and-symbols ; si-derived-units-tbl-06
  (nd-clear)
  (nd "dynamic viscosity" "динамическая вязкость" nil
      "pascal second" "паскаль-секунда" "Pa*s" "Па*с"
      (vd 1 :kg 1 :m -1 :s -1) )        ; "Pa s=kg m-1 s-1"
  (nd "moment of force" "момент силы" nil
      "newton metre" "ньютон-метр" "N*m" "Н*м"
      (vd 1  :kg 1 :m 2 :s -2) )        ; "N m=kg m2 s-2"
  (nd "surface tension" "поверхностное натяжение" nil
      "" "ньютон-метр" "N/m" "Н*м" 
      (vd 1 :kg 1 :s -2) )              ; "N m-1=kg s-2"
  (nd "angular velocity, angular frequency" "угловая скорость" nil
      "radian per second" "радиан в секунду" "rad/s" "рад/с"
      (vd 1 :rad 1 :s -1) )             ; "rad s−1=s−1"
  (nd "angular acceleration" "угловое ускорение" nil
      "radian per second squared" "радиан на секунду в квадрате" "rad/s^2" "рад/с^2"     
      (vd 1 :rad 1 :s -2) )             ; "rad s−2=s−2"
  (nd "heat flux density, irradiance" "поверхностная плотность потока энергии"  nil
      "watt per square metre" "ватт на квадратный метр" "W/m^2" "Вт/м^2"
      (vd 1 :kg 1 :s -3) )              ; "W m−2=kg s−3"
  (nd "heat capacity, entropy" "теплоемкость системы, энтропия системы" nil
      "joule per kelvin" "джоуль на кельвин" "J/K" "Дж/К"
      (vd 1 :kg 1 :m 2 :s -2 :K -1) )   ; "J K−1=kg m2 s−2 K−1"
  (nd "specific heat capacity, specific entropy" "удельная теплоёмкость, удельная энтропия" nil
      "joule per kilogram kelvin" "джоуль на килограмм-кельвин" "J/(kg*K)" "Дж/(кг*К)"
      (vd 1 :m 2 :s -2 :K -1) )         ; "J K−1 kg−1=m2 s−2 K−1"
  (nd "specific energy" "удельная энергия" nil
      "joule per kilogram" "джоуль на килограмм" "J/kg" "Дж/кг"
      (vd 1 :m 2 :s -2))                ; "J kg−1=m2 s−2"
  (nd "thermal conductivity" "теплопроводность" nil
      "watt per metre kelvin" "ватт на метр-кельвин" "W/(m*K)" "Вт/(м*К)"
      (vd 1 :kg 1 :m 1 :s -3 :K -1 ))   ; "W m−1 K−1=kg m s−3 K−1"
  (nd "energy density" "плотноть энергии" nil
      "joule per cubic metre" "джоуль на кубический метр" "J/m^3" "Дж/м^3"
      (vd 1  :kg 1 :m -1 :s -2))        ; "J m−3=kg m−1 s−2"
  (nd "electric field strength" "напряженность электрического поля" nil
      "volt per metre" "воль на метр" "V/m" "В/м"
      (vd 1 :kg 1 :m 1 :s -3 :A -1))    ; "V m−1=kg m s−3 A−1"
  (nd "electric charge density" "пространственная плотность электрического заряда" nil
      "coulomb per cubic metre" "кулон на кубический метр" "C/m^3" "Кл/м^3"
      (vd 1 :A 1 :s 1 :m -3))           ; "C m−3=A s m−3"
  (nd "surface charge density" "поверхностная плотность заряда" nil
      "coulomb per square metre" "кулон на квадратный метр" 
      "C/m^2" "Кл/м^2"
      (vd 1 :A 1 :s 1 :m -2))           ; "C m−2=A s m−2"
  (nd "electric flux density, electric displacement" "плотность электрического потока, электрическое смещение" nil 
      "coulomb per square metre" "кулон на квадратный метр" 
      "C/m^2" "Кл/м^2"
      (vd 1 :A 1 :s 1 :m -2))           ; "C m−2=A s m−2"
  (nd "permittivity" "диэлектрическая проницаемость" nil
      "farad per metre" "фарад на метр" "F/m" "Ф/м"
      (vd 1 :kg -1 :m -3 :s 4 :A 2))    ; "F m−1=kg−1 m−3 s4 A2"
  (nd "permeability" "магнитная проницаемость" nil
      "henry per metre" "генри на метр" "H/m" "Гн/м"
      (vd 1 :kg 1 :m 1 :s -2 :A -2))    ; "H m−1=kg m s−2 A−2"
  (nd "molar energy" "молярная внутренняя энергия" nil
      "joule per mole" "джоуль на моль" "J/mol" "Дж/моль"
      (vd 1 :kg 1 :m 2 :s -2 :mol -1))  ;  "J mol−1=kg m2 s−2 mol−1"
  (nd "molar entropy, molar heat capacity" "молярная энтропия, молярная теплоёмкость" nil
      "joule per mole kelvin" "джоуль на моль-кельвин" "J/(mol*K)" "Дж/(моль*К)"
      (vd 1 :kg 1 :m 2 :s -2 :mol -1 :K -1)) ; "J K−1 mol−1=kg m2 s−2 mol−1 K−1"
  (nd "exposure (x- and γ- rays)" "экспозиционная доза фотонного излучения" nil
      "coulomb per kilogram" "кулон на килограмм" "C/kg" "Кл/кг"
      (vd 1 :A 1 :s 1 :kg -1))          ; "C kg−1=A s kg−1"
  (nd "absorbed dose rate" "мощность поглощённой дозы" nil
      "gray per second" "грей в секунду" "Gy/s" "Гр/с"
      (vd 1 :m 2 :s -3))                ; "Gy s−1=m2 s−3"
  (nd "radiant intensity" "сила излучения" nil
      "watt per steradian" "ватт на стерадиан" "W/sr" "Вт/ср"
      (vd 1 :kg 1 :m 2 :s -3 :sr -1))   ;  "W sr−1=kg m2 s−3"
  (nd "radiance" "энергетическая яркость" nil
      "watt per square metre steradian" "ватт на стерадан-кадратный метр" "W/(sr*m^2)" "Вт/(ср*м^2)"
      (vd 1 :kg 1 :s -3 :sr -1))        ; "W sr−1 m−2=kg s−3"
  (nd "catalytic activity concentration" "концентрация каталитической активности" nil
      "katal per cubic metre" "катал на кубический метр" "kat/m^3" "кат/м^3"
      (vd 1 :mol 1 :s -1 :m -3))  ; "kat m−3=mol s−1 m−3"
;;;; Дополнительные примеры начало  
  (nd "force impulse" "импульс силы" nil
      "newton second" "ньютон-секунда" "N*s" "Н*с"
      (vd 1 :kg 1 :m 1 :s -1))         ; "N s=kg*m*s^-1"
;;;; Дополнительные примеры конец
  (setf *nd-table-6-examples-of-si-coherent-derived-units-whose-names-and-symbols-include-si-coherent-derived-units-with-special-names-and-symbols* (nd-get))
  (setf (documentation  '*nd-table-6-examples-of-si-coherent-derived-units-whose-names-and-symbols-include-si-coherent-derived-units-with-special-names-and-symbols* 'variable)
        "Производные единицы СИ, наименования и обозначения
которых образованы с использованием специальных наименований и
обозначений.

(см. Таблица 4, ГОСТ 8.417-2002; Table 6. Examples of SI coherent
derived units whose names and symbols include SI coherent derived
units with special names and symbols"))

(block table-8-non-si-units-accepted-for-use-with-the-si-units
  (nd-clear)
  (nd "time" "время" nil "minute" "минута" "min" "мин"
      (vd 60 :s 1) nil)
  (nd "time" "время" nil "hour" "час" "h" "ч"
      (vd 3600 :s 1) nil)
  (nd "time" "время" nil "day" "сутки" "d" "сут"
      (vd 86400 :s 1) nil)
;;;;
  (nd "length" "длина" nil "astronomical unit" "астрономическая единица" "au" "а.е."
      (vd 149597870700d0 :m 1) nil)
;;;;
  (nd "plane angle" "плоский угол" nil "degree" "градус" "°" "°"
         (vd (/ pi 180) :rad 1) nil)
   (nd "plane angle" "плоский угол" nil "minute" "минута" "'" "'"
         (vd (/ pi 180 60) :rad 1) nil)
   (nd "plane angle" "плоский угол" nil "second" "секунда" "\"" "\""
         (vd (/ pi 180 60 60) :rad 1) nil)
;;;;
   (nd "area" "площадь" nil "hectare" "гектар" "ha" "га"
       (vd (* 100 100) :m 2)   nil)
;;;;
   (nd "volume" "объём" nil "liter" "литр" "l" "л"
       (vd 1/1000 :m 3) '((-3 -3) (0 3)))
;;;;
   (nd "mass" "масса" nil "ton" "тонна" "t" "т"
       (vd 1000 :kg 1) '((0 24)))
   (nd "mass" "масса" nil "dalton" "атомная единица массы" "Da" "а.е.м."
       (vd 1.6605390666050d-27 :kg 1))
;;;;
   (nd "energy" "энергия" nil "electronvolt" "электрон-вольт" "eV" "эВ"
       (vd 1.602176634d-19 :m 2 :kg 1 :s -2) nil)
;;;; logarithmic neper (h) Np see text
;;;; ratio quantities bel (h) B
;;;; decibel (h) dB
;;;; Дополнительные величины начало
   (nd "plane angle" "плоский угол" nil "gon" "град" "gon" "град"
         (vd  (/ pi 200) :rad 1) nil)
   (nd "length"    "длина" nil "light year" "световой год" "ly" "св.год"
         (vd 9.460730472580800d15 :m 1) nil)
   (nd "length" "длина" nil "parsec" "парсек" "pc" "пк"
         (vd 3.0856776d16  :m 1) nil)
   (nd "optical force" "оптическая сила" nil "dioptre" "диоптрия" "dpt" "дптр"
         (vd 1 :m -1)  nil)
   (nd "area" "площадь" nil "are" "aр" "a" "а"
         (vd 100 :m 2)   nil)
   (nd "energy" "энергия" nil "kilowatt-hour" "киловатт-час" "kW*h" "кВт*ч"
         (vd (* 36/10 1000 1000) :m 2 :kg 1 :s -2) nil) 
   (nd "full power" "полная мощность" nil "volt ampere" "вольт-ампер" "V*A" "В*А"
         (vd 1 :m 2 :kg 1 :s -3) nil)
   (nd "reactive power" "рекативная мощность" nil "var" "вар" "var" "вар"
         (vd 1 :m 2 :kg 1 :s -3) nil)
   (nd "electric charge" "электрический заряд" nil "ampere hour" "ампер-час" "A*h" "А*ч"
       (vd (* 36/10 1000) :s 1 :A 1) nil)
;;;; Дополнительные величины конец
     (setf *nd-table-8-non-si-units-accepted-for-use-with-the-si-units* (nd-get))
     (setf (documentation  '*nd-stable-8-non-si-units-accepted-for-use-with-the-si-units* 'variable)
           "Внесистемные единицы, допустимые к применению наравне с единицами СИ"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(block table-9-others
  (nd-clear)
;;; "mass" "масса"  
  (nd "mass" "масса" nil "quintal" "центнер" "q" "ц"
      (vd 100 :kg 1) nil)
;;; "area" "площадь"
  (nd "area" "площадь" nil "ar" "ар" "a" "а"
      (vd 100 :m 2) '((0 0) (2 2)))
;;; "angle of rotation" "угол поворота"
  (nd "angle of rotation" "угол поворота" nil "turn" "оборот" "tr" "об"
      (vd (* 2 pi) :rad 1) nil)  
  
;;; "length" "длина"  
  (nd "length" "длина" nil "nautical mile" "морская миля" "nmi" "миля"
      (vd 1852 :m 1)   nil)
  (nd "length" "длина" nil "angstrom" "ангстрем" "Å" "Å"
      (vd 1d-10 :m 1) nil)
  (nd "length" "длина" nil "micron" "микрон" "μ" "мк"
      (vd (/ 1 1000 1000) :m 1) nil)
  
;;; "force" "сила"
  (nd "force" "сила" nil "gram-force" "грамм-сила" "gf" "гс"
      (vd (* 1/1000 9.80665d0) :m 1 :s -2 :kg 1) '((-24 3)))
  (nd "force" "сила" nil "ton-force" "тонна-сила" "tf" "тс"
      (vd (* 1000 9.80665d0) :m 1 :s -2 :kg 1) '((0 24)))
  (nd "force" "сила" nil "pound" "понд" "p" "п"
      (vd (* 1/1000 9.80665d0) :m 1 :s -2 :kg 1) '((0 0) (3 9)))
  
;;; "pressure" "давление"
  (nd "pressure" "давление" nil "meter of water column" "метр водяного столба" "m_H2O" "м_вод._ст."
      (vd (* 1000 9.80665d0) :m -1 :kg 1 :s -2) '((-3 0)))
  (nd "pressure" "давление" nil "meter of mercury" "метр ртутного столба" "m_Hg" "м_pт._ст."
      (vd  133322d0 :m -1 :kg 1 :s -2) '((-3 0)))
  (nd "pressure" "давление" nil "torr" "торр" "Torr" "Торр"
      (vd 133.322d0 :m -1 :kg 1 :s -2) '((-6 9)))
  (nd "pressure" "давление" nil "bar" "бар" "bar" "бар"
      (vd (* 100 000) :m -1 :kg 1 :s -2) '((-3 -3) (3 9)))
;;; "power" "мощность" 
  (nd "power" "мощность" nil "horsepower" "лошадиная сила" "hp" "л.с."
      (vd (* 75 9.80665d0) :kg 1 :m 2 :s -3 ) nil)
;;; "rotational speed" "частота вращения"
  (nd "rotational speed" "частота вращения" nil "cycle per minute" "оборот в секунду" "cps" "об/с"
      (vd (* pi 2)  :rad 1  :s -1)   nil)
  (nd "rotational speed" "частота вращения" nil "revolution per minute" "оборот в минуту" "rpm" "об/мин"
      (vd (* pi 2 1/60) :rad 1 :s -1)   nil) 

  (setf *nd-table-9-others* (nd-get))
  (setf (documentation  '*nd-table-9-others* 'variable)
        "Соотношение некоторых внесистемных единиц с единицами СИ"))
