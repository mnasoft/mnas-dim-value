;;;; si-units.lisp

(in-package #:mnas-dim-value)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *not-si-units-tbl-05*
  (list
   (list "mass"      "масса" nil
	 "ton"       "тонна"
	 "t"         "т"                 (vd* 1000 |kg|))
   (list "mass"      "масса" nil
	 ""          "атомная единица массы"
	 "u" "а.е.м."                    (vd 1.660540210d-27 :kg 1))
   (list "time"      "время" nil
	 "minute"    "минута"
	 "min"       "мин"               (vd* 60             |s|))
   (list "time"      "время" nil
	 "hour"      "час"
	 "h"         "ч"                 (vd* 3600           |s|))
   (list "time"      "время" nil
	 "day"       "сутки"
	 "d"         "сут"               (vd* 86400          |s|))
   (list "plane angle" "плоский угол" nil
	 "degree"    "градус"
	 "°"         "°"                 (vd* (/ pi 180) |rad|))
   (list "plane angle" "плоский угол" nil
	 "minute"    "минута"
	 "'"         "'"                 (vd* (/ pi 180 60) |rad|))
   (list "plane angle" "плоский угол" nil
	 "second"    "секунда"
	 "\""        "\""                (vd* (/ pi 180 60 60) |rad|))
   (list "plane angle" "плоский угол" nil
	 "gon"       "град"
	 "gon"       "град"              (vd* (/ pi 200) |rad|))
   (list "volume"    "объём" nil
	 "liter"     "литр"
	 "l"         "л"                 (vd* 1/1000 |m| |m| |m|))
   (list "length"    "длина" nil
	 "astronomical unit" "астрономическая единица"
	 "ua"        "а.е."              (vd* 1.495978706916d11  |m|))
   (list "length"    "длина" nil
	 "light year" "световой год"
	 "ly"        "св.год"            (vd* 9.460730472580800d15 |m|))
   (list "length"    "длина" nil
	 "parsec"    "парсек"
	 "pc"        "пк"                (vd* 3.0856776d16  |m|))
   (list "optical force" "оптическая сила" nil
	 ""          "диоптрия"
	 "1/m"       "дптр"              (vd/ |m|))
   (list "area"      "площадь" nil
	 "hectare"   "гектар"
	 "ha"        "га"                (vd* 100 100 |m| |m|))
   (list "area"      "площадь" nil
	 "are"       "aр"
	 "a"         "а"                 (vd* 100     |m| |m|)) ;;;; 
   (list "energy"    "энергия" nil
	 "electron-volt" "электрон-вольт"
	 "eV"        "эВ"                (vd* 1.60217733e-19 |J|))
   (list "energy"    "энергия" nil
	 "kilowatt-hour" "киловатт-час"
	 "kW*h"      "кВт*ч"             (vd* 3.6 1000 1000 |J|))
   (list "full power" "полная мощность" nil
	 "volt-ampere" "вольт-ампер"
	 "V*A"       "В*А"               (vd* |W|))
   (list "reactive power" "рекативная мощность" nil
	 "var"       "вар"
	 "var"       "вар"               (vd* |W|))
   (list "electric charge" "электрический заряд" nil
	 "ampere hour" "ампер-час"
	 "A*h"       "А*ч"               (vd* 3.6 1000 |C|))
   )
  "Внесистемные единицы, допустимые к применению наравне с единицами СИ")

(defparameter *not-si-units-tbl-07*
  (list
   (list "" "длина"              "морская миля"     "n mile" "миля"    (vd 1852 :m 1))
   (list "" "масса"              "карат"            ""       "кар"     (vd 2/10000 :kg 1))
   (list "" "линейная плотность" "текс"             "tex"    "текс"    (vd 1/1000000 :kg 1 :m -1))
   (list "" "скорость"           "узел"             "kn"     "уз"      (vd/ |m| (vd/ 3600 1852.0 |s|)))
   (list "" "ускорение"          "гал"              "Gal"    "Гал"     (vd 1/100 :m 1 :s -2))
   (list "" "частота вращения"   "оборот в секунду" "r/s"    "об/с"    (vd 1    :s -1))
   (list "" "частота вращения"   "оборот в минуту"  "r/min"  "об/мин"  (vd 1/60 :s -1))
   (list "" "давление"           "бар"              "bar"    "бар"     (vd* 100000 |Pa|))
   )
  "Внесистемные единицы, временно допустимые к применению")

(defparameter *other-units-tbl-b-01*
  (list
   (list "length" "длина"   "angstrom" "ангстрем" "Å" "Å"                                (vd* 1.0e-10        |m|))
   (list "area"   "площадь" "barn"     "барн"     "b" "b"                                (vd* 1e-28      |m| |m|))
   
   (list "force" "сила" "" "тонна-сила" "tf" "тс"                                        (vd* 1000 *g*       |kg|))
   (list "force" "сила" "" "грамм-сила" "gf" "гс"                                        (vd* 1/1000 *g*     |kg|))

   (list '("pressure" "stress") '("давление" "напряжение")  "" "" "m_Hg"  "м pт.ст."     (vd* 133322.0       |Pa|))
   (list '("pressure" "stress") '("давление" "напряжение")  "" "" "mm_Hg"  "мм pт.ст."   (vd* 133.322        |Pa|))
   
   (list '("pressure" "stress") '("давление" "напряжение")  "" "" "m_H2O"  "м вод.ст."   (vd* 9806.65        |Pa|))
   (list '("pressure" "stress") '("давление" "напряжение")  "" "" "mm_H2O" "мм вод.ст."  (vd* 9.80665        |Pa|))

   (list '("pressure" "stress") '("давление" "напряжение")  "" "" "kgf/mm^2" "кгс/мм^2"   (vd/ *g* 1/1000 1/1000 |m| |m|))
   (list '("pressure" "stress") '("давление" "напряжение")  "" "" "kgf/cm^2" "кгс/см^2"   (vd/ *g* 1/100 1/100 |m| |m|)) 
   (list '("pressure" "stress") '("давление" "напряжение")  "" "" "kgf/m^2"  "кгс/м^2"    (vd/ *g*  |m| |m|))

   (list "kinematic viscosity"      "кинематическая вязкость" "" "стокс" "St" "Ст"     (vd 1/10000 :m 2 :s -1))
   
   (list '("energy" "work" "quantity of heat") '("энергия" "работа" "количество теплоты") "" "калория" "cal" "кал" (vd* 4.1868         |J|))

   (list "St"    (vd  1/10000 :m 2 :s -1))

   (list "atm"   (vd* 101325         |Pa|))
   (list "Torr"  (vd*
		  (/ 101325.0 760)   |Pa|))
   (list "psi"   (vd/
		  ( vd* 0.45359237 *g* |kg|)
		  ( vd* 0.0254 |m|)
		  ( vd* 0.0254 |m|)))
   
   (list "length" "длина" "in"    (vd* 0.0254         |m|))
   (list "lb"    (vd* 0.45359237     |kg|))
   (list "lbf"   (vd* 0.45359237 *g* |kg|))
   )
  "Соотношение некоторых внесистемных единиц с единицами СИ"
  )

(defparameter *dim-type*
  '(("length"   ("Mm" "km" "m" "mm" ))
    ("mass"     ("kt" "t" "kg" "g"  ))
    ("time"     ("d" "h" "min" "s"))
    ("electric current")
    ("temperature")
    ("amount of substance")
    ("luminous intensity")
    ("pressure" ("MPa" "kPa" "Pa" "kgf/mm^2" "kgf/cm^2" "kgf/m^2" "mm_Hg" "mm_H2O"))
			   
    ("force"    ("MN" "kN" "N" "tf" "kgf" "gf") )))

(concatenate 'list  "€")


"□̊˚"
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *nd-not-si-units-tbl-05*
  (mapcar
   #'(lambda (el)
              (make-instance 'nd
		      :quantity-name-en (first el)
		      :quantity-name-ru (second el)
		      :dimension-symbol (third el)
		      :unit-name-en     (fourth el)
		      :unit-name-ru     (fifth el)
		      :unit-symbol-en   (sixth el)
		      :unit-symbol-ru   (seventh  el)
		      :value            (car (last el))
		      )
       )
   *not-si-units-tbl-05*))

("mass" "масса" "ton" "тонна" "t" "т" 1000 kg)

(nd-value            (first *nd-not-si-units-tbl-05*))
(nd-quantity-name-ru (first *nd-not-si-units-tbl-05*))
(nd-unit-name-ru     (first *nd-not-si-units-tbl-05*))
(nd-unit-symbol-ru   (first *nd-not-si-units-tbl-05*))

(first *not-si-units-tbl-05*)
