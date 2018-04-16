;;;; si-units.lisp

(in-package #:mnas-dim-value)

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
   (list ""               "длина"               nil
	 ""               "морская миля"
	 "n mile"         "миля"                (vd 1852 :m 1))
   (list ""               "масса"    nil
	 ""               "карат"
	 ""               "кар"                 (vd 2/10000 :kg 1))
   (list ""               "линейная плотность"  nil
	 ""               "текс"
	 "tex"            "текс"                (vd 1/1000000 :kg 1 :m -1))
   (list ""               "скорость"            nil
	 ""               "узел"
	 "kn"             "уз"                  (vd/ |m| (vd/ 3600 1852.0 |s|)))
   (list ""               "ускорение"           nil
	 ""               "гал"
	 "Gal"            "Гал"                 (vd 1/100 :m 1 :s -2))
   (list ""               "частота вращения"    nil
	 ""               "оборот в секунду"
	 "r/s"            "об/с"                (vd 1  :rad 1  :s -1))
   (list ""               "частота вращения"    nil
	 ""               "оборот в минуту"
	 "r/min"          "об/мин"              (vd 1/60 :rad 1 :s -1))
   (list ""               "давление"            nil
	 ""               "бар"
	 "bar"            "бар"                 (vd* 100000 |Pa|))
   )
  "Внесистемные единицы, временно допустимые к применению")

(defparameter *other-units-tbl-b-01*
  (list
   (list "length"        "длина"                nil
	 "angstrom"      "ангстрем"
	 "Å" "Å"                                (vd* 1.0e-10 |m|))
   (list "area"          "площадь"              nil
	 "barn"          "барн"
	 "b"             "б"                    (vd* 1e-28 |m| |m|))
   (list ""              "масса"
	 ""              "центнер"
	 "q"             "ц"                    (vd* 100 |kg|))
   (list ""              "телесный угол"        nil
	 ""              "квадратный градус"
	 "□˚"            "□˚"                   (vd* |sr| (/ (* pi pi) (* 180 180)))) ;;;; "ڤ˚" "□˚"
   (list "force"         "сила"                 nil
	 ""              "дина"
	 "dyn"           "дин"                  (vd* 1/100000 |N|))
   (list "force"         "сила"                 nil
	 ""	         "килограмм-сила"
	 "kgf"           "кгс"                  (vd* 1 *g*     |kg|))

   (list "force"         "сила"                 nil
	 ""	         "килопонд"
	 "kp"            "-"                    (vd* 1 *g*     |kg|))
   (list "force"         "сила"                 nil
	 ""	         "грамм-сила"
	 "gf"            "гс"                   (vd* 1/1000 *g*     |kg|))
   (list "force"         "сила"                 nil
	 ""	         "понд"
	 "p"             "-"                    (vd* 1 *g*     |kg|))
   (list "force"         "сила"                 nil
	 ""              "тонна-сила"
	 "tf"            "тс"                   (vd* 1000 *g*       |kg|))
   (list "pressure"      "давление"
	 ""              "килограмм-сила на квадратный сантиметр"
	 "kgf/cm^2"      "кгс/см^2"             (vd/ (vd* |kg| *g*) 1/100 1/100 |m| |m|))
   (list "pressure"      "давление"
	 ""              "килопонд на квадратный сантиметр"
	 "kp/cm^2"       "-"                    (vd/ (vd* |kg| *g*) 1/100 1/100 |m| |m|))
   (list "pressure"      "давление"             nil
	 ""              "миллиметр водяного столба"
	 "mm H2O"        "мм вод. ст."          (vd* 9.80665 |Pa|))
   (list "pressure"      "давление"             nil
	 ""              "миллиметр ртутного столба"
	 "mm Hg"         "мм pт. ст."           (vd* 133.322 |Pa|))
   (list "pressure"      "давление"             nil
	 ""              "торр"
	 "Torr"          "-"                    (vd* 133.322 |Pa|))
   (list "stress"        "напряжение"
	 ""              "килограмм-сила на квадратный миллиметр"
	 "kgf/mm^2"      "кгс/мм^2"             (vd/ (vd* |kg| *g*) 1/1000 1/1000 |m| |m|))
   (list "stress"        "напряжение"
	 ""              "килопонд на квадратный миллиметр"
	 "kp/mm^2"       "-"                    (vd/ (vd* |kg| *g*) 1/1000 1/1000 |m| |m|))
   (list ""              '("работа" "энергия")  nil
	 ""              "эрг"
	 "erg"           "эрг"                  (vd/ |J| 10 1000 1000))
   (list ""              "мощность"             nil
	 ""              "лошадиная сила"
	 "-"             "л.с"                  (vd* 75 *g* (vd/ |m| |s|)))
   (list "kinematic viscosity" "динамическая вязкость" nil
	 ""              "пуаз"
	 "P"             "П"                    (vd* 1/10 |Pa| |s|))
   (list "kinematic viscosity" "кинематическая вязкость" nil
	 ""              "стокс"
	 "St"            "Ст"                   (vd 1/10000 :m 2 :s -1))
;;;; ....
   (list "quantity of heat" '("количество теплоты" "термодинамический потециал") nil
	 ""              "калория"
	 "cal"           "кал"                  (vd* 4.1868 |J|))
   (list "quantity of heat" '("количество теплоты" "термодинамический потециал") nil
	 ""              "калория термохимическая"
	 "cal_{th}"      "кал_{тх}"             (vd* 4.1840 |J|))
   (list "" '("теплота химической рекции")      nil
	 ""              "калория  15-градусная"
	 "cal_{15}"      "кал_{15}"             (vd* 4.1855 |J|))
;;;; ....
   (list ""              "длина"                nil
	 ""              "микрон"
	 "μ"             "мк"                   (vd/ |m| 1000 1000))
   (list ""              "угол поворота"         nil
	 ""              "оборот"
	 "r"             "об"                   (vd* 2 pi |rad|))
;;;; ....
   (list ""              "площадь"              nil
	 ""              "ар"
	 "a"             "а"                   (vd* 100 |m| |m|)))
  "Соотношение некоторых внесистемных единиц с единицами СИ"
  )

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

(defparameter *nd-not-si-units-tbl-07*
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
   *not-si-units-tbl-07*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

   (list '("pressure" "stress") '("давление" "напряжение")  "" "" "m_Hg"  "м pт.ст."     (vd* 133322.0       |Pa|))
   (list '("pressure" "stress") '("давление" "напряжение")  "" "" "m_H2O"  "м вод.ст."   (vd* 9806.65        |Pa|))
   (list '("pressure" "stress") '("давление" "напряжение")  "" "" "kgf/m^2"  "кгс/м^2"    (vd/ *g*  |m| |m|))
   (list "atm"   (vd* 101325         |Pa|))
   (list "psi"   (vd/
		  ( vd* 0.45359237 *g* |kg|)
		  ( vd* 0.0254 |m|)
		  ( vd* 0.0254 |m|)))
   (list "length" "длина" "in"    (vd* 0.0254         |m|))
   (list "lb"    (vd* 0.45359237     |kg|))
   (list "lbf"   (vd* 0.45359237 *g* |kg|))
