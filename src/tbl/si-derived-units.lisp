;;;; si-units-derived.lisp

(in-package :mnas-dim-value)

;;@intern


;;@intern


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
