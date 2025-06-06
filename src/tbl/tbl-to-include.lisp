;;;; ./src/tbl/tbl-to-include.lisp

(in-package :mnas-dim-value/tbl)



(defparameter *not-si-units-tbl-07*
  `(
    ("mass" "масса"
            nil
	    "carat"                 "карат"
	    "кар"              "кар"
            ,(vd 2/10000 :kg 1)   nil)
    ("linear density"   "линейная плотность"  nil
	                ""                 "текс"
	                "tex"              "текс"
                        ,(vd 1/1000000 :kg 1 :m -1)   nil)
    ("velocity"         "скорость"            nil
	                "knot"             "узел"
	                "kn"               "уз"
                        ,(vd (/ 1852 3600) :m 1 :s -1)   nil)
    ("acceleration"     "ускорение"           nil
	                ""                 "гал"
	                "Gal"              "Гал"
                        ,(vd 1/100 :m 1 :s -2)   nil)
    
    )
  "Внесистемные единицы, временно допустимые к применению")

(defparameter *other-units-tbl-b-01*
  `(
    ("area"          "площадь"              nil
	             "barn"          "барн"
	             "b"             "б"
                    ,(vd 1d-28 :m -2) nil)
    
    ("solid angle"   "телесный угол"        nil
	             "square degree" "квадратный градус"
	             "□°"            "□°"
                    ,(vd (/ (* pi pi) (* 180 180)) :sr 1)   nil) 
    ("force"         "сила"                 nil
	             ""              "дина"
	             "dyn"           "дин"
                    ,(vd  1/100000 :m 1 :kg 1 :s -2) nil)

    (("energy" "work" "quantity of heat")
          ("работа" "энергия")   nil
	  ""              "эрг"
	  "erg"           "эрг"
         ,(vd (/ 1 10 1000 1000) :m 2 :kg 1 :s -2) ((-30 30)))
    

    ("kinematic viscosity" "динамическая вязкость" nil
	                   ""              "пуаз"
	                   "P"             "П"
                          ,(vd 1/10 :m -1 :kg 1 :s -1) ((-30 30)))
    ("kinematic viscosity" "кинематическая вязкость" nil
	                   ""              "стокс"
	                   "St"            "Ст"
                          ,(vd 1/10000 :m 2 :s -1) ((24 24)))
    
;;;; ....
    
    )
  "Соотношение некоторых внесистемных единиц с единицами СИ"
  )

(defparameter *tbl-g1*
  '(
    "Часть I пространство и время"
    (("Плоский угол") ("rad" "рад") ("mrad" "мрад" "μrad" "мкрад") ("…°" "…′" "…″") ("-"))
    (("Телесный угол") ("sr" "cp") ("-") ("-") ("-"))
    (("Длина") ("m" "м") ("km" "км" "cm" "см" "mm" "мм" "μm" "мкм" "nm" "нм") ("-") ("-"))
    (("Площадь") ("m2" "м2") ("km2" "км2" "dm2" "дм2" "cm2" "см2" "mm2" "мм2") ("-") ("-"))
    (("Объем" "вместимость") ("m3" "м3") ("dm3" "дм3" "cm3" "см3" "mm3" "мм3") ("l" "л") ("hl" "гл" "dl" "дл" "cl" "cл" "ml" "мл"))
    (("Время") ("s" "с") ("ks" "кс" "ms" "мс" "μs" "мкс" "ns" "нc") ("d" "сут" "h" "ч" "min" "мин") ("-"))
    (("Скорость") ("m/s" "м/с") ("-") ("-") ("km/h" "км/ч"))
    (("Ускорение") ("m/s2" "м/с2") ("-") ("-") ("-"))
    "Часть II Периодические и связанные с ними явления"
    (("Частота периодического процесса") ("Hz" "Гц") ("THz" "ТГц" "GHz" "ГГц" "MHz" "МГц" "kHz" "кГц") ("-") ("-"))
    (("Частота вращения") ("s-1" "с-1") ("-") ("min-1" "мин-1") ("-"))
    "Часть III Механика"
    (("Масса") ("kg" "кг") ("Mg" "Мг" "g" "г" "mg" "мг" "μg" "мкг") ("t" "т") ("Mt" "Мт" "kt" "кт" "dt" "дт"))
    (("Линейная плотность") ("kg/m" "кг/м") ("mg/m" "мг/м или" "g/km" "г/км") ("-") ("-"))
    (("Плотность") ("kg/m3" "кг/м3") ("Mg/m3" "Мг/м3" "kg/dm3" "кг/дм3" "g/cm3" "г/см3") ("t/m3" "т/м3 или kg/l" "кг/л") ("g/ml" "г/мл" "g/l" "г/л"))
    (("Количество движения") ("kg⋅m/s" "кг⋅м/с") ("-") ("-") ("-"))
    (("Момент количества движения") ("kg⋅m2/s" "кг⋅м2/с") ("-") ("-") ("-"))
    (("Момент инерции" "динамический момент инерции") ("kg⋅m2" "кг⋅м2") ("-") ("-") ("-"))
    (("Сила" "вес") ("N" "Н") ("MN" "МН" "kN" "кН" "mN" "мН" "μN" "мкН") ("-") ("-"))
    (("Момент силы") ("N⋅m" "Н⋅м") ("MN·m" "МН·м" "kN·m" "кН·м" "mN·m" "мН·м" "μN·m" "мкН·м") ("-") ("-"))
    (("Давление") ("Ра" "Па") ("GPa" "ГПа" "МРа" "МПа" "kPa" "кПа" "mРа" "мПа" "μРа" "мкПа") ("-") ("-"))
    (("Нормальное напряжение" "касательное напряжение") ("Ра" "Па") ("GPa" "ГПа" "МРа" "МПа" "kPa" "кПа") ("-") ("-"))
    (("Динамическая вязкость") ("Pa·s" "Па·с") ("Pa·s" "мПа·с") ("-") ("-"))
    (("Кинематическая вязкость") ("m2/s" "м2/с") ("mm2/s" "мм2/с") ("-") ("-"))
    (("Поверхностное натяжение") ("N/m" "Н/м") ("mN/m" "мН/м") ("-") ("-"))
    (("Энергия" "работа") ("J" "Дж") ("TJ" "ТДж" "GJ" "ГДж" "MJ" "МДж" "kJ" "кДж" "mJ" "мДж") ("-") ("-"))
    (("Мощность") ("W" "Вт") ("GW" "ГВт" "MW" "МВт" "kW" "кВт" "mW" "мВт" "μW" "мкВт") ("-") ("-"))
    (("Термодинамическая температура") ("K" "К") ("МК" "МК" "kK" "кК" "mК" "мК" "μК" "мкК") ("-") ("-"))
    (("Температура Цельсия") ("°C" "°С") ("-") ("-") ("-"))
    (("Температурный интервал") ("K" "К" "°C" "°С") ("-") ("-") ("-"))
    (("Температурный коэффициент") ("К-1" "К-1") ("-") ("-") ("-"))
    (("Теплота" "количество теплоты") ("J" "Дж") ("TJ" "ТДж" "GJ" "ГДж" "MJ" "МДж" "kJ" "кДж" "mJ" "мДж") ("-") ("-"))
    (("Тепловой поток") ("W" "Вт") ("kW" "кВт") ("-") ("-"))
    (("Теплопроводность") ("W/(m·K)" "Вт/(м·К)") ("-") ("-") ("-"))
    (("Коэффициент теплопередачи") ("W/(m2·K)" "Вт/(м2·К)") ("-") ("-") ("-"))
    (("Теплоемкость") ("J/K" "Дж/К") ("kJ/К" "кДж/К") ("-") ("-"))
    (("Удельная теплоемкость") ("J/(kg·K)" "Дж/(кг·К)") ("kJ/(kg·K)" "кДж/(кг·К)") ("-") ("-"))
    (("Энтропия") ("J/K" "Дж/К") ("kJ/K" "кДж/К") ("-") ("-"))
    (("Удельная энтропия") ("J/(kg·K)" "Дж/(кг·К)") ("kJ/(kg·K)" "кДж/(кг·К)") ("-") ("-"))
    (("Удельное количество теплоты") ("J/kg" "Дж/кг") ("MJ/kg" "МДж/кг" "kJ/kg" "кДж/кг") ("-") ("-"))
    (("Удельная теплота фазового превращения") ("J/kg" "Дж/кг") ("MJ/kg" "МДж/кг" "kJ/kg" "кДж/кг") ("-") ("-"))
    "Часть V Электричество и магнетизм"
    (("Электрический ток" "сила электрического тока") ("А" "A") ("kА" "кА" "mA" "мА" "μА" "мкА" "nA" "нА" "рА" "пА") ("-") ("-"))
    (("Электрический заряд" "количество электричества") ("С" "Кл") ("kC" "кКл" "μС" "мкКл" "nС" "нКл" "рС" "пКл") ("A⋅h" "А⋅ч") ("-"))
    (("Пространственная плотность электрического заряда") ("С/m3" "Кл/м3") ("C/mm3" "Кл/мм3" "МС/m3 " "МКл/м3" "С/сm3" "Кл/см3" "kC/m3" "кКл/м3" "mС/m3 " "мКл/м3" "μС/m3" "мкКл/м3") ("-") ("-"))
    (("Поверхностная плотность электрического заряда") ("С/m2" "Кл/м2") ("МС/m2" "МКл/м2" "С/mm2" "Кл/мм2" "С/сm2" "Кл/см2" "kC/m2" "кКл/м2" "mС/m2" "мКл/м2" "μС/m2" "мкКл/м2") ("-") ("-"))
    (("Напряженность электрического поля") ("V/m" "В/м") ("MV/m" "МВ/м" "kV/m" "кВ/м" "V/mm" "В/мм" "V/cm" "В/см" "mV/m" "мВ/м" "μV/m" "мкВ/м") ("-") ("-"))
    (("Электрическое напряжение" "электрический потенциал" "разность электрических потенциалов" "электродвижущая сила") ("V" "В") ("MV" "MB" "kV" "кВ" "mV" "мВ" "μV" "мкВ" "nV" "нВ") ("-") ("-"))
    (("Электрическое смещение") ("С/m2" "Кл/м2") ("С/сm2" "Кл/см2" "kC/cm2" "кКл/см2" "mС/m2" "мКл/м2" "μС/m2" "мкКл/м2") ("-") ("-"))
    (("Поток электрического смещения") ("С" "Кл") ("МС" "МКл" "kC" "кКл" "mС" "мКл") ("-") ("-"))
    (("Электрическая емкость") ("F" "Ф") ("mF" "мФ" "μF" "мкФ" "nF" "нФ" "pF" "пФ" "fF" "фФ" "аF" "аФ") ("-") ("-"))
    (("Диэлектрическая проницаемость" "электрическая постоянная") ("F/m" "Ф/м") ("pF/m" "пФ/м") ("-") ("-"))
    (("Поляризованность") ("С/m2" "Кл/м2") ("С/сm2" "Кл/см2" "kC/m2" "кКл/м2" "mС/m2" "мКл/м2" "μС/m2" "мкКл/м2") ("-") ("-"))
    (("Электрический момент диполя") ("С·m" "Кл·м") ("-") ("-") ("-"))
    (("Плотность электрического тока") ("А/m2" "А/м2") ("МА/m2" "МА/м2" "A/mm2" "А/мм2" "A/cm2" "А/см2" "kA/m2" "кА/м2") ("-") ("-"))
    (("Линейная плотность электрического тока") ("А/m" "А/м") ("kA/m" "кА/м" "А/mm" "А/мм" "А/сm" "А/cм") ("-") ("-"))
    (("Напряженность магнитного поля") ("A/m" "А/м") ("kA/m" "кА/м" "А/mm" "А/мм" "А/сm" "А/cм") ("-") ("-"))
    (("Магнитодвижущая сила" "разность магнитных потенциалов" "магнитный потенциал") ("А" "А") ("kA" "кА" "mА" "мА") ("-") ("-"))
    (("Магнитная индукция" "плотность магнитного потока") ("T" "Тл") ("mT" "мТл" "μT" "мкТл" "nT" "нТл") ("-") ("-"))
    (("Магнитный поток") ("Wb" "Вб") ("mWb" "мВб") ("-") ("-"))
    (("Магнитный векторный потенциал") ("T·m" "Тл·м") ("kT·m" "кТл·м") ("-") ("-"))
    (("Индуктивность" "взаимная индуктивность") ("Н" "Гн") ("kН" "кГн" "mH" "мГн" "μН" "мкГн" "nН" "нГн" "рН" "пГн") ("-") ("-"))
    (("Магнитная проницаемость" "магнитная постоянная") ("H/m" "Гн/м") ("μН/m" "мкГн/м" "nН/m" "нГн/м") ("-") ("-"))
    (("Магнитный момент") ("А·m2" "А·м2") ("-") ("-") ("-"))
    (("Намагниченность") ("А/m" "А/м") ("kA/m" "кА/м" "А/mm" "А/мм") ("-") ("-"))
    (("Магнитная поляризация") ("T" "Тл") ("mT" "мТл") ("-") ("-"))
    (("Электрическое сопротивление" "активное сопротивление" "модуль полного сопротивления" "реактивное сопротивление") ("Ω" "Ом") ("TΩ" "ТОм" "GΩ" "ГОм" "MΩ" "МОм" "kΩ" "кОм" "mΩ" "мОм" "μΩ" "мкОм" )("-") ("-"))
    (("Электрическая проводимость" "активная проводимость" "модуль полной проводимости") ("S" "См") ("kS" "кСм" "mS" "мСм" "μS" "мкСм" "nS" "нСм" "pS" "пСм") ("-") ("-"))
    (("Реактивная проводимость") ("S" "См") ("kS" "кСм" "mS" "мСм" "μS" "мкСм") ("-") ("-"))
    (("Разность фаз" "фазовый сдвиг" "угол сдвига фаз") ("rad" "рад") ("mrad" "мрад" "μrad" "мкрад") ("°") ("-"))
    (("Удельное электрическое сопротивление") ("Ω·m" "Ом·м") ("GΩ·m" "ГОм·м" "MΩ·m" "МОм·м" "kΩ·m" "кОм·м" "Ω·cm" "Ом·см" "mΩ·m" "мОм·м" "μΩ·m" "мкОм·м" "nΩ·m" "нОм·м") ("-") ("-"))
    (("Удельная электрическая проводимость") ("S/m" "См/м") ("MS/m" "МСм/м" "kS/m" "кСм/м") ("-") ("-"))
    (("Магнитное сопротивление") ("H-1" "Гн-1") ("-") ("-") ("-"))
    (("Магнитная проводимость") ("H" "Гн") ("-") ("-") ("-"))
    (("Активная мощность") ("W" "Вт") ("TW" "ТВт" "GW" "ГВт" "MW" "МВт" "kW" "кВт" "mW" "мВт" "μW" "мкВт" "nW" "нВт") ("V⋅A" "В⋅А" "var" "вар") ("-"))
    (("Энергия") ("J" "Дж") ("TJ" "ТДж" "GJ" "ГДж" "MJ" "МДж" "kJ" "кДж") ("eV" "эВ") ("kW⋅h" "кВт⋅ч"))
    "Часть VI Свет и связанные с ним электромагнитные излучения"
    (("Длина волны") ("m" "м") ("μm" "мкм" "nm" "нм" "pm" "пм") ("-") ("-"))
    (("Волновое число") ("m-1" "м-1") ("cm-1" "см-1") ("-") ("-"))
    (("Энергия излучения") ("J" "Дж") ("-") ("-") ("-"))
    (("Поток излучения" "мощность излучения") ("W" "Вт") ("-") ("-") ("-"))
    (("Сила излучения") ("W/sr" "Вт/ср") ("-") ("-") ("-"))
    (("Спектральная плотность силы излучения") ("W/(sr·m)" "Вт/(ср·м)") ("-") ("-") ("-"))
    (("Энергетическая яркость") ("W/(sr·m2)" "Вт/(ср·м2)") ("-") ("-") ("-"))
    (("Спектральная плотность энергетической яркости") ("W/(sr·m3)" "Вт/(ср·м3)") ("-") ("-") ("-"))
    (("Облученность") ("W/m2" "Вт/м2") ("-") ("-") ("-"))
    (("Спектральная плотность облученности") ("W/m3" "Вт/м3") ("-") ("-") ("-"))
    (("Энергетическая светимость") ("W/m2" "Вт/м2") ("-") ("-") ("-"))
    (("Сила света") ("cd" "кд") ("-") ("-") ("-"))
    (("Световой поток") ("lm" "лм") ("-") ("-") ("-"))
    (("Световая энергия") ("lm·s" "лм·с") ("-") ("lm·h" "лм·ч") ("-"))
    (("Яркость") ("cd/m2" "кд/м2") ("-") ("-") ("-"))
     (("Светимость") ("lm/m2" "лм/м2") ("-") ("-") ("-"))
    (("Освещенность") ("lx" "лк") ("-") ("-") ("-"))
    (("Световая экспозиция") ("lx·s" "лк·с") ("-") ("-") ("-"))
    (("Световая эффективность") ("lm/W" "лм/Вт") ("-") ("-") ("-"))
    "Часть VII Акустика"
    (("Период") ("s" "с") ("ms" "мс" "μs" "мкс") ("-") ("-"))
    (("Частота периодического процесса") ("Hz" "Гц") ("MHz" "МГц" "kHz" "кГц") ("-") ("-"))
    (("Длина волны") ("m" "м") ("mm" "мм") ("-") ("-"))
    (("Звуковое давление") ("Ра" "Па") ("mРа" "мПа" "μРа" "мкПа") ("-") ("-"))
    (("Скорость колебания частицы") ("m/s" "м/с") ("mm/s" "мм/с") ("-") ("-"))
    (("Объемная скорость") ("m3/s" "м3/с") ("-") ("-") ("-"))
    (("Скорость звука") ("m/s" "м/с") ("-") ("-") ("-"))
    (("Поток звуковой энергии" "звуковая мощность") ("W" "Вт") ("kW" "кВт" "mW" "мВт" "μW" "мкВт" "pV" "пВт") ("-") ("-"))
    (("Интенсивность звука") ("W/m2" "Вт/м2") ("mW/m2" "мВт/м2" "μW/m2" "мкВт/м2" "pW/m2" "пВт/м2") ("-") ("-"))
    (("Удельное акустическое сопротивление") ("Pa·s/m" "Па·с/м") ("-") ("-") ("-"))
    (("Акустическое сопротивление") ("Pa·s/m3" "Па·с/м3") ("-") ("-") ("-"))
    (("Механическое сопротивление") ("N·s/m" "Н·с/м") ("-") ("-") ("-"))
    (("Эквивалентная площадь поглощения поверхностью или предметом") ("m2" "м2") ("-") ("-") ("-"))
    (("Время реверберации") ("s" "с") ("-") ("-") ("-"))
    "Часть VIII Физическая химия и молекулярная физика"
    (("Количество вещества") ("mol" "моль") ("kmol" "кмоль" "mmol" "ммоль" "μmol" "мкмоль") ("-") ("-"))
    (("Молярная масса") ("kg/mol" "кг/моль") ("g/mol" "г/моль") ("-") ("-"))
    (("Молярный объем") ("m3/mol" "м3/моль") ("dm3/mol" "дм3/моль" "cm3/mol" "см3/моль") ("l/mol" "л/моль") ("-"))
    (("Молярная внутренняя энергия") ("J/mol" "Дж/моль") ("kJ/mol" "кДж/моль") ("-") ("-"))
    (("Молярная энтальпия") ("J/mol" "Дж/моль") ("kJ/mol" "кДж/моль") ("-") ("-"))
    (("Химический потенциал") ("J/mol" "Дж/моль") ("kJ/mol" "кДж/моль") ("-") ("-"))
    (("Молярная теплоемкость") ("J/(mol·K)" "Дж/(моль·К)") ("-") ("-") ("-"))
    (("Молярная энтропия") ("J/(mol·K)" "Дж/(моль·К)") ("-") ("-") ("-"))
    (("Молярная концентрация компонента") ("mol/m3" "моль/м3") ("mol/dm3" "моль/dм3" "kmol/m3" "моль/м3") ("mol/l" "моль/л") ("-"))
    (("Удельная адсорбция") ("mol/kg" "моль/кг") ("mmol/kg" "ммоль/кг") ("-") ("-"))
    (("Массовая концентрация компонента") ("kg/m3" "кг/м3") ("mg/m3" "мг/м3" "mg/dm3" "мг/дм3") ("mg/l" "мг/л") ("-"))
    "Часть IX Ионизирующие излучения"
    (("Поглощенная доза ионизирующего излучения" "керма") ("Gy" "Гр") ("TGy" "ТГр" "GGy" "ГГр" "MGy" "МГр" "kGy" "кГр" "mGy" "мГр" "μGy" "мкГр") ("-") ("-"))
    (("Активность нуклида в радиоактивном источнике" "активность радионуклида") ("Bq" "Бк") ("EBq" "ЭБк" "PBq" "ПБк" "TBq" "ТБк" "GBq" "ГБк" "MBq" "МБк" "kBq" "кБк") ("-") ("-"))
    (("Эквивалентная доза ионизирующего излучения" "эффективная доза ионизирующего излучения") ("Sv" "Зв") ("mSv" "мЗв") ("-") ("-"))
    )
  "Рекомендуемые к применению единицы Си.")
