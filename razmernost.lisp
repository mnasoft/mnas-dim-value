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

;;;;(gethash "μ" *muti-prefix*)

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

(defparameter *si-main*
  '(("длина"	                     "L"           "метр"          "m"                          "м")
    ("масса"                         "M"           "килограмм"     "kg"                         "кг")
    ("время"                         "T"           "секунда"       "s"                          "с")
    ("сила электрического тока"      "I"           "ампер"	   "A"	                        "А")
    ("термодинамическая температура" "Θ"	   "кельвин"	   "K"                          "К")
    ("сила света"                    "J"           "кандела"	   "cd"                         "кд")
    ("количество вещества"           "N"           "моль"          "mol"                        "моль"))
  "Задает основные единицы измерения системы SI
Каждый подсписок состоит из следующих элементов:
1 - русское наименование типа единицы измерения
2 - буквенное обозначене типа единицы измерения
3 - русское наименование единицы измерения
4 - международное обозначение единицы измерения
5 - русское обозначение единицы измерения")

(defparameter *si-additional*
  '(("плоский угол"  "L/L"     "радиан"    "rad" "рад")
    ("телесный угол" "L^2/L^2" "стерадиан" "sr"  "ср"))
  "Задает дополнительные единицы измерения системы SI
Каждый подсписок состоит из следующих элементов:
1 - русское наименование типа единицы измерения
2 - буквенное обозначене типа единицы измерения
3 - русское наименование единицы измерения
4 - международное обозначение единицы измерения
5 - русское обозначение единицы измерения")


(defparameter *si-derive*
  '(("площадь"                           ""               "m^2"                        "м^2"       "m^2")
    ("объём"                             ""               "m^3"                        "м^3"       "m^3")
    ("удельный объём"                    ""               "m^3/kg"                     "м^3/кг"    "m^3/kg")
    ("плотность"                         ""               "kg/m^3"                     "кг/м^3"	   "kg/m^3")
    ("частота периодического процесса"   "герц"	          "Hz"                         "Гц"	   "1/s")
    ("скорость"                          ""               "m/s"                        "м/с"       "m/s")
    ("ускорение"                         ""               "m/s^2"                      "м/с^2"     "m/s^2")
    ("угловая скорость"                  ""               "rad/s"                      "рад/с"     "rad/s")
    ("угловое ускорение"                 ""               "rad/s^2"                    "рад/с^2"   "rad/s^2")
    ("сила (вес)"	                 "ньютон"	  "N"	                       "Н"	   "kg*m/s^2")
    ("давление" 	                 "паскаль"	  "Pa"	                       "Па"        "kg/(m*s^2)")
    ("механическое напряжение"           "паскаль"	  "Pa"	                       "Па"        "kg/(m*s^2)")
    ("импульс (количество движения)"     ""               "kg*m/s"                     "кг*м/с"    "kg*m/s")
    ("импульс силы"                      ""               "N*s"                        "Н*с"       "N*s")
    ("кинематическая вязкость"           ""               "m^2/s"                      "м^2/с"     "m^2/s")
    ("динамическая вязкость"             ""               "Pa*s"                       "Па*с"      "kg/(m*s)")
    ("работа"                            "джоуль"         "J"                          "Дж"        "kg*m^2/s^2")
    ("энергия"                           "джоуль"         "J"                          "Дж"        "kg*m^2/s^2")
    ("количество теплоты"                "джоуль"         "J"                          "Дж"        "kg*m^2/s^2")
    ("мощность"                          "ватт"           "W"                          "Вт"        "kg*m^2/s^3")
    ("момент силы"                       ""               "N*m"                        "Н*м"       "kg*m^2/s^2")
    ("момент инерции"                    ""               "kg*m^2"                     "кг*м^2"    "kg*m^2")
    ("удельная теплоемкость"             ""               "J/(kg*K)"                   "Дж/(кг*К)" "m^2/(s^2*K)")
    ("энторопия"                         ""               "J/K"                        "Дж/К"      "kg*m^2/(s^2*K)")
    ("теплопроводность"                  ""               "W/(m*K)"                    "Вт/(кг*К)" "kg*m/(s^3*K)")
    ("электрический заряд"               "кулон"          "C"                          "Кл"        "A*c")
    ("количество электричества"          "кулон"          "C"                          "Кл"        "A*c")
    ("электрическое напряжение"          "вольт"          "V"                          "В"         "kg*m^2/(A*s^3)")
    ("электродвижущая сила"              "вольт"          "V"                          "В"         "kg*m^2/(A*s^3)")
    ("напряженность электрического поля" ""               "V/m"                        "В/м"       "kg*m/(A*s^3)")
    ("электрическое сопротивление"	 "ом"	          "Ω"	                       "Ом"        "kg*m^2/(A^2*s^3)")
    ("электрическая проводимость"	 "сименс"	  "S"                          "См"        "kg^-1*m^-2*A^2*s^3")
    ("электрическая ёмкость"             "фарад"          "F"                          "Ф"         "kg^-1*m^-2*A^2*s^4")
    ("магнитный поток"	                 "вебер"          "Wb"                         "Вб"        "kg*m^2/(A*s^2)")
    ("индуктивность"                     "генри"          "H"                          "Гн"        "kg*m^2/(A^2*s^2)")
    ("магнитная индукция"                "тесла"          "T"                          "Тл"        "kg/(A*s^2)")
    ("напряжённость магнитного поля"     ""               "A/m"                        "А/м"       "A/m")
    ("магнитодвижушая сила"              "ампер"          "A"                          "А"         "A")
    ("сила излучения (интенсивность)"	 ""               "W/sr"                       "Вт/ср"     "kg*m^2/(s^3*sr)")
    ("волновое число"                    ""               "m^-1"                       "м^-1"      "m^-1")
    ("световой поток"                    "люмен"          "lm"                         "лм"        "cd*sr")
    ("яркость"                           ""               "cd/m^2"                     "кд/м^2"    "cd/m^2")
    ("освещенность"                      "люкс"           "lx"                         "лк"        "cd*sr/m^2")
    ("активность нуклида в радиовктивном источнике" "беккерель" "Bq"                   "Бк"        "1/s")
    ("поглощенная доза излучения, керма" "грей"           "Gy"                         "Гр"        "m^2/s^2")
    ("показатель поглощенной дозы"       "грей"           "Gy"                         "Гр"        "m^2/s^2")
    ("эквивалентная доза излучения"      "зиверст"        "Sv"                         "Зв"        "m^2/s^2"))
  "Задает производные единицы измерения системы SI
Каждый подсписок состоит из следующих элементов:
1 - русское наименование величины
2 - русское наименование единицы измерения
3 - международное буквенное обозначене типа единицы измерения
4 - русское буквенное обозначение единицы измерения
5 - размерность")


;;;;(mapcar #'(lambda(el) (string= (fourth el)(sixth el))) *si-derive*)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *op* '(("+" . 1) ("-" . 1) ("*" . 2) ("/" . 2) ("^" . 3)))

(defun foo-operatorp (op) (assoc op *op* :test #'equal))

(defun foo-open-parenthesisp(str) (equal str "("))

(defun foo-close-parenthesisp(str) (equal str ")"))

(defun foo-split(str)
  "Пример использования (foo-split *s*)
"
  (do 
   ((start 0)
    (sub nil)
    (len nil)
    (rez nil))
   ((equal len 0) (reverse rez))
    (setf sub
;;;;  	  (cl-ppcre:scan-to-strings "(\\()|(\\))|(\\*)|(\\/)|(\\^)|([A-Za-z]+)|([+-]*[0-9]+)" str :start start)
  	  (cl-ppcre:scan-to-strings "(\\()|(\\))|(\\*)|(\\/)|(\\^)|(\\-)|(\\+)|([A-Za-z]+)|([0-9]+)" str :start start)
	  len (length sub)
	  start (+ start len)
	  )
    (if (>= len 1) 
	(setf rez (cons sub rez)))))

(defun foo-lexem-tree (ll)
  "Пример использования
(foo-rev(foo-lexem-tree (foo-split *s*)))"
  (do ((i 0 (1+ i)) (len (length ll)) (tree nil) (st  nil) (obj nil))
      ((>= i len) tree)
    (setf obj (nth i ll))    
    (cond
      ((foo-open-parenthesisp obj) (push tree st) (setf tree nil))
      ((foo-close-parenthesisp obj) (setf tree (cons tree (pop st))))
      (t (push obj tree)))))

(defun foo-rev (l)
  "Выполняет глубокое реверсирование списков"
  (cond ((null l) nil)
        ((listp (car l))
	 (append (foo-rev (cdr l)) 
		 (list (foo-rev (car l)))))
        (t
	 (append (foo-rev (cdr l)) 
		 (list (car l))))))

(defun foo-is-digit (str)
  (let
      ((rez (cl-ppcre:scan-to-strings "(^[+-]*[0-9]+)$" str)))
    (if (and rez (> (length rez) 0) (= (length rez) (length str)))
	t
	nil)))

(defun foo-convert-str-to-atom(str)
  (cond
    ((foo-is-digit str) (parse-integer str))
    ((foo-operatorp str) (read-from-string str))
    ))


(defun foo-is-dimension(str)
  )

(defun foo-max-operand-level(ll)
  "Возвращает максимальный уровень операда, примененного в выражении"
  (do* ((op-level 0) (len (length ll)) (i 0 (1+ i)) (tmp 0) (i-num 0))
       ((>= i len) (values i-num op-level))
    (setf tmp (nth i ll))
    (if (and (foo-operatorp tmp)
	     (>= (cdr (foo-operatorp tmp)) op-level))
	(setf op-level (cdr (foo-operatorp tmp))
	      i-num i))))

(defun foo-operand-op-operand(ll n)
  (let
      ((operand-l (1- n))
       (operand-r (1+ n))
       (operation n)
       (len (length ll)))
    (cond
      ((and (< 0 n (1- len)))
       (append
	(subseq ll 0 operand-l)
	(list
	 (list
	  (nth n ll)
	  (nth operand-l ll)
	  (nth operand-r ll)))
	(subseq ll (1+ operand-r))))
      (t ll))))

(defun foo-parse-list(ll)
  (do ()
      ((= 1 (length ll)) (car ll))
    (setf ll (foo-operand-op-operand ll (foo-max-operand-level ll)))))

(defun foo-parse-list-recursive(ll)
  (let ((a (car ll) (car ll)))
   (cons
   ((null ll) nil))));;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (optimize (debug 3)))

(defparameter *ss* "25*kg^2+65*kg*g")

(defparameter *tt* "(kg^(-2))*(m*s^3)/(H^2*m^3)")

(defparameter *uu* "kg^2")

(defparameter *ll* (foo-rev (foo-lexem-tree(foo-split *tt*))))

(defparameter *oo* '(0 1 2 3 4 5 6))

(foo-parse-list *ll*)




