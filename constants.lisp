;;;; constants.lisp

(defparameter *g*     9.8065d0   "Ускорение свободного падения, м/с^2")
(defparameter *Gr*    6.672d-11  "Гравитационная постоянная, м^3/кг*с^2")
(defparameter *C-0*   273.15d0   "Ноль шкалы температур Цельсия, К")
(defparameter *V-0*   22.41d0    "Молекулярный объем идеального газа при нормальных условиях, м^3/кмоль")
(defparameter *R*     8.314d0    "Универсальная газовая постоянная, Дж/моль*К")
(defparameter *Na*    6.022d23   "Число Авогадро, 1")
(defparameter *No*    2.7d25     "Число Лошмидта, 1")
(defparameter *k*     1.38d-23   "Постоянная Больцмана, Дж/К")
(defparameter *a-e-m* 1.660d-27  "Постоянная Больцмана, Дж/К")
(defparameter *m-e*   9.1095d-31 "Масса покоя электрона, кг")
(defparameter *e*     1.6022d-19 "Масса покоя электрона, Кл")
(defparameter *F*     9.6500d4   "Число Фарадея, 1")
(defparameter *ε-0*   8.8542d-12 "Электрическая постоянная, Ф/м")
(defparameter *μ-0*   12.5664d-7 "Магнитная постоянная, Гн/м")
(defparameter *h*     6.626d-34  "Постоянная Планка, Дж*с")
(defparameter *с*     2.997925d8 "Скорость света в вакууме, м/с")


;;;;cl-ppcre
(cl-ppcre:split "(\\()|(\\))|(\\*)|(/)|\\*" "(kg*m^3)/s^3" )


(let* 
    ((str   "(kg*m^3)/(s^3)")
     (spl_/ (cl-ppcre:split "(/)" str)))
  (cond
    ((= (length spl_/) 0)
     (values spl_/ 0 str ))
    ((= (length spl_/) 1)
     (values spl_/ 1 str ))
    ((= (length spl_/) 2)
     (values spl_/ 2 str )
     (let ((chisl (cl-ppcre:split "(\\*)" (first  spl_/) :omit-unmatched-p t))
	   (znam  (cl-ppcre:split "(\\*)" (second spl_/) :omit-unmatched-p t)))
       (values chisl znam)))))

(cl-ppcre:do-matches-as-strings "\\." "(kg*m^3)" )


(spl_/ (cl-ppcre:split "(/)" str)))

(defun foo-split/ (str)
  (cl-ppcre:split "/" str))

(defun foo-split-bracket (str)
  (cl-ppcre:split "(/\\()|(\\()|(\\))" str))

(defparameter *str* "kg*m^2/(A*s^3)")

(defparameter *str* "(A*s^3)")

(foo-split/ *str*)

(foo-split-bracket *str*)

(read-sequence  *str*)
