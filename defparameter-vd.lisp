;;;; defparameter-vd.lisp

(in-package #:mnas-dim-value)

(annot:enable-annot-syntax)

@export
(defparameter |m|   (vd 1 :m   1)           "метр")

@export
(defparameter |kg|  (vd 1 :kg  1)           "килограмм")

@export
(defparameter |s|   (vd 1 :s   1)           "секунда")

@export
(defparameter |A|   (vd 1 :A   1)           "ампер")

@export
(defparameter |K|   (vd 1 :K   1)           "кельвин")

@export
(defparameter |cd|  (vd 1 :cd  1)           "кандела")

@export
(defparameter |mol| (vd 1 :mol 1)           "моль")

@export
(defparameter |rad| (vd 1 :rad 1)           "радиан")

@export
(defparameter |sr|  (vd 1 :sr  1)           "стерадиан")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

@export
(defparameter |Hz|  (vd/ |s|)               "герц")

@export
(defparameter |N|   (vd 1 :m 1 :kg 1 :s -2) "ньютон")

@export
(defparameter |Pa|  (vd/ |N| |m| |m|)       "паскаль")

@export
(defparameter |J|   (vd* |N| |m|)           "джоуль")

@export
(defparameter |W|   (vd/ |J| |s|)           "ватт")

@export
(defparameter |C|   (vd* |A| |s|)           "кулон")

@export
(defparameter |V|   (vd/ |W| |A|)           "вольт")

@export
(defparameter |F|   (vd/ |C| |V|)           "фарад")

@export
(defparameter |Ω|   (vd/ |V| |A|)           "ом")

@export
(defparameter |S|   (vd/ |Ω|)               "сименс")

@export
(defparameter |Wb|  (vd* |V| |s|)           "вебер")

@export
(defparameter |Τ|   (vd/ |Wb| |m| |m|)      "тесла. Примечание для избежания конфликта вместо t используется τ верхнего регистра.")

@export
(defparameter |H|   (vd/ |Wb| |A|)          "генри")

@export
(defparameter |lm|  (vd* |cd| |sr|)         "люмен")

@export
(defparameter |lx|  (vd/ |lm| |m| |m|)      "люкс")

@export
(defparameter |Bq|  |Hz|                    "беккерель")

@export
(defparameter |Gy|  (vd/ |J| |kg|)          "грэй")

@export
(defparameter |Sv|  (vd/ |J| |kg|)          "зиверст")

@export
(defparameter |kat| (vd/ |mol| |s|)         "катал")


