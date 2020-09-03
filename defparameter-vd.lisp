;;;; defparameter-vd.lisp

(in-package #:mnas-dim-value)

(export '|m|   )
(defparameter |m|   (vd 1 :m   1)           "метр")

(export '|kg|  )
(defparameter |kg|  (vd 1 :kg  1)           "килограмм")

(export '|s|   )
(defparameter |s|   (vd 1 :s   1)           "секунда")

(export '|A|   )
(defparameter |A|   (vd 1 :A   1)           "ампер")

(export '|K|   )
(defparameter |K|   (vd 1 :K   1)           "кельвин")

(export '|cd|  )
(defparameter |cd|  (vd 1 :cd  1)           "кандела")

(export '|mol| )
(defparameter |mol| (vd 1 :mol 1)           "моль")

(export '|rad| )
(defparameter |rad| (vd 1 :rad 1)           "радиан")

(export '|sr|  )
(defparameter |sr|  (vd 1 :sr  1)           "стерадиан")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(export '|Hz|  )
(defparameter |Hz|  (vd/ |s|)               "герц")

(export '|N|   )
(defparameter |N|   (vd 1 :m 1 :kg 1 :s -2) "ньютон")

(export '|Pa|  )
(defparameter |Pa|  (vd/ |N| |m| |m|)       "паскаль")

(export '|J|   )
(defparameter |J|   (vd* |N| |m|)           "джоуль")

(export '|W|   )
(defparameter |W|   (vd/ |J| |s|)           "ватт")

(export '|C|   )
(defparameter |C|   (vd* |A| |s|)           "кулон")

(export '|V|   )
(defparameter |V|   (vd/ |W| |A|)           "вольт")

(export '|F|   )
(defparameter |F|   (vd/ |C| |V|)           "фарад")

(export '|Ω|   )
(defparameter |Ω|   (vd/ |V| |A|)           "ом")

(export '|S|   )
(defparameter |S|   (vd/ |Ω|)               "сименс")

(export '|Wb|  )
(defparameter |Wb|  (vd* |V| |s|)           "вебер")

(export '|Τ|   )
(defparameter |Τ|   (vd/ |Wb| |m| |m|)      "тесла. Примечание для избежания конфликта вместо t используется τ верхнего регистра.")

(export '|H|   )
(defparameter |H|   (vd/ |Wb| |A|)          "генри")

(export '|lm|  )
(defparameter |lm|  (vd* |cd| |sr|)         "люмен")

(export '|lx|  )
(defparameter |lx|  (vd/ |lm| |m| |m|)      "люкс")

(export '|Bq|)
(defparameter |Bq|  |Hz|                    "беккерель")

(export '|Gy|  )
(defparameter |Gy|  (vd/ |J| |kg|)          "грэй")

(export '|Sv|  )
(defparameter |Sv|  (vd/ |J| |kg|)          "зиверст")

(export '|kat| )
(defparameter |kat| (vd/ |mol| |s|)         "катал")
