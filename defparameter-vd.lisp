;;;; defparameter-vd.lisp

(in-package #:mnas-dim-value)

(progn
  (defparameter |m|   (vd 1 :m   1)           "метр")
  (defparameter |kg|  (vd 1 :kg  1)           "килограмм")
  (defparameter |s|   (vd 1 :s   1)           "секунда")
  (defparameter |A|   (vd 1 :A   1)           "ампер")
  (defparameter |K|   (vd 1 :K   1)           "кельвин") 
  (defparameter |cd|  (vd 1 :cd  1)           "кандела")
  (defparameter |mol| (vd 1 :mol 1)           "моль")
  (defparameter |rad| (vd 1 :rad 1)           "радиан")
  (defparameter |sr|  (vd 1 :sr  1)           "стерадиан"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(progn
  (defparameter |Hz|  (vd/ |s|)               "герц")
  (defparameter |N|   (vd 1 :m 1 :kg 1 :s -2) "ньютон")
  (defparameter |Pa|  (vd/ |N| |m| |m|)       "паскаль")
  (defparameter |J|   (vd* |N| |m|)           "джоуль")
  (defparameter |W|   (vd/ |J| |s|)           "ватт")
  (defparameter |C|   (vd* |A| |s|)           "кулон")
  (defparameter |V|   (vd/ |W| |A|)           "вольт")
  (defparameter |F|   (vd/ |C| |V|)           "фарад")
  (defparameter |Ω|   (vd/ |V| |A|)           "ом")
  (defparameter |S|   (vd/ |Ω|)               "сименс")

  (defparameter |Wb|  (vd* |V| |s|)           "вебер")
  (defparameter |Τ|   (vd/ |Wb| |m| |m|)      "тесла") ;;;; τ
  (defparameter |H|   (vd/ |Wb| |A|)          "генри")

  (defparameter |lm|  (vd* |cd| |sr|)         "люмен")
  (defparameter |lx|  (vd/ |lm| |m| |m|)      "люкс")
  (defparameter |Bq|  |Hz|                    "беккерель")
  (defparameter |Gy|  (vd/ |J| |kg|)          "грэй")
  (defparameter |Sv|  (vd/ |J| |kg|)          "зиверст")
  (defparameter |kat| (vd/ |mol| |s|)         "катал")
  )
