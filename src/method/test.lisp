(in-package :mnas-dim-value/method)

(set-env :force "SI" *variable-set*)
(set-env :adaptive "SI" *variable-set*)
(vd-convert "m")

(vd~/ (vd~* "20d0′0″" "N" :m) :rad)
(vd~/ (vd~* "20d" "N" "m") :rad :kg)

(vd-convert "45d0'0\"")

(setf *angle* :rot)
(setf *a-units* 4)


(vd~* (* 60 60 24 (* 1.253111111d0 365.25)) "s")

(set-env :s "TIME"  *variable-set*)
(set-env :ms "TIME"  *variable-set*)
(set-env :hms "TIME" *variable-set*)
(set-env :dhms "TIME" *variable-set*)
(set-env :s "TIME" *variable-set*)
(set-env :m "TIME" *variable-set*)
(set-env :h "TIME" *variable-set*)
(set-env :mon "TIME" *variable-set*)
(set-env :year "TIME" *variable-set*)

             
(setf *units* 8)

(get-env "LANGUAGE" *variable-set*)
(get-env "ANGLE"    *variable-set*)


(descr-env "SI"    *variable-set*)
(set-env :dm "ANGLE" *variable-set*)
(vd-convert "30°20′22.5″")

(set-env :dm "ANGLE" *variable-set*)
(set-env 8 "UNITS" *variable-set*)

(vd~/ "J" "kg") (vd~* "Sv")

(vd~sin "18°")

(quantity-name (vd~/ "N" "m" "m"))

;; (unit-name (vd~/ "J" "kg"))
