(defpackage :mnas-dim-value/tbl-en
  (:use #:cl
        #:mnas-dim-value/class
        #:mnas-dim-value/mk-class
        #:mnas-dim-value/tbl
        )
  (:export *table-1*
           *table-2*
           *table-4*
           *table-5*
           *table-6*
           *table-7*
           *table-8*
           )
  (:export *table-9*)
  (:export *nd-tables*)
  )

(in-package :mnas-dim-value/tbl-en)

(defun nd-check-table (table)
  (loop :for nd :in table
        :do (nd-check nd)))


(defparameter *table-2* nil)
(defparameter *table-4* nil)
(defparameter *table-5* nil)
(defparameter *table-6* nil)
(defparameter *table-8* nil)
(defparameter *table-9* nil)
(defparameter *nd-tables* nil)


(defparameter *table-7* 
  '((10  30 "quetta" "Q")
    (10  27 "ronna"  "R")
    (10  24 "yotta"  "Y")
    (10  21 "zetta"  "Z")
    (10  18 "exa"    "E")
    (10  15 "peta"   "P")
    (10  12 "tera"   "T")
    (10   9 "giga"   "G")
    (10   6 "mega"   "M")
    (10   3 "kilo"   "k")
    (10   2 "hecto"  "h")
    (10   1 "deca"   "da")
    (10   0 ""       "")
    (10  -1 "deci"   "d")
    (10  -2 "centi"  "c")
    (10  -3 "milli"  "m")
    (10  -6 "micro"  "μ")
    (10  -9 "nano"   "n")
    (10 -12 "pico"   "p")
    (10 -15 "femto"  "f")
    (10 -18 "atto"   "a")
    (10 -21 "zepto"  "z")
    (10 -24 "yocto"  "y")
    (10 -27 "ronto"  "r")
    (10 -30 "quecto" "q")
    )
  "*table-7* содержит множителные приставки;
Каждый подсписок содержит описание одной множительной приставки в
следующем формате:
1 - base        - основание степени;
2 - power       - показатель степени;
3 - unit-name   - наименование множителя международное;
4 - unit-symbol - обозначение множителя международное.")

(defparameter *table-1* 
  `(("Defining constant"                    "Symbol"   "Numerical value" "Unit")
    ("hyperfine transition frequency of Cs" "Δv_{Cs}"  9192631770        ,(vd 1 :s -1))
    ("speed of light in vacuum"             "c"        299792458         ,(vd 1 :m 1  :s -1))
    ("Planck constant"                      "h"        6.62607015d-34    ,(vd 1 :m 2 :kg 1 :s -1))
    ("elementary charge"                    "e"        1.602176634d-19   ,(vd 1 :s 1 :A 1))
    ("Boltzmann constant"                   "k"        1.380649d-23      ,(vd 1 :m 2 :kg 1 :s -2 :K -1))
    ("Avogadro constant"                    "N_{A}"    6.02214076d+23    ,(vd 1 :mol -1))
    ("luminous efficacy"                    "Kcd"      683               ,(vd 1 :cd 1 :sr 1 :m -2 :kg -1 :s 3))
    )
  "Table 1. The seven defining constants of the SI and the seven
corresponding units they define")

(block table-2
  (nd-clear)
  (nd "unitless"             "U"   "unitless" "ul"  (vd 1             )  nil)
  (nd "length"               "L"   "meter"    "m"   (vd 1       :m   1) )
  (nd "mass"                 "M"   "gram"     "g"   (vd 1/1000  :kg  1) )
  (nd "time"                 "T"   "second"   "s"   (vd 1       :s   1) )
  (nd "electric current"     "I"   "ampere"   "A"   (vd 1       :A   1) )
  (nd "temperature"          "Θ"   "kelvin"   "K"   (vd 1       :K   1) )
  (nd "amount of substance"  "N"   "mole"     "mol" (vd 1       :mol 1) )
  (nd "luminous intensity"   "J"   "candela"  "cd"  (vd 1       :cd  1) )
  (setf *table-2* (nd-get))
  (setf (documentation  '*table-2* 'variable) 
        "Table 2. SI base units")
  (mnas-dim-value/tbl:nd-check-table *table-2*))

(block table-4
  (nd-clear)
  (nd "plane angle"
      "L/L" "radian"    "rad" (vd 1 :rad 1) )            ; "rad=m/m"
  (nd "solid angle"
      "L^2/L^2" "steradian" "sr"  (vd 1 :sr 1))              ; "sr=m^2/m^2"
  (nd "frequency"
      "1/T" "hertz"     "Hz"  (vd 1 :s -1) )             ; "Hz=s^-1"
  (nd "force"
      nil "newton"      "N"   (vd 1 :m 1 :kg 1 :s -2) )  ; "N=kg*m*s^-2"
  (nd "pressure, stress"
      nil "pascal"     "Pa"  (vd 1 :m -1 :kg 1 :s -2) ) ; "Pa=kg*m^−1*s^−2=N/m^2"
  (nd "energy, work, quantity of heat" nil "joule"     "J"   (vd 1 :m 2 :kg 1 :s -2) )  ; "J=kg*m^2*s^-2=N*m"
  (nd "power, radiant flux"            nil "watt"      "W"   (vd 1 :m 2 :kg 1 :s -3) )  ; "W=kg*m^2*s^−3=J/s"
  (nd "electric charge, quantity of electricity"  nil "coulomb" "C" (vd 1 :s 1 :A 1) )  ; "C=A*s"
  (nd "electric potential difference, electromotive force"
      nil      "volt" "V" (vd 1 :m 2 :kg 1 :s -3 :A -1) )   ; "V=kg*m^2*s^−3*A^−1=W/A"
  (nd "capacitance" nil "farad"  "F" (vd 1 :m -2 :kg -1 :s 4 :A 2)  )  ; "F=kg−1 m−2 s4 A2=C/V"
  (nd "electric resistance" nil "ohm" "Ω"  (vd 1 :kg 1 :m 2  :s -3 :A -2) ) ; "Ω = kg m2 s−3 A−2=V/A"
  (nd "electric conductance" nil "siemens" "S" (vd 1 :m -2 :kg -1 :s 3 :A 2) )   ; "S = kg−1 m−2 s3 A2=A/V"
  (nd "magnetic flux" nil "weber" "Wb" (vd 1 :m 2 :kg 1 :s -2 :A -1) )   ; "Wb = kg m2 s−2 A−1=V s"
  (nd "magnetic flux density" nil "tesla" "T" (vd 1 :kg 1 :s -2 :A -1) )        ; "T = kg s−2 A−1 Wb/m2"
  (nd "inductance" nil "henry" "H" (vd 1 :m 2 :kg 1 :s -2 :A -2) )   ; "H = kg m2 s−2 A−2=Wb/A"
  (nd "Celsius temperature" nil "degree Celsius" "°С" (vd 1 :K 1) nil)                  ; "°С=K"
  (nd "luminous flux"  nil "lumen" "lm" (vd 1 :cd 1 :sr 1) )              ; "lm = cd sr = cd sr"
  (nd "illuminance" nil "lux" "lx" (vd 1 :cd 1 :sr 1 :m -2) )        ; "lx = cd sr m−2=lm/m2"
  (nd "activity referred to a radionuclide" nil "becquerel" "Bq" (vd 1 :s -1) ) ; "Bq = s−1"
  (nd "absorbed dose, kerma" nil "gray" "Gy" (vd 1 :m 2 :s -2) )   ; "Gy = m2 s−2 J/kg"
  (nd "dose equivalent" nil "sievert" "Sv"   (vd 1 :m 2 :s -2) )   ; "Sv = m2 s−2=J/kg"
  (nd "catalytic activity" nil "katal" "кат" (vd 1 :s -1 :mol 1) ) ; "kat = mol s−1"
  (setf *table-4* (nd-get))
  (setf (documentation  '*table-4* 'variable)
        "Table 4. The 22 SI units with special names and symbols")
  (mnas-dim-value/tbl:nd-check-table *table-4*))

(block table-5
  (nd-clear)
  (nd "area" nil "square meter" "m^2" (vd 1 :m 2) )                     ; "m2"
  (nd "volume" nil "cubic meter" "m^3" (vd 1 :m 3)  )                    ; "m3"
  (nd "speed, velocity" nil "meter per second" "m/s" (vd 1 :m 1 :s -1) )               ; "m s−1"
  (nd "acceleration"   nil "meter per second in a square" "m/s^2" (vd 1 :m 1 :s -2) )               ; "m s−2"
  (nd "wavenumber" nil "one per meter" "1/m" (vd 1 :m -1) nil)                 ; "m−1"
  (nd "density, mass density" nil "kilogram per cubic meter" "kg/m^3" (vd 1 :kg 1 :m -3) )              ; "kg m−3"
  (nd "surface density" nil "kilogram per square meter" "kg/m^2" (vd 1 :kg 1 :m -2) )              ; "kg m−2"
  (nd "specific volume" nil "cubic meter per kilogram" "m^3/kg"  (vd 1 :m 3 :kg -1) )              ; "m3 kg−1"
  (nd "current density" nil "ampere per square meter" "A/m^2"       (vd 1 :A 1 :m -2) )               ; "A m−2"
  (nd "magnetic field strength" nil "ampere per meter" "A/m"       (vd 1 :A 1 :m -1) )               ; "A m−1"
  (nd "amount of substance concentration"  nil "mole per cubic meter" "mol/m^3"       (vd 1 :mol 1 :m -3) )             ; "mol m−3"
  (nd "mass concentration"  nil "kilogram per cubic meter" "kg/m^3"      (vd 1 :kg -1 :m 3) )              ; "kg m−3"
  (nd "luminance"  nil "candela per square meter"  "cd/m^2"       (vd 1 :cd 1 :m -2) )  ; "cd m−2"
;;;; Дополнительные примеры величин начало
  (nd "momentum"  nil "kilogram meter per second" "kg*m/s"       (vd 1 :kg 1 :m 1 :s -1))
  (nd "kinematic viscosity" nil "square meter per second" "m^2/s" (vd 1 :m 2 :s -1))
  (nd "moment of inertia"  nil "kilogram meter squared" "kg*m^2" (vd 1 :kg 1 :m 2))
  (nd "mass flow rate" nil "kilogram per second" "kg/s"       (vd 1 :kg 1 :s -1))
  (nd "volumetric flow" nil "cubic meter per second" "m^3/s"       (vd 1 :m 3 :s -1))
  (nd "flow rate" nil       "mole per second" "mol/s"        (vd 1 :mol 1 :s -1))
  (nd "molar weight"  nil   "kilogram per mole" "kg/mol"       (vd 1 :kg 1 :mol -1))
;;;; Дополнительные примеры величин конец
  (setf *table-5* (nd-get))
  (setf (documentation '*table-5* 'variable)
        "Table 5. Examples of coherent derived units in the SI expressed in
terms of base units)")
  (mnas-dim-value/tbl:nd-check-table *table-5*))

(block table-6
  (nd-clear)
  (nd "dynamic viscosity" nil "pascal second" "Pa*s"  (vd 1 :kg 1 :m -1 :s -1) )        ; "Pa s=kg m-1 s-1"
  (nd "moment of force" nil "newton metre"  "N*m"     (vd 1  :kg 1 :m 2 :s -2) )        ; "N m=kg m2 s-2"
  (nd "surface tension"  nil "newton per metre" "N/m" (vd 1 :kg 1 :s -2) )              ; "N m-1=kg s-2"
  (nd "angular velocity, angular frequency"  nil "radian per second" "rad/s" (vd 1 :rad 1 :s -1) )             ; "rad s−1=s−1"
  (nd "angular acceleration" nil "radian per second squared" "rad/s^2"       (vd 1 :rad 1 :s -2) )             ; "rad s−2=s−2"
  (nd "heat flux density, irradiance"  nil "watt per square metre" "W/m^2"   (vd 1 :kg 1 :s -3) )              ; "W m−2=kg s−3"
  (nd "heat capacity, entropy"  nil "joule per kelvin" "J/K"                 (vd 1 :kg 1 :m 2 :s -2 :K -1) )   ; "J K−1=kg m2 s−2 K−1"
  (nd "specific heat capacity, specific entropy" nil "joule per kilogram kelvin" "J/(kg*K)" (vd 1 :m 2 :s -2 :K -1) )         ; "J K−1 kg−1=m2 s−2 K−1"
  (nd "specific energy" nil "joule per kilogram" "J/kg"       (vd 1 :m 2 :s -2))                ; "J kg−1=m2 s−2"
  (nd "thermal conductivity" nil "watt per metre kelvin" "W/(m*K)" (vd 1 :kg 1 :m 1 :s -3 :K -1 ))   ; "W m−1 K−1=kg m s−3 K−1"
  (nd "energy density" nil "joule per cubic metre" "J/m^3" (vd 1  :kg 1 :m -1 :s -2))        ; "J m−3=kg m−1 s−2"
  (nd "electric field strength" nil "volt per metre" "V/m" (vd 1 :kg 1 :m 1 :s -3 :A -1))    ; "V m−1=kg m s−3 A−1"
  (nd "electric charge density" nil "coulomb per cubic metre" "C/m^3"  (vd 1 :A 1 :s 1 :m -3))           ; "C m−3=A s m−3"
  (nd "surface charge density" nil  "coulomb per square metre" "C/m^2" (vd 1 :A 1 :s 1 :m -2))           ; "C m−2=A s m−2"
  (nd "electric flux density, electric displacement" nil "coulomb per square metre" "C/m^2"       (vd 1 :A 1 :s 1 :m -2))           ; "C m−2=A s m−2"
  (nd "permittivity" nil "farad per metre" "F/m"  (vd 1 :kg -1 :m -3 :s 4 :A 2))    ; "F m−1=kg−1 m−3 s4 A2"
  (nd "permeability"  nil "henry per metre" "H/m" (vd 1 :kg 1 :m 1 :s -2 :A -2))    ; "H m−1=kg m s−2 A−2"
  (nd "molar energy" nil "joule per mole" "J/mol" (vd 1 :kg 1 :m 2 :s -2 :mol -1))  ;  "J mol−1=kg m2 s−2 mol−1"
  (nd "molar entropy, molar heat capacity" nil "joule per mole kelvin" "J/(mol*K)" (vd 1 :kg 1 :m 2 :s -2 :mol -1 :K -1)) ; "J K−1 mol−1=kg m2 s−2 mol−1 K−1"
  (nd "exposure (x- and γ- rays)"  nil "coulomb per kilogram" "C/kg" (vd 1 :A 1 :s 1 :kg -1))          ; "C kg−1=A s kg−1"
  (nd "absorbed dose rate" nil "gray per second"  "Gy/s"       (vd 1 :m 2 :s -3))                ; "Gy s−1=m2 s−3"
  (nd "radiant intensity"  nil "watt per steradian" "W/sr"       (vd 1 :kg 1 :m 2 :s -3 :sr -1))   ;  "W sr−1=kg m2 s−3"
  (nd "radiance"  nil       "watt per square metre steradian"  "W/(sr*m^2)" (vd 1 :kg 1 :s -3 :sr -1))        ; "W sr−1 m−2=kg s−3"
  (nd "catalytic activity concentration" nil "katal per cubic metre" "kat/m^3" (vd 1 :mol 1 :s -1 :m -3))  ; "kat m−3=mol s−1 m−3"
;;;; Дополнительные примеры начало  
  (nd "force impulse"  nil "newton second"  "N*s" (vd 1 :kg 1 :m 1 :s -1))         ; "N s=kg*m*s^-1"
;;;; Дополнительные примеры конец
  (setf *table-6* (nd-get))
  (setf (documentation  '*table-6* 'variable)
        "Table 6. Examples of SI coherent derived units whose names and symbols
include SI coherent derived units with special names and symbols")
  (mnas-dim-value/tbl:nd-check-table *table-6*))

(block table-8
  (nd-clear)
  (nd "time" nil "minute" "min" (vd 60 :s 1) nil)
  (nd "time" nil "hour"   "h"   (vd 3600 :s 1) nil)
  (nd "time" nil "day"    "d"   (vd 86400 :s 1) nil)
;;;;
  (nd "length" nil "astronomical unit" "au" (vd 149597870700d0 :m 1) nil)
;;;;
  (nd "plane angle" nil "degree" "°"        (vd (/ pi 180) :rad 1) nil)
  (nd "plane angle" nil "minute" "'"        (vd (/ pi 180 60) :rad 1) nil)
  (nd "plane angle" nil "second" "\""       (vd (/ pi 180 60 60) :rad 1) nil)
;;;;
  (nd "area" nil "hectare" "ha"             (vd (* 100 100) :m 2)   nil)
;;;;
  (nd "volume" nil "liter" "l"              (vd 1/1000 :m 3) '((-3 -3) (0 3)))
;;;;
  (nd "mass" nil "ton" "t"                  (vd 1000 :kg 1) '((0 24)))
  (nd "mass" nil "dalton"  "Da"             (vd 1.6605390666050d-27 :kg 1))
;;;;
  (nd "energy" nil "electronvolt"  "eV"     (vd 1.602176634d-19 :m 2 :kg 1 :s -2) nil)
;;;; logarithmic neper (h) Np see text
;;;; ratio quantities bel (h) B
;;;; decibel (h) dB
;;;; Дополнительные величины начало
  (nd "plane angle"   nil "gon"  "gon"       (vd  (/ pi 200) :rad 1) nil)
  (nd "length"        nil "light year"  "ly" (vd 9.460730472580800d15 :m 1) nil)
  (nd "length"        nil "parsec" "pc"      (vd 3.0856776d16  :m 1) nil)
  (nd "optical force" nil "dioptre" "dpt"    (vd 1 :m -1)  nil)
  (nd "area"          nil "are" "a"          (vd 100 :m 2)   nil)
  (nd "energy"  nil  "kilowatt-hour"  "kW*h" (vd (* 36/10 1000 1000) :m 2 :kg 1 :s -2) nil) 
  (nd "full power"  nil "volt ampere"  "V*A" (vd 1 :m 2 :kg 1 :s -3) nil)
  (nd "reactive power"  nil "var" "var"           (vd 1 :m 2 :kg 1 :s -3) nil)
  (nd "electric charge"  nil "ampere hour" "A*h" (vd (* 36/10 1000) :s 1 :A 1) nil)
;;;; Дополнительные величины конец
  (setf *table-8* (nd-get))
  (setf (documentation  '*nd-stable-8-non-si-units-accepted-for-use-with-the-si-units* 'variable)
        "Внесистемные единицы, допустимые к применению наравне с единицами СИ")
  (mnas-dim-value/tbl:nd-check-table *table-8*))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(block table-9-others
  (nd-clear)
;;; "mass" "масса"  
  (nd "mass" nil "quintal" "q" (vd 100 :kg 1) nil)
;;; "angle of rotation" "угол поворота"
  (nd "angle of rotation" nil "turn" "tr" (vd (* 2 pi) :rad 1) nil)  
;;; "length" "длина"  
  (nd "length" nil "nautical mile" "nmi" (vd 1852 :m 1)   nil)
  (nd "length" nil "angstrom" "Å" (vd 1d-10 :m 1) nil)
  (nd "length"  nil "micron" "μ" (vd (/ 1 1000 1000) :m 1) nil)
;;; "force" "сила"
  (nd "force" nil "gram-force" "gf"     (vd (* 1/1000 9.80665d0) :m 1 :s -2 :kg 1) '((-24 3)))
  (nd "force" nil "ton-force" "tf"      (vd (* 1000 9.80665d0) :m 1 :s -2 :kg 1) '((0 24)))
  (nd "force" nil "pound" "p"           (vd (* 1/1000 9.80665d0) :m 1 :s -2 :kg 1) '((0 0) (3 9)))
;;; "pressure" "давление"
  (nd "pressure"  nil "meter of water column" "m_H2O" (vd (* 1000 9.80665d0) :m -1 :kg 1 :s -2) '((-3 0)))
  (nd "pressure"  nil "meter of mercury" "m_Hg"       (vd  133322d0 :m -1 :kg 1 :s -2) '((-3 0)))
  (nd "pressure"  nil "torr"  "Torr"                  (vd 133.322d0 :m -1 :kg 1 :s -2) '((-6 9)))
  (nd "pressure"  nil "bar"   "bar"                   (vd (* 100 000) :m -1 :kg 1 :s -2) '((-3 -3) (3 9)))
;;; "power" "мощность" 
  (nd "power"  nil "horsepower" "hp"                  (vd (* 75 9.80665d0) :kg 1 :m 2 :s -3 ) nil)
;;; "rotational speed" "частота вращения"
  (nd "rotational speed" nil "cycle per second"  "cps" (vd (* pi 2)  :rad 1  :s -1)   nil)
  (nd "rotational speed" nil "revolution per minute" "rpm" (vd (* pi 2 1/60) :rad 1 :s -1)   nil) 
;;;
  (setf *table-9* (nd-get))
  (setf (documentation  '*table-9* 'variable)
        "Соотношение некоторых внесистемных единиц с единицами СИ")
  (mnas-dim-value/tbl:nd-check-table *table-9*))

(defparameter *nd-tables*
  (append 
   *table-2*
   *table-4*
   *table-5*
   *table-6*
   *table-8*
   *table-9*))

(mnas-dim-value/tbl:nd-check-table *nd-tables*)
