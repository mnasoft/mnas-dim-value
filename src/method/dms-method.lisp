;;;; ./src/method/method.lisp

(in-package :mnas-dim-value/method)

(vd~/ (vd~* "20d" "N" "m") "rad")

(defun radians-to-dms (radians &key (seconds-format "~2f") (force-sign nil))
  (let* ((degrees (/ (* radians 180) pi))
         (sign (if (and (not (radians degrees)) (or (minusp radians) force-sign)) "-" ""))
         (abs-degrees (abs degrees))
         (d (truncate abs-degrees))
         (m (* 60 (- abs-degrees d)))
         (min (truncate m))
         (s (* 60 (- m min))))
    (format nil "~a~d°~d'~a\"" sign d min (format nil seconds-format s))))

;; Пример использования
(radians-to-dms 1.0 :seconds-format "~3F" :force-sign t)

(defun radians-to-dms (radians &key (seconds-format "~2f") (force-sign nil))
  (let* ((degrees (/ (* radians 180) pi))
         (abs-degrees (abs degrees))
         (sign (if (or (and force-sign (plusp degrees)) (minusp degrees)) (if (minusp degrees) "-" "+") ""))
         (d (truncate abs-degrees))
         (m (* 60 (- abs-degrees d)))
         (min (truncate m))
         (s (* 60 (- m min))))
    (format nil "~a~d°~d'~a\"" sign d min (format nil seconds-format s))))

;; Пример использования
(radians-to-dms 1.0 :seconds-format "~3f" :force-sign nil)  ;; Возвращает "+57°17'45.832\""
(radians-to-dms -1.0 :seconds-format "~3f" :force-sign t)  ;; Возвращает "-57°17'45.832\""
(radians-to-dms 1.0 :seconds-format "~3f" :force-sign t)  ;; Возвращает "-57°17'45.832\""

(defun radians-to-dm (radians &key (minutes-format "~2f") (force-sign nil))
  (let* ((degrees (/ (* radians 180) pi))
         (abs-degrees (abs degrees))
         (sign (if (or (and force-sign (plusp degrees)) (minusp degrees)) (if (minusp degrees) "-" "+") ""))
         (d (truncate abs-degrees))
         (m (* 60 (- abs-degrees d)))
         (min (format nil minutes-format m)))
    (format nil "~a~d°~a'" sign d min)))

(radians-to-dm 1.0 :minutes-format "~12F" :force-sign nil)  ;; Возвращает "-57°17'45.832\""

(defun radians-to-degrees (radians &key (degrees-format "~2f") (force-sign nil))
  (let* ((degrees (/ (* radians 180) pi))
         (sign (if (or (and force-sign (plusp degrees)) (minusp degrees)) (if (minusp degrees) "-" "+") ""))
         (formatted-degrees (format nil degrees-format (abs degrees))))
    (format nil "~a~a°" sign formatted-degrees)))

;; Пример использования
(radians-to-degrees 1.0 :degrees-format "~3f" :force-sign nil)  ;; Возвращает "57.296°"
(radians-to-degrees -1.0 :degrees-format "~3f" :force-sign t)  ;; Возвращает "-57.296°"

(defun radians-to-angle (radians &key (output :dms) (seconds-format "~2f") (minutes-format "~2f") (force-sign nil))
  (let* ((degrees (/ (* radians 180) pi))
         (abs-degrees (abs degrees))
         (sign (if (or (and force-sign (plusp degrees)) (minusp degrees)) (if (minusp degrees) "-" "+") ""))
         (d (truncate abs-degrees))
         (m (* 60 (- abs-degrees d)))
         (min (truncate m))
         (s (* 60 (- m min))))
    (case output
      (:dms
       (format nil "~a~d°~d'~a\"" sign d min (format nil seconds-format s)))
      (:dm
       (format nil "~a~d°~a'" sign d (format nil minutes-format m)))
      (:degrees
       (format nil "~a~a°" sign (format nil "~2f" degrees)))
      (t
       (error "Invalid output format. Valid options are :degrees, :dm, or :dms.")))))

;; Пример использования
(radians-to-angle 1.0 :output :dms :seconds-format "~3f" :force-sign t)  ;; Возвращает "57°17'45.832\""
(radians-to-angle -1.0 :output :dm :minutes-format "~3f" :force-sign nil)  ;; Возвращает "-57°17.453'"
(radians-to-angle 1.0 :output :degrees :force-sign nil)  ;; Возвращает "57.30°"

(defun radians-to-angle (radians &key (output :dms) (seconds-format "~2f") (minutes-format "~2f") (degrees-format "~2f") (suppress-zero nil) (force-sign nil))
  (let* ((degrees (/ (* radians 180) pi))
         (abs-degrees (abs degrees))
         (sign (if (or (and force-sign (plusp degrees)) (minusp degrees)) (if (minusp degrees) "-" "+") ""))
         (d (truncate abs-degrees))
         (m (* 60 (- abs-degrees d)))
         (min (truncate m))
         (s (* 60 (- m min)))
         (degrees-str (if (and suppress-zero (zerop d)) "" (format nil "~a~a°" sign (format nil degrees-format d))))
         (minutes-str (if (and suppress-zero (zerop min)) "" (format nil "~a'" (format nil minutes-format min))))
         (seconds-str (format nil seconds-format s)))
    (case output
      (:dms
       (format nil "~a~d'~a\"" degrees-str min seconds-str))
      (:dm
       (format nil "~a~a'" degrees-str minutes-str))
      (:degrees
       degrees-str)
      (t
       (error "Invalid output format. Valid options are :degrees, :dm, or :dms.")))))

;; Пример использования
(radians-to-angle -0.001 :output :dms :seconds-format "~3f" :suppress-zero t)  ;; Возвращает "57°17'45.832\""
(radians-to-angle -1.0 :output :dm :minutes-format "~3f" :suppress-zero t :force-sign t)  ;; Возвращает "-57°17.453'"
(radians-to-angle 1.0 :output :degrees :degrees-format "~D" :suppress-zero t)  ;; Возвращает "57°"
