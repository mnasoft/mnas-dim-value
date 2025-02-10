;;;; ./src/method/vd-convert.lisp

(in-package :mnas-dim-value/method)

(defun degrees-minutes-seconds-to-radians (dms-string)
  (labels
      ((parse-float (str)
         (if (string= str "") 0
             (mnas-string/parse:read-number str)))
       (parse-d-m-s (str)
         (cond
           ((string= "°" str)  1)
           ((string= "d" str)  1)
           ((string= "'" str)  1/60)
           ((string= "′" str)  1/60)
           ((string= "\"" str) 1/3600)
           ((string= "″" str)  1/3600)    
           (t 0))))
    (let ((regex
            "^([-+])?([0-9]*\\.?[0-9]*)([°d]?)([0-9]*\\.?[0-9]*)(['′]?)([0-9]*\\.?[0-9]*)([\"″]?)$"
            #+nil  "^([-+])?([0-9]*\\.?[0-9]*)([°d'′\"″]?)([0-9]*\\.?[0-9]*)([°d'′\"″]?)([0-9]*\\.?[0-9]*)([°d'′\"″]?)$"
            #+nil  "^([-+])?([0-9]*\\.?[0-9]*)([°d'\"]?)([0-9]*\\.?[0-9]*)([°d'\"]?)([0-9]*\\.?[0-9]*)([°d'\"]?)$"))
      (multiple-value-bind (match substrings)
          (cl-ppcre:scan-to-strings regex dms-string)
        (if match
            (let* ((a-sign (aref substrings 0))
                   (a-v1   (aref substrings 1))
                   (a-v2   (aref substrings 3))
                   (a-v3   (aref substrings 5))
                   (a-s1   (aref substrings 2))
                   (a-s2   (aref substrings 4))
                   (a-s3   (aref substrings 6))
                   (sign (cond
                           ((null a-sign) 1)
                           ((string= "+" a-sign)  1)
                           ((string= "-" a-sign) -1)
                           (t 1))))
              (loop :for v :in (list a-v1 a-v2 a-v3)
                    :as  s :in (list a-s1 a-s2 a-s3)
                    :summing (* (parse-float v) (parse-d-m-s s))
                      :into dms
                    :finally (return (* sign dms (/ pi 180)))))
            nil)))))

(defmethod vd-convert ((x string))
  (let ((dms (degrees-minutes-seconds-to-radians x)))
    (when dms (return-from vd-convert (vd~* dms "rad"))))
  (multiple-value-bind (val find) (gethash x mnas-dim-value/ht-en:*nm->value*)
    (if find
        val
        (progn
          (format t "~&Размерность ~S неизвестна: заменяю ~S -> ~S~%"
                  x x (vd 1.0))
          (vd 1.0)))))


(defmethod vd-convert ((x null))
  (progn
    (format t "~&Размерность ~S неизвестна: заменяю ~S -> ~S~%"
            x x (vd 0.0))
    (vd 0.0)))
