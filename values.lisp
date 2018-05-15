;;;; values.lisp

(in-package #:mnas-dim-value)

(defun quantity-from-string-not-eval (str)
  (let ((o-b "(") (c-b ")") (dig "°") (s-q "'") (d-q "\"") (sps " ") (s-^ "^") (s-/ "/") (s-* "*") (s-- "-") (s-+ "+"))
    (labels ((string-add-brackets&quotes (str)
	       (concatenate 'string o-b sps str sps c-b))
	     (add-space-around-sym (sym str)
	       (mnas-string:string-replace-all
		str sym (concatenate 'string sps sym sps )))
	     (add-space (str)
	       (let ((rez str))
		 (mapcar #'(lambda (el) (setf rez (add-space-around-sym el rez)))
			 (list o-b c-b s-^ s-/ s-* s-- s-+
			       d-q  s-q dig))
		 (str:words rez)))
	     (baz (str)
	       (let ((rez
		      (str:unwords 
		       (mapcar
			#'(lambda (el)
			    (cond
			      ((or (string= o-b el)
				   (string= c-b el)
				   (string= s-^ el)
				   (string= s-/ el)
				   (string= s-* el)
				   (string= s-- el)
				   (string= s-+ el))
			       el)
			      ((or (string= dig el)
				   (string= s-q el))
			       (str:concat d-q el d-q))
			      ((string= d-q el)
			       (str:concat d-q "\\" el d-q))
			      ((mnas-string:read-from-string-number el nil) el)
			      (t (str:concat d-q el d-q))))
			(add-space str)))))
;;;;		 (format t "~A~%" rez)
		 rez)))
      (read-from-string
       (string-add-brackets&quotes
	(str:concat "quantity "(baz str)))))))

(defun quantity-from-string (str)
  (eval (quantity-from-string-not-eval str)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun prompt-read-line ()
  (format t "Введите выражение:")
  (read-line))

(defun print-stack (stack)
  (qi-sample)
  (print-stack-clean)
  (let ((i 0))
    (mapc #'(lambda (el) (format t "X~A=~A~%" (incf i) el))
	  (reverse stack))))

(defun print-stack-clean ()
  (loop for i from 0 to 10 do
       (format t "~%")))

(defun quantity-inetractive ()
  "Позволяет выполнить запуск интерактивного калькулятора"
  (do* ((rez-lst  nil)
	(rez      nil)
	(str-lst  nil (push str str-lst))
	(str     (progn (print-stack rez-lst) (prompt-read-line))
		 (progn (print-stack rez-lst) (prompt-read-line))))
       ((string= str "exit")
	(values (reverse str-lst)
		(reverse rez-lst)))
    (cond
      ((string= ""      (string-trim " " str)))
      ((string= "help"  (string-trim " " str)) (help))
      ((string= "clear" (string-trim " " str)) (setf rez-lst  nil))
      ((string= "<>"   (string-trim " " str))
       (let ((x1 (pop rez-lst))
	     (x2 (pop rez-lst)))
	 (push x1 rez-lst)
	 (push x2 rez-lst)))
      ((string= "*" str) (push (vd* (pop rez-lst) (pop rez-lst)) rez-lst))
      ((string= "/" str) (push
			  (let ((x1 (pop rez-lst))
				(x2 (pop rez-lst)))
			    (vd/ x2 x1))
			  rez-lst))
      ((string= "+" str) (push (vd+ (pop rez-lst) (pop rez-lst)) rez-lst))
      ((string= "-" str) (push (vd- (pop rez-lst) (pop rez-lst)) rez-lst))
      (t
       (format t "~A~%" (quantity-from-string-not-eval str))
       (setf rez (quantity-from-string str))
       (push rez rez-lst)
       (format t "~A~%" rez)))))

(export 'quantity-inetractive)

(defun qi ()
  "Позволяет выполнить запуск интерактивного калькулятора короткой командой"
  (quantity-inetractive))

(export	'qi)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun qi-sample ()
  (mapc
   (lambda (el)
     (format t "~A~%" el))
   '(
     "
Команды:
==============================
exit  - выход из калькулятора;
clear - очистка стека;
flip  - 
<>    - 
============================="
     "(25 kgf + 783.565 gf) / cm^2"
     "70*kgf/(70*cm^2)"
     "(0-1)*(55 m^2+45 mm^2)kgf/cm^2"
     "(1/kg^2)*(m*s^3)/(N^2*m^3)"
     "3600 r/h"
     "2°+10'+55.4\"")))
