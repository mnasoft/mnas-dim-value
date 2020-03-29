;;;; values.lisp

(in-package #:mnas-dim-value)

(annot:enable-annot-syntax)

(defun quantity-from-string-not-eval (str)
  (let* ((o-b "(") (c-b ")") (dig "°") (s-q "'") (d-q "\"") (sps " ") (s-^ "^") (s-/ "/") (s-* "*") (s-- "-") (s-+ "+")
	 (o-lst (list o-b c-b s-^ s-/ s-* s-- s-+))
	 (sp-list (list o-b c-b s-^ s-/ s-* s-- s-+ d-q s-q dig)))
    (labels ((string-add-brackets&quotes (str)
	       (concatenate 'string o-b sps str sps c-b))
	     (add-space-around-sym (sym str)
	       (mnas-string:string-replace-all
		str sym (concatenate 'string sps sym sps )))
	     (add-space (str)
	       (let ((rez str))
		 (mapcar
		  #'(lambda (el) (setf rez (add-space-around-sym el rez)))
		  sp-list)
		 (str:words rez)))
	     (replase-5 (str)
	       (mnas-string:replace-all
		(mnas-string:replace-all
		 (mnas-string:replace-all
		  (mnas-string:replace-all
		   (mnas-string:replace-all
		    str
		    " + " " :+ ")
		   " - " " :- ")
		  " * " " :* ")
		 " / " " :/ ")
		" ^ " " :^ "))
	     (baz (str)
	       (let ((rez (str:unwords 
			   (mapcar
			    #'(lambda (el)
				(cond
				  ((member el o-lst :test #'equal) el)
				  ((or (string= dig el)
				       (string= s-q el))
				   (str:concat d-q el d-q))
				  ((string= d-q el)
				   (str:concat d-q "\\" el d-q))
				  ((mnas-string:read-from-string-number el nil) el)
				  (t (str:concat d-q el d-q))))
			    (add-space str)))))
		 (format t "~S~%" rez)
		 rez)))
      (read-from-string
       (string-add-brackets&quotes
	(replase-5 
	 (str:concat "mnas-dim-value:quantity "(baz str))))))))

;;(str:unwords '("1" "234" "235"))

@export
(defun quantity-from-string (str)
  (eval (quantity-from-string-not-eval str)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

@export
(defun prompt-read-line ()
  (format t "Введите выражение:")
  (force-output t)
  (read-line))

(defun qi-sample ()
  (mapc
   (lambda (el)
     (format t "~A~%" el))
   '(
     "Команды:
==============================
exit  - выход из калькулятора;
help  - отображает дополнительную справку;
clear - очистка стека;
flip  - меняет местами регистры X1 и Χ2
<>    - меняет местами регистры X1 и Χ2
============================="
     "Примеры исппользования"
     "(25 kgf + 783.565 gf) / cm^2"
     "70*kgf/(70*cm^2)"
     "(0-1)*(55 m^2+45 mm^2)kgf/cm^2"
     "(1/kg^2)*(m*s^3)/(N^2*m^3)"
     "3600 r/h"
     "2°+10'+55.4\"")))

(defun print-stack (stack)
  (qi-sample)
  (print-stack-clean)
  (let ((i 0))
    (mapc #'(lambda (el) (format t "X~A=~A~%" (incf i) el))
	  (reverse stack))))

(defun print-stack-clean ()
  (loop for i from 0 to 10 do
       (format t "~%")))

(defun constants-help ()
  (mapc
   #'(lambda (el)
       (format t "~8A = ~A~%" el (documentation el 'VARIABLE)))
   '(*g* *Gn* *C-0* *V-0* *R-0* *Na* *No* *k* *a-e-m* *m-e* *e* *F* *h* *c* *μ-0* *ε-0*)))

@export
@annot.doc:doc
"@b(Описание:) функция запускает интерактивный калькулятор.

 @b(Пример использования:)
@begin[lang=lisp](code)
 (quantity-interactive)
@end(code)
"
(defun quantity-interactive ()
  (do* ((rez-lst  nil)
	(rez      nil)
	(str-lst  nil (push str str-lst))
	(str     (progn (print-stack rez-lst) (string-trim " " (prompt-read-line)))
		 (progn (print-stack rez-lst) (string-trim " " (prompt-read-line)))))
       ((string= str "exit")
	(values (reverse str-lst)
		(reverse rez-lst)))
    (cond
      ((string= ""      str) )
      ((string= "help"  str) (help))
      ((string= "constants"  str) (constants-help))
      ((string= "clear" str) (setf rez-lst  nil))
      ((or (string= "<>"    str) (string= "flip"    str) )
       (let ((x1 (pop rez-lst))
	     (x2 (pop rez-lst)))
	 (push x1 rez-lst)
	 (push x2 rez-lst)))
      ((string= "pop"  str) (pop rez-lst) )
      ((string= "push" str) (push (car rez-lst) rez-lst))
      ((string= "^" str) (push
			  (let ((x1 (pop rez-lst))
				(x2 (pop rez-lst)))
			    (vd-expt x2 x1))
			  rez-lst))
      ((string= "*" str) (push (vd* (pop rez-lst) (pop rez-lst)) rez-lst))
      ((string= "/" str) (push
			  (let ((x1 (pop rez-lst))
				(x2 (pop rez-lst)))
			    (vd/ x2 x1))
			  rez-lst))
      ((string= "+" str) (push (vd+ (pop rez-lst) (pop rez-lst)) rez-lst))
      ((string= "-" str) (push
			  (let ((x1 (pop rez-lst))
				(x2 (pop rez-lst)))
			    (vd- x2 x1))
			  rez-lst))
      (t
       (format t "~A~%" (quantity-from-string-not-eval str))
       (setf rez (quantity-from-string str))
       (push rez rez-lst)
       (format t "~A~%" rez)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

@export
@annot.doc:doc
"Запускает интерактивный калькулятор короткой командой"
(defun qi () (quantity-interactive))

