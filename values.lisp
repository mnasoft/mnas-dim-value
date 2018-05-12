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

(defun quantity-inetractive ()
  "Позволяет выполнить запуск интерактивного калькулятора"
  (do* ((str-lst  nil (push str str-lst))
	(str     (prompt-read-line) (prompt-read-line))
	(rez      nil)
	(rez-lst  nil))
       ((string= str "exit")
	(values (reverse str-lst)
		(reverse rez-lst)))
    (cond
      ((string= ""      (string-trim " " str)))
      ((string= "help"  (string-trim " " str)) (help))
      ((string= "*" str) (push (vd* (pop rez-lst) (pop rez-lst)) rez-lst))
      ((string= "/" str) (push (vd/ (pop rez-lst) (pop rez-lst)) rez-lst))
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
