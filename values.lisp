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
		 (format t "~A~%" rez)
		 rez)))
      (read-from-string
       (string-add-brackets&quotes
	(str:concat "quantity "(baz str)))))))

(defun quantity-from-string (str)
  (eval (quantity-from-string-not-eval str)))
