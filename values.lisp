;;;; values.lisp

(in-package #:mnas-dim-value)

(defun dimensionp (str)
  (multiple-value-bind (val find) (gethash str *mult-nm-vl*)
    (if find val nil)))

(dimensionp "pV")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *op* '(("+" . 1) ("-" . 1) ("*" . 2) ("/" . 2) ("^" . 3)))

(defun foo-operatorp (op) (assoc op *op* :test #'equal))

(defun foo-open-parenthesisp(str) (equal str "("))

(defun foo-close-parenthesisp(str) (equal str ")"))

(defun foo-split(str)
  "Пример использования (foo-split *s*)
"
  (do 
   ((start 0)
    (sub nil)
    (len nil)
    (rez nil))
   ((equal len 0) (reverse rez))
    (setf sub
;;;;  	  (cl-ppcre:scan-to-strings "(\\()|(\\))|(\\*)|(\\/)|(\\^)|([A-Za-z]+)|([+-]*[0-9]+)" str :start start)
  	  (cl-ppcre:scan-to-strings "(\\()|(\\))|(\\*)|(\\/)|(\\^)|(\\-)|(\\+)|([A-Za-z]+)|([0-9]+)" str :start start)
	  len (length sub)
	  start (+ start len)
	  )
    (if (>= len 1) 
	(setf rez (cons sub rez)))))

(defun foo-lexem-tree (ll)
  "Пример использования
;;;; (foo-rev(foo-lexem-tree (foo-split *s*)))"
  (do ((i 0 (1+ i)) (len (length ll)) (tree nil) (st  nil) (obj nil))
      ((>= i len) tree)
    (setf obj (nth i ll))    
    (cond
      ((foo-open-parenthesisp obj) (push tree st) (setf tree nil))
      ((foo-close-parenthesisp obj) (setf tree (cons tree (pop st))))
      (t (push obj tree)))))

(defun foo-rev (l)
  "Выполняет глубокое реверсирование списков"
  (cond ((null l) nil)
        ((listp (car l))
	 (append (foo-rev (cdr l)) 
		 (list (foo-rev (car l)))))
        (t
	 (append (foo-rev (cdr l)) 
		 (list (car l))))))

(defun foo-is-digit (str)
  (let
      ((rez (cl-ppcre:scan-to-strings "(^[+-]*[0-9]+)$" str)))
    (if (and rez (> (length rez) 0) (= (length rez) (length str)))
	t
	nil)))

(defun foo-convert-str-to-atom(str)
  (cond
    ((foo-is-digit str) (parse-integer str))
    ((foo-operatorp str) (read-from-string str))
    ))

(defun foo-max-operand-level(ll)
  "Возвращает максимальный уровень операда, примененного в выражении"
  (do* ((op-level 0) (len (length ll)) (i 0 (1+ i)) (tmp 0) (i-num 0))
       ((>= i len) (values i-num op-level))
    (setf tmp (nth i ll))
    (if (and (foo-operatorp tmp)
	     (>= (cdr (foo-operatorp tmp)) op-level))
	(setf op-level (cdr (foo-operatorp tmp))
	      i-num i))))

(defun foo-operand-op-operand(ll n)
  (let
      ((operand-l (1- n))
       (operand-r (1+ n))
       (operation n)
       (len (length ll)))
    (cond
      ((and (< 0 n (1- len)))
       (append
	(subseq ll 0 operand-l)
	(list
	 (list
	  (nth n ll)
	  (nth operand-l ll)
	  (nth operand-r ll)))
	(subseq ll (1+ operand-r))))
      (t ll))))

(defun foo-parse-list(ll)
  (do ()
      ((= 1 (length ll)) (car ll))
    (setf ll (foo-operand-op-operand ll (foo-max-operand-level ll)))))

(defun foo-parse-list-recursive(ll)
  (let ((a (car ll)))
   (cons
    ((null ll) nil))
   ))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;






;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (optimize (debug 3)))

(defparameter *ss* "25 kg^2+65kg*g")

(foo-split *ss*)

(defparameter *tt* "(kg^(-2))*(m*s^3)/(H^2*m^3)")

(defparameter *uu* "kg^2")

(defparameter *ll* (foo-rev (foo-lexem-tree(foo-split *tt*))))

(defparameter *oo* '(0 1 2 3 4 5 6))

(foo-parse-list *ll*)
