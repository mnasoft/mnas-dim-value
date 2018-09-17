;;;; marco.lisp

(in-package #:mnas-dim-value)

(defun op-exclude (op lst func-op)
  "Выполняет исключение первого вхождения бинарной операции op 
из списка lst, заменяя ее на функцию func-op.
Пример использования:
;;;; (op-exclude '+ '(1 + 3 + 4 - 6 + 7) 'vd+) => ((VD+ 1 3) + 4 - 6 + 7)
"
  (let ((op-poz (position op lst)))   
    (when op-poz
      (setf lst (append
		 (subseq lst 0 (- op-poz 1) )
		 (list (list func-op (nth (- op-poz 1) lst ) (nth (+ op-poz 1) lst)))
		 (subseq lst (+ op-poz 2)))))
    lst))

(defun m-op-exclude (op lst func-op)
  "Выполняет исключение бинарной операции op 
из списка lst, заменяя ее на функцию func-op.
Пример использования:
;;;; (m-op-exclude '+ '(1 + 3 + 4 - 6 + 7) 'vd+) => ((VD+ (VD+ 1 3) 4) - (VD+ 6 7))
"
  (do ((p-data lst data)
     (data (op-exclude op lst func-op) (op-exclude op data func-op)))
      ((equal p-data data) data)))

(defun operatorp (x)
  "Выполняет проверку, является-ли символ бинарным оператором"
  (if (member x '(^ * / + - )) t nil))

(defun not-operatorp (x)
  "Выполняет проверку, не является-ли символ бинарным оператором"
  (null (operatorp x)))

(defun add-asterix (lst)
  (let ((rez nil)
	(prev nil)
	(curr nil))
    (dolist (i lst)
      (setf curr (not-operatorp i))
      (cond
	((and curr prev) (push '* rez)))
      (push i rez)
      (setf prev curr))
    (reverse rez)))

(defun vd-quantity (data)
  (let ((rez (add-asterix data)))
    (setf rez (m-op-exclude '^ rez 'vd-expt))
    (setf rez (m-op-exclude '/ rez 'vd/))
    (setf rez (m-op-exclude '* rez 'vd*))
    (setf rez (m-op-exclude '- rez 'vd-))
    (setf rez (m-op-exclude '+ rez 'vd+))
    (first rez)))

(defun rec-quantity (data)
  (let ((rez nil))
    (dolist (i data)
      (if (atom i)
	  (push i rez)
	  (push (rec-quantity i) rez)))
    (vd-quantity (reverse rez))))

(defmacro quantity (x &rest y )
  (eval (list 'rec-quantity  (list 'quote (append (list x) y)))))
