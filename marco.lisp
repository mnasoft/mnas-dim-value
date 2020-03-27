;;;; marco.lisp

(in-package #:mnas-dim-value)

(annot:enable-annot-syntax)

@export
(defparameter ^ '^ "Обозначение бинарной опреации возведения в степень")

@annot.doc:doc
"@b(Описание:) Функция op-exclude выполняет исключение первого вхождения 
бинарной операции op из списка lst, заменяя ее на функцию func-op.

@b(Пример использования:)
@begin[lang=lisp](code)
 (op-exclude '+ '(1 + 3 + 4 - 6 + 7) 'vd+) => ((VD+ 1 3) + 4 - 6 + 7)
@end(code)
"
(defun op-exclude (op lst func-op)
  
  (let ((op-poz (position op lst)))   
    (when op-poz
      (setf lst (append
		 (subseq lst 0 (- op-poz 1) )
		 (list (list func-op (nth (- op-poz 1) lst ) (nth (+ op-poz 1) lst)))
		 (subseq lst (+ op-poz 2)))))
    lst))

@annot.doc:doc
"@b(Описание:) Функция m-op-exclude выполняет множественое исключение 
бинарной операции op из списка lst, заменяя ее на функцию func-op.

@b(Пример использования:)
@begin[lang=lisp](code)
 (m-op-exclude '+ '(1 + 3 + 4 - 6 + 7) 'vd+) => ((VD+ (VD+ 1 3) 4) - (VD+ 6 7))
@end(code)
"
(defun m-op-exclude (op lst func-op)
  (do ((p-data lst data)
     (data (op-exclude op lst func-op) (op-exclude op data func-op)))
      ((equal p-data data) data)))

@annot.doc:doc
"@b(Описание:) Функция operatorp выполняет проверку, 
является-ли символ бинарным оператором.

@b(Пример использования:)
@begin[lang=lisp](code)
 (operatorp '*) => T 
 (operatorp  *) => nil 
 (operatorp '/) => T 
 (operatorp '+) => T 
 (operatorp '-) => T 
 (operatorp '^) => T 
 (operatorp  ^) => T 
@end(code)
"
(defun operatorp (x)
  (if (member x '(^ * / + - )) t nil))

@annot.doc:doc
"@b(Описание:) Функция not-operatorp выполняет проверку, 
не является-ли символ бинарным оператором.
"
(defun not-operatorp (x)
  (not (operatorp x)))

@annot.doc:doc
"@b(Описание:) функция add-asterix добавляет звезды между символами, 
которые не являются операторами.

 @b(Пример использования:)
@begin[lang=lisp](code)
 (add-asterix '(1 (kg m ^ 2) 2 + 3))
 (add-asterix '(kg m ^ 2))
@end(code)

 @b(Примечание:) функция не является рекурсивной.
"
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

@export
@annot.doc:doc
"@b(Описание:) макрос quantity

 @b(Пример использования:)

@begin[lang=lisp](code)
 (progn
  (list
  (list (mdv:quantity 220 \"V\" 15 \"A\"))
  (list (mdv:quantity 220 * \"V\" * 15 * \"A\"))
  (list (mdv:quantity 20 \"kg\f\" / \"cm\" / \"cm\"))))
@end(code)

@begin[lang=lisp](code)
 (progn
   (list
     (mdv:quantity 101325 \"Pa\" + ( 2.0 mdv:|*g*| / (1 * \"cm\" * \"cm\")))
   ;;(mdv:quantity 101325 \"Pa\" + ( 2.0 mdv:|*g*| / (1 * \"cm\" ^ 2))) 
  ))
@end(code)

@begin[lang=lisp](code)
 (progn
  (in-package :mnas-dim-value)
  (list (mnas-dim-value:quantity 1 \"d\" + 15 \"h\" 20 + \"min\" + 30.5 \"s\")))
@end(code)
"
(defmacro quantity (x &rest y )
  (eval (list 'rec-quantity  (list 'quote (append (list x) y)))))
