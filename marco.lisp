;;;; marco.lisp

(in-package #:mnas-dim-value)

(annot:enable-annot-syntax)

@annot.doc:doc
"@b(Описание:) Функция op-exclude выполняет исключение первого вхождения 
бинарной операции op из списка lst, заменяя ее на функцию func-op.

 @b(Пример использования:)
@begin[lang=lisp](code)
 (op-exclude :+ '(1 :+ 3 :+ 4 :- 6 :+ 7) 'vd+) => ((VD+ 1 3) :+ 4 :- 6 :+ 7)
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
 (m-op-exclude :+ '(1 :+ 3 :+ 4 :- 6 :+ 7) 'vd+) => ((VD+ (VD+ 1 3) 4) :- (VD+ 6 7)) 
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
 (operatorp :^) => T 
 (operatorp :*) => T 
 (operatorp :/) => T 
 (operatorp :+) => T 
 (operatorp :-) => T 
@end(code)
"
(defun operatorp (x) (if (member x '(:^ :* :/ :+ :- )) t nil))

@annot.doc:doc
" @b(Описание:) Функция not-operatorp выполняет проверку, 
не является-ли символ бинарным оператором.
"
(defun not-operatorp (x)
  (not (operatorp x)))

@annot.doc:doc
"@b(Описание:) функция add-asterix добавляет звезды между символами, 
которые не являются операторами.

 @b(Пример использования:)
@begin[lang=lisp](code)
 (add-asterix '(1 (|kg| |m| ^ 2) 2 :+ 3)) => (1 :* (|kg| |m| ^ 2) :* 2 :+ 3)
 (add-asterix '(mdv:|kg| mdv:|m| :^ 2))   => (|kg| :* |m| :^ 2)
@end(code)

 @b(Примечание:) функция не является рекурсивной.
"
(defun add-asterix (lst)
  (let ((rez nil) (prev nil) (curr nil))
    (dolist (i lst)
      (setf curr (not-operatorp i))
      (when (and curr prev) (push :* rez))
      (push i rez)
      (setf prev curr))
    (reverse rez)))

@annot.doc:doc
"@b(Описание:) функция vd-quantity выполняет последовательное исключение 
символов бинарных операций из списка data с их заменой вызовами соответствующих функций.

 @b(Пример использования:)
@begin[lang=lisp](code)
 (in-pacakge :cl-user)
 (mdv::vd-quantity '(10 :^ 2 mdv:|Pa| / mdv:|s|)) => (VD* (VD* (VD* (VD-EXPT 10 2) |Pa|) /) |s|)
 (mdv::vd-quantity '(\"V\" \"A\" )) => (VD* \"V\" \"A\") 
@end(code)
"
(defun vd-quantity (data)
  (let ((rez (add-asterix data)))
    (setf rez (m-op-exclude :^ rez 'vd-expt))
    (setf rez (m-op-exclude :/ rez 'vd/))
    (setf rez (m-op-exclude :* rez 'vd*))
    (setf rez (m-op-exclude :- rez 'vd-))
    (setf rez (m-op-exclude :+ rez 'vd+))
    (first rez)))

@annot.doc:doc
"@b(Описание:) функция rec-quantity  выполняет рекурсивное
исключение символов бинарных операций из списка data  
с их заменой вызовами соответствующих функций.

 @b(Пример использования:)
@begin[lang=lisp](code)
 (rec-quantity '(101325 \"N\" :/ (10 \"cm\" :^ 2 :+ 505.5 \"mm\" :^ 2)))
 =>
 (VD* 101325
     (VD/ \"N\" (VD+ (VD* 10 (VD-EXPT \"cm\" 2)) (VD* 505.5 (VD-EXPT \"mm\" 2)))))
 =>
 6.7303224e7 [Pa]
@end(code)
"
(defun rec-quantity (data)
  (let ((rez nil))
    (dolist (i data)
      (if (atom i)
	  (push i rez)
	  (push (rec-quantity i) rez)))
    (vd-quantity (reverse rez))))

@export
@annot.doc:doc
"@b(Описание:) макрос quantity выполняет разбор и вычисление выражения, 
имеющего в своем составе размерные величины.

 Выражение может состоять из списков, чисел, размерных величин, символов бинарных операций.

 Символы бинарных операции имеют старшинство и перечислены в списке в порядке убывания старшинства:
@begin(list)
 @item(:^ - возведение числа или размерности в степень;)
 @item(:/ - деление;)
 @item(:* - умножение;)
 @item(:- - вычитание;)
 @item(:+ - сложение.)
@end(list)

 Перечень допустимых разменых величин определен в хеш-таблице *nm-vl*.
Для ее просмотра используйте следующий код:
@begin[lang=lisp](code)
 (mdv::print-hash-table mdv::*nm-vl*)
@end(code)
 Размерные величины можно задавать в виде:
@begin(list)
 @item(строки: например: \"Pa\", \"s\", \"mol\"; )
 @item(в виде констант пакета mnas-dim-value (mdv): например: mdv:|Pa|, mnas-dim-value:|s|, mdv:|mol|.)
@end(list)

 @b(Пример использования:)
@begin[lang=lisp](code)
 (progn
  (list
  (list (mdv:quantity 220 \"V\" 15 \"A\"))
  (list (mdv:quantity 220 :* \"V\" :* 15 :* \"A\"))
  (list (mdv:quantity 20 \"kgf\" :/ \"cm\" :^ 2))))
@end(code)

@begin[lang=lisp](code)
 (progn
   (list
     (mdv:quantity 101325 \"Pa\" :+ ( 2.0 mdv:|*g*| :/ (1 :* \"cm\" :* \"cm\")))
     (mdv:quantity 101325 \"Pa\" :+ ( 2.0 mdv:|*g*| :/ (1 :* \"cm\" :^ 2))) 
  ))
@end(code)

@begin[lang=lisp](code)
 (progn
  (in-package :mnas-dim-value)
  (list (mnas-dim-value:quantity 1 \"d\" :+ 15 \"h\" 20 :+ \"min\" :+ 30.5 \"s\")))
@end(code)
"
(defmacro quantity (x &rest y )
  (eval (list 'rec-quantity  (list 'quote (append (list x) y)))))
