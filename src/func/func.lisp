;;;; mnas-dim-value.lisp

(defpackage :mnas-dim-value/func
  (:use #:cl)
  (:export print-hash-table)
  (:export is-in-range))

(in-package :mnas-dim-value/func)

(defun print-hash-table (ht &optional (s t))
  "@b(Описание:) функция print-hash-table выполняет вывод содержимого
 таблицы в поток s.

@begin[lang=lisp](code)
 (print-hash-table *nm-vl*)
@end(code)
"
  (maphash
   #'(lambda (key val)
       (format s "~S ~S~%" key val))
   ht))

(defun is-in-range (val r-list)
" @b(Описание:) функция is-in-range служит для определения того
может-ли использоваться та или иная множительная приставка 
системы SI для данной единицы измерения.

 @b(Переменые:)
@begin(list)
 @item(val - множитель, определяемый формулой val=10@sup(n), где n - целое число;)
 @item(r-list - список, содержащий списки диапазонов. 
Диапазон - упорядоченый список двух целых - (n@sub(1) n@sub(2)).
Для n@sub(1) и n@sub(2) должно выполняться следующее неравенство n@sub(1) <= n@sub(2).)
@end(list)

 Функция возвращает T, если для одного из диапазонов выполняется следующее неравенство:
10@sup(n@sub(1)) <= val <= 10@sup(n@sub(2))

 @b(Пример использования:)
@begin[lang=lisp](code)
 (is-in-range 1/10 '((0 2))) => nil
 (is-in-range 1    '((0 2))) => T
 (is-in-range 10   '((0 2))) => T
 (is-in-range 100  '((0 2))) => T
 (is-in-range 1000 '((0 2))) => NIL
 (is-in-range 1000 '((0 2) (4 6))) => NIL
@end(code)
"
    (eval (append (list 'or)
	  (mapcar
	   #'(lambda (range)
	       (let ((r-rez
		      (mapcar
		       #'(lambda (el) (expt 10 el)) range)))
		 (list '<= (first r-rez) val (second r-rez) )))
	   r-list))))
