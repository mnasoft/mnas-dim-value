(in-package :mnas-dim-value/method)

(defmethod quantity-name ((value <vd>) &key (vd-language (get-env "LANGUAGE" *variable-set*)))
  "Возвращает наименование величины.
Пример использования:

 @b(Пример использования:)
@begin[lang=lisp](code)
  (quantity-name (vd/ \"kg\" \"m\" \"m\" \"m\") :vd-language :en)
  => (\"density\" \"mass density\")
  (quantity-name (vd/ (vd* \"kg\" *g*) (vd-expt (vd* 0.01 \"m\") 2) 1000))
@end(code)

"
  (let ((rez nil)
	(item nil)
	(quantity-name-language
	 (cond
	   ((eq vd-language :en) #'<nd>-quantity)
	   ((eq vd-language :ru) #'<nd>-quantity))))
    (mapc
     #'(lambda (el)
	 (when (equal (<vd>-dims (<nd>-value el))
		      (<vd>-dims value))
	   (setf item (funcall quantity-name-language el))
	   (cond
	     ((stringp item) (push item rez))
	     ((listp item) (setf rez (append item rez))))))
     (apply #'append mnas-dim-value/ht-en:*nd-list*))
    (remove-duplicates rez :test #'equal)))

(defmethod same-dimension ((x <vd>) (y <vd>))
  "Проверяет два числа с размерностью на совпадение"
  (equal (<vd>-dims x) (<vd>-dims y)))
