(in-package :mnas-dim-value/method)

(defun dimensionp (str)
"@b(Описание:) функция dimensionp 
@begin[lang=lisp](code)
   (dimensionp \"m\")                 => 1 m
   (dimensionp (string-upcase \"t\")) => 1 T
   (dimensionp \"Pa\")                => 1 Pa
 ;;(dimensionp \"knot\")
@end(code)
"
  (multiple-value-bind (val find) (gethash str mnas-dim-value/ht-en:*nm->value*)
    (if find val nil)))
