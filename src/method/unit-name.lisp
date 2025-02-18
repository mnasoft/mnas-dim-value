(in-package :mnas-dim-value/method)

(defmethod unit-name ((x <vd>))
  (let ((s (make-string-output-stream)))
    (multiple-value-bind (dimens find)
        (gethash (<vd>-dims x) (mnas-dim-value/ht:dim->unit-symbol))
      (if find
	  (format s "~A" dimens)
	  (progn (format s "[" )
	         (mapc #'(lambda (no str)
			   (cond
			     ((= (nth no (<vd>-dims x)) 1)
                              (format s (concatenate 'string str "")))
			     ((/= (nth no (<vd>-dims x)) 0)
                              (format s (concatenate 'string str "^~A") (nth no (<vd>-dims x))))))
		       '( 0    1   2   3   4    5     6     7    8)
		       (cond
		         ((eq (get-env "LANGUAGE" *variable-set*) :en) +vd-names-en+)
		         ((eq (get-env "LANGUAGE" *variable-set*) :ru) +vd-names-ru+)))
	         (format s "]"))))
    (get-output-stream-string s)))
