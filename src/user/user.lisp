;;;; cl-user-funcs.lisp

(defpackage :mnas-dim-value/user
  (:nicknames "MDV/USER")
  (:use #:cl)
  (:export use
           unuse
           ))

(in-package :mnas-dim-value/user )

(defun use ()
  "Импортирует внешние символы пакета mnas-dim-value
для импользования в проистранстве имет пакета :cl-user"
  (use-package (find-package :mnas-dim-value) (find-package :cl-user)))

(defun unuse ()
  "Делает недоступными внешние символы пакета mnas-dim-value
в проистранстве имет пакета :cl-user"
  (unuse-package (find-package :mnas-dim-value) (find-package :cl-user)))
