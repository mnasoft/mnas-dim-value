;;;; cl-user-funcs.lisp

(in-package :mnas-dim-value )

(export 'use-mnas-dim-value )

(defun use-mnas-dim-value ()
"Импортирует внешние символы пакета mnas-dim-value
для импользования в проистранстве имет пакета :cl-user"
  (use-package (find-package :mnas-dim-value) (find-package :cl-user)))

(export 'unuse-mnas-dim-value )

(defun unuse-mnas-dim-value ()
"Делает недоступными внешние символы пакета mnas-dim-value
в проистранстве имет пакета :cl-user"
  (unuse-package (find-package :mnas-dim-value) (find-package :cl-user)))
