;;;; cl-user-funcs.lisp

(in-package #:mnas-dim-value )

(annot:enable-annot-syntax)

@export
@annot.doc:doc
"Импортирует внешние символы пакета mnas-dim-value
для импользования в проистранстве имет пакета :cl-user"
(defun use-mnas-dim-value ()
  (use-package (find-package :mnas-dim-value) (find-package :cl-user)))

@export
@annot.doc:doc
"Делает недоступными внешние символы пакета mnas-dim-value
в проистранстве имет пакета :cl-user"
(defun unuse-mnas-dim-value ()
  (unuse-package (find-package :mnas-dim-value) (find-package :cl-user)))
