(in-package :cl-user)
(setf system:*stack-overflow-behaviour* :warn)
(load (concatenate 'string (namestring (get-working-directory)) "screamer324/screamer.lisp"))
(screamer:define-screamer-package :t2l 
  (:use :system)
  (:export "MAPPRULES"))
(get-working-directory)
(setq filepath (namestring (get-working-directory)))
(dolist (x '(
             "general"
             "t2l-screamer"
             ))
  (load (concatenate 'string filepath x ".lisp")))
