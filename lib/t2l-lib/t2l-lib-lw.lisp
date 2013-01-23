(in-package :cl-user)
(setf system:*stack-overflow-behaviour* :warn)
(load "C:/Program Files (x86)/OM 6.5.1/buildimage/build-om/build-om.lisp")
(pushnew (concatenate 'string (namestring (get-working-directory)) "ompw/") asdf:*central-registry* :test #'eq)
(asdf:operate 'asdf:load-op :ompw)
; (load (concatenate 'string (namestring (get-working-directory)) "ompw/load.lisp"))
; (load (concatenate 'string (namestring (get-working-directory)) "screamer324/screamer.lisp"))
(screamer:define-screamer-package :t2l 
                                  (:use :system :ompw)
                                  (:export "MAPPRULES"))
(setq filepath (namestring (get-working-directory)))
(dolist (x '(
               "general"
               "t2l-screamer"
               "t2l-screamer-boxes"
               "t2l-screamer-misc"
               "t2l-omlib"
               "midic"
               "music-conversion"
               "enppar"
               "export"
               ))
  (load (concatenate 'string filepath x ".lisp")))
