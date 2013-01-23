(in-package :cl-user)
(setf system:*stack-overflow-behaviour* :warn)
(load "C:/Program Files (x86)/OM 6.5.1/buildimage/build-om/build-om.lisp")
(setq ws (get-working-directory))
(setq ompw-pathname (make-pathname :directory (append (butlast (PATHNAME-DIRECTORY ws)) (list "ompw")) :host (pathname-host ws)))
(pushnew (namestring ompw-pathname) asdf:*central-registry* :test #'eq)
(asdf:operate 'asdf:load-op :ompw)
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
