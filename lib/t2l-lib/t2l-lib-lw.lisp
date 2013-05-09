(in-package :cl-user)

(load "C:/Users/User/Documents/Projects/OM-6.5.1-SRC/buildimage/build-om/build-om.lisp")

(setq ws (get-working-directory))
(setq ompw-pathname (make-pathname :directory (append (butlast (PATHNAME-DIRECTORY ws)) (list "ompw")) :host (pathname-host ws)))
(pushnew (namestring ompw-pathname) asdf:*central-registry* :test #'eq)
(asdf:operate 'asdf:load-op :ompw)

(screamer:define-screamer-package :t2l 
                                  (:use :system :ompw)
                                  (:export "MAPPRULES"))
(setq filepath (namestring ws))
(dolist (x '(
             "general"
             "t2l-screamer+"
             "t2l-screamer"
             "t2l-screamer-boxes"
             "t2l-screamer-misc"
             "t2l-omlib"
             "mapprules"
             "midic"
             "music-conversion"
               ;"enppar"
             "export"
             ))
  (load (concatenate 'string filepath x ".lisp")))