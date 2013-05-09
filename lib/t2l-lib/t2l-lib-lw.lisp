(in-package :cl-user)
(setf system:*stack-overflow-behaviour* :warn)
; (load "C:/Program Files (x86)/OM 6.5.1/buildimage/build-om/build-om.lisp")
(load "C:/Users/User/Documents/Projects/OM-6.5.1-SRC/buildimage/build-om/build-om.lisp")
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

#|(setq omtristan30-ws (make-pathname :directory (append (butlast (PATHNAME-DIRECTORY ws)) (list "OmTristan 3.0" "sources")) :host (pathname-host ws)))
(if (probe-file omtristan30-ws)
    (load (concatenate 'string (namestring omtristan30-ws) "TMlibrairie-OM.lisp")))|#