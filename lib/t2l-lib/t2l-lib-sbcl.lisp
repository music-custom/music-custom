(in-package :cl-user)
(load "C:/Users/User/Documents/Projects/SCM/music-conversion-lw/lib/ompw/load.lisp")
(load "C:/Users/User/Documents/Projects/SCM/music-conversion-lw/lib/t2l-lib/screamer324/screamer.lisp")
(screamer:define-screamer-package :t2l 
                                  (:use :system :ompw)
                                  (:export "MAPPRULES"))
(setq filepath "C:/Users/User/Documents/Projects/SCM/music-conversion-lw/lib/t2l-lib/")
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