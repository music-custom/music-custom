;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-

(defsystem :soundex
  :components ((:file "package")
               (:file "soundex" :depends-on ("package"))))
