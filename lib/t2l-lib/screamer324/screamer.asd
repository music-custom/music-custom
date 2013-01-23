;;;; Silly emacs, this is -*- Lisp -*-
;;;; arch-tag: 3AEA583B-0A11-1454-E9321DE74C68

(in-package :asdf)

(defsystem :screamer
  :components
  ((:static-file "screamer.asd")
   (:file "screamer")))

