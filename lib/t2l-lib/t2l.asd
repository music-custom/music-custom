(in-package :ASDF)

(defsystem "t2l"

  :description "PWGL Screamer Functions"
  :long-description ""
  :version "2"
  :author "emergn36@hotmail.com"
  :licence ""
  :maintainer ""

  ;; :serial t means that each component is only compiled, when the
  ;; predecessors are already loaded
  :serial t
  :components
  (
   (:FILE "package")
   (:FILE "general") 
   (:FILE "t2l-screamer")
   (:FILE "t2l-screamer-export")
   (:FILE "midic")
   (:FILE "music-conversion")
   (:FILE "enppar")
   (:FILE "export"))
  :depends-on
  (:screamer :ompw))
