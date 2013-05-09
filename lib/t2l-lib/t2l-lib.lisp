; 
; (in-package :cl-user)
; (CLRHASH SCREAMER::*FUNCTION-RECORD-TABLE*)
; (pushnew :screamer-clos *features* :test #'eq)
; (delete-package :screamer-user)
; (delete-package :screamer)
; (load (om::om-relative-path '("screamer324") "screamer") :verbose t :print t)
(in-package :cl-user)
; (load (concatenate 'string (namestring (get-working-directory)) "ompw/load.lisp"))
(unless (find-package :t2l)
  (setf system:*stack-overflow-behaviour* :warn)
  (screamer:define-screamer-package :t2l 
                                    (:use :ompw :system)
                                    (:export "MAPPRULES")))

(in-package :t2l)
(print om:*current-lib*)
(defvar *t2l-lib-files* nil)
(setf *t2l-lib-files* (list ;(om::om-relative-path '(".") "screamer+")
                            (om::om-relative-path '(".") "general")
			    (om::om-relative-path '(".") "t2l-screamer+")
			    (om::om-relative-path '(".") "t2l-screamer")
                            (om::om-relative-path '(".") "t2l-screamer-boxes")
                            (om::om-relative-path '(".") "t2l-screamer-export")
			    (om::om-relative-path '(".") "t2l-screamer-misc")
			    (om::om-relative-path '(".") "t2l-omlib")
			    (om::om-relative-path '(".") "mapprules")
			    (om::om-relative-path '(".") "midic")
			    (om::om-relative-path '(".") "music-conversion")
			    ;(om::om-relative-path '(".") "enppar")
			    (om::om-relative-path '(".") "export")
                            ))

(mapc #'om::compile&load *t2l-lib-files*)

; using "make-pathname" plus *load-pathname*, allow us to put our library anywhere


;--------------------------------------------------
; Seting the menu and sub-menu structure, and filling packages
; The sub-list syntax:
; ("sub-pack-name" subpacke-lists class-list function-list class-alias-list)
;--------------------------------------------------

(om::fill-library 
 '(("music-conversion" 
    Nil
    Nil
    (ms-vars->elems
     ms-vars->ratios
     pcset-filter
     scale-ms-events
     seqc->ms-vars
     seqc->poly
     seqc->voices
     seqc-ms->elems
     seqc-ms->ratios) 
    Nil)

   ("music-conversion-rule" 
    Nil
    Nil
    (list-nsucc<>v
     listdx-similarity-var
     modal-xposn-containing
     pcset=v
     print-seqcx-ival-lists
     seqc-list-gap-memberv
     seqc-list-gap-vars
     seqc-ms-elemcount-div-ms-num-var
     seqc-ms-elemcount-is-multiple-of-msnv
     seqc-ms-ratios/=v
     seqc-ms-ratios<=v
     seqc-ms-ratios<>=v
     seqc-ms-ratios<>v
     seqc-ms-ratios<v
     seqc-ms-ratios=v
     seqc-ms-ratios>=v
     seqc-ms-ratios>v
     seqc-ms-signatures-mapv
     seqc-xl-ival-members-var
     seqc-xl-pcsets=v) 
    Nil)

   ))