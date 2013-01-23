(in-package :cl-user)
(progn
  (setq filepath (om:file-chooser))
  (om::set-outfiles-folder (directory-namestring filepath))
  (setq omi (t2l::read-from-string (t2l::read-textfile filepath)))
  (setq enp (t2l::om-expr->score-enp (system:cdr-assoc :trees omi) 
                                :midic-list (t2l::funcall-rec #'(lambda (x) (floor (* x 0.01))) 
                                                         (system:cdr-assoc :midics omi))))
  (t2l::write-textfile enp "convert" "enp"))