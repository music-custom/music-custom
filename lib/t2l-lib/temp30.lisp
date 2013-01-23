(in-package :t2l)
(setq path "C:/Users/User/Documents/Projects/SCM/t2l-om/lib/t2l-lib/export2.lisp"
      syms (read-from-string (read-textfile path)))
(setq e (init-enppar (make-instance 'multipart-enppar-sequence) syms *export1-enppar-assoc*))
(midics (car (enps e)))
(setq e (process-enp e))
(print (expr e))
(write-textfile (expr e) "untiTLED" "ENP")
