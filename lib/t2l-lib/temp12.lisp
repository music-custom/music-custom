(in-package :t2l)
(setq path "C:/Users/User/Documents/Projects/SCM/t2l-om/lib/t2l-lib/export2.lisp"
      syms (read-textfile path))
(setq e (export1-symbols->enppar (cadr (read-from-string syms))))
(setq e (process-enp e))
(print (expr e))
(write-textfile (expr e) "untiTLED" "ENP")
