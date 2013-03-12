(in-package :t2l)
(setq l1 (list (an-integer-betweenv 1 5) (an-integer-betweenv 1 5) (an-integer-betweenv 1 5)))
(setq l2 '(3 5 2))
(assert! (set-equalv l1 l2))

(print (all-values (solution l1 (static-ordering #'linear-force))))
