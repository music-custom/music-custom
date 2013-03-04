(in-package :t2l)

(defun set-difference-eq (x y) (and (not (or (null x) (null y))) (or (and (null x) (null y)) (null (set-difference x y)))))
   

(setq min 60
      max 80 
      prules '((:S 5 :A)
               (:S :A 5)
               (:S :S :S)
               (:A -9)
               (:A -8)
               (:A -7)
               (:A -5)
               (:A -4)
               (:A -3)
               (:A -2)
               (:A -1)
               (:A 1)
               (:A 2)
               (:A 3)
               (:A 4)
               (:A 5)
               (:A 7))
      s '(a b c d e f g h i j k l m n o p))
(setq s (mapcar #'(lambda (x) (an-integer-betweenv min max)) s))

(assert! (mapprules s prules :listdxx t :input-process-increment 2))

(setq xpos1 (an-integer-betweenv 0 11))
(setq xpos1 5)

(setq s2 s)
(setq s2 (funcall-rec #'(lambda (x) (+v x xpos1)) s))

(setq ps (nsucc s 2 :step 2)
      ps2 (rotate (nsucc s2 2 :step 2) -2))
(mapcar #'print (list ps ps2))

(setq seqc
      (mat-trans
       (maplist #'(lambda (xs ys) 
                    (cond ((cdr ys)
                           (list (list (car xs) (cadar xs))
                                 (list (cadar ys) (cadr ys))))
                          (t
                           (list (list (car xs) (cadar xs))
                                 (list (cadar ys))))))
                ps ps2)))
(mapcar #'print seqc)

(setq cartx (remove-duplicates
               (flat1
                (mapcar 
                 #'(lambda (xs)
                     (remove-duplicates 
                      (remove-if #'(lambda (x) (eq (car x) (cadr x)))
                                 (cartesian-product xs xs))
                      :test #'set-difference-eq))
                 (mat-trans (flatten-seqc seqc))))
               :test #'set-difference-eq))
(print cartx)

(setq ivs (mapcar #'(lambda (xs) (modv (-v (car xs) (cadr xs)) 12)) cartx))


(assert! (all-memberv ivs '(2 3 4 5 6 7 8 9)))

(one-value (solution s (static-ordering #'linear-force)) 'fail)
