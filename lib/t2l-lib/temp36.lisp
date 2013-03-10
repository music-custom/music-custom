(in-package :t2l)

(defun lists=v (list1 list2 &optional symbol-mode)
  (apply #'andv
         (mapcar #'(lambda (a b) (=v a b))
                 list1
                 list2)))

(defun a-permutation-ofv (list &key symbol-mode)
  (let ((vars (mapcar #'(lambda (x) 
                          (let ((v (an-integerv)))
                            (assert! (memberv v list))
                            v))
                      list))
        (perms (all-values (a-permutation-of list))))
    (assert! (reduce-chunks 
              #'orv                            
              (mapcar #'(lambda (p) (lists=v p vars))
                      perms)))
    vars))

(defun pcsets=v (set1 set2)
  (labels
      ((list->m12 (list)
         (mapcar #'(lambda (x); (if (possibly? (assert! (orv (<v x 0) (>v x 11))))
                              ;     (modv x 12)
                              ;   x))
                     (modv x 12))
                 list))
       (sorted-permutations-of (list)
         (all-values (let ((ps (a-permutation-of list)))
                       (unless (possibly? 
                                 (assert! (apply #'<=v ps))
                                 t)
                         (fail))
                       ps)))
       (rotatem12 (list i)
         (let ((rs (rotate list i)))
           (list->m12 (list-v rs (car rs)))))
       (all-rotations-of (list) 
         (mapcar #'(lambda (x) (rotatem12 list x))
                 (arithm-ser 0 (1- (length list)) 1))))
    (let ((set1m12 (list->m12 set1))
          (set2m12 (list->m12 set2)))
      (let ((ps1 (mapcar #'all-rotations-of (sorted-permutations-of set1m12)))
            (ps2 (mapcar #'all-rotations-of (sorted-permutations-of set2m12))))
        (reduce-chunks
         #'orv
         (mapcar
          #'(lambda (a)
              (reduce-chunks
               #'orv
               (mapcar
                #'(lambda (b)
                    (reduce-chunks
                     #'orv
                     (mapcar 
                      #'(lambda (c)
                          (reduce-chunks
                           #'orv
                           (mapcar
                            #'(lambda (d) (lists=v b d))
                            c)))
                      ps2)))                
               a)))
          ps1))))))
        
    
     
(setq s1 (list (an-integer-betweenv 4 6)
               (an-integer-betweenv 1 3)
               (an-integer-betweenv 7 9))
      s2 '(0 3 5))

(setq ps1 (all-values (let ((p (a-permutation-of s1)))
                        (unless (possibly? (assert! (apply #'<=v p)) t) (fail))
                        p)))
(setq ps1 (all-values (let ((p (a-permutation-of s1)))
                        ;(unless (possibly? (assert! (apply #'<=v p))) (fail))
                        p)))
(assert! (pcsets=v s1 s2))
(print s1)
(all-values (solution s1 (static-ordering #'linear-force)))
  
           