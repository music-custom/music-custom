(in-package :t2l)
(setq seqc '(x0 
             ((y0 y1 y2) (y3 y4))
             (z0 z1)))
(print (flatten-seqc seqc))
(setq
 list
 (let ((layers 3)
       (levels 1)
       (templates '(_ (_ _ (_)) ((_) _ _))))
   (labels
       ((embed (levels) (embed-rec  0 levels))
        (embed-rec (level levels)
          (let ((template (random-list-e templates)))
            (cond
             ((null template) nil)
             ((atom template) template)
             (t (funcall-rec #'(lambda (x) 
                                 (cond ((< level levels) (embed-rec (1+ level) levels))
                                       (t x)))
                             template))))))
     (let ((list (let ((list (random-list-e templates)))
                   (cond
                    ((null list) nil)
                    ((atom list) (list list))
                    (t list)))))
       (mapcar #'(lambda (x)
                   (embed levels))
               (make-sequence 'list layers))))))
(print seqc)
(print list)
'(_ _ (((((_) _ _)) (_ _ (_)) (_ _ (_)))))
'((_ (_ _ (_)) (_)) 
  ((_ _ (_)) (_ _ (_)) (((_) _ _))) 
  (((_) _ _) _ (_))) 

                           