;;; Edit a valid LAMBDA EXPRESSION for "list-2species-canon-1"
;;; e.g. (lambda (arg1 arg2 ...) ( ... ))

(lambda (s xpos-levels voices)
  (unless (and xpos-levels
               (listp xpos-levels))
    (setf xpos-levels
          (cond ((numberp xpos-levels) (list xpos-levels))
                (t (list (t2l::arithm-ser 0 11 1))))))
  (unless voices (setf voices 2))
  (let* ((xpos-list (mapcar #'(lambda (x) (screamer:a-member-ofv xpos-levels))
                            (make-sequence 'list voices)))
         (voice-list (mapcar 
                      #'(lambda (x)
                          (mapcar #'(lambda (y)
                                      (screamer:+v x y))
                                  s))
                      xpos-list)))
    (let (ps)
      (dotimes (i (length voice-list))
        (push (t2l::rotate (t2l::nsucc (elt voice-list i) 2 :step 2) (* i -2))
              ps))
      (setf ps (reverse ps))
      (let ((seqc 
             (mapcar
              #'t2l::flat1
              (t2l::mat-trans
               (apply
                #'maplist 
                #'(lambda (&rest xs)
                    (let (c)
                      (dotimes (i (length xs))
                        (let ((ys (elt xs i)))
                          (push
                           (cond
                            ((= 0 (mod i 2))
                             (list (car ys) (cadar ys)))
                           (t
                            (cond
                             ((cdr ys)
                              (list (cadar ys) (cadr ys)))
                             (t
                              (list (cadar ys) (car (elt ps i)))))))
                           c)))
                      (reverse c)))
                ps)))))
        (list
         (cons 'xpos xpos)
         (cons 'seqc seqc))))))