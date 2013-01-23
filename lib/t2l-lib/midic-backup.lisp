(let ((tree (cond (enp
                         (list ; voice
                          (mapcar 
                           #'(lambda (signature beats)
                               (append 
                                (mapcar 
                                 #'(lambda (beat)
                                     (list (car beat)
                                           (mapcar 
                                            #'(lambda (p)
                                                (let ((p1 (cond ((and (listp enp) enp-midics) p)
                                                                ((listp enp) (* -1 p))
                                                                (t p))))
                                                  (list p1 :NOTES (list
                                                                   (cond ((and (listp enp)
                                                                               enp-midics)
                                                                          (unless (floatp p) (pop enp-midics))
                                                                          (if enp-midics (car enp-midics) -1))
                                                                         ((listp enp) -1)
                                                                         (t 60))))))
                                            (cadr beat))))
                                 beats)
                                (list :time-signature signature)))
                           ms
                           partns-mrg)))
                        (list-mode (prcs-ms-timepoint-sigs ms partns-mrg :list-mode t))
                        (t
                         (append (list (read-from-string "?"))
                                 (list (prcs-ms-timepoint-sigs ms partns-mrg)))))))       