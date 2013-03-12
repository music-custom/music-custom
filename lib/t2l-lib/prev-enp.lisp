(cond
                      ((and (cdr-assoc :continue (params obj))
                            (cdr-assoc :prev-enp (params obj))
                            (subtypep (type-of (cdr-assoc :prev-enp (params obj))) 'midic-enppar)
                            (midics (cdr-assoc :prev-enp (params obj)))
                            (last-atom (remove nil (flat (midics (cdr-assoc :prev-enp (params obj)))))))
                       (let* ((init (an-integer-betweenv 0 127))
                              (prev-midics 
                               (remove nil 
                                (flat 
                                 (midics (cdr-assoc :prev-enp (params obj))))))
                              (prev (last-atom prev-midics))
                              (prev-mhead (car prev-midics)))
                         (if (cdr-assoc :midic-max (params obj))
                             (assert! (<=v init (cdr-assoc :midic-max (params obj)))))
                         (if (cdr-assoc :midic-min (params obj))
                             (assert! (>=v init (cdr-assoc :midic-min (params obj)))))
                         (cond
                          ((possibly? 
                             (assert! (andv (/=v init prev-mhead)
                                            (orv (=v init prev)
                                                 (andv (>=v init (-v prev 3))
                                                       (<=v init (+v prev 3)))
                                                 (if (cdr-assoc :midic-start (params obj))
                                                     (orv (=v init (cdr-assoc :midic-start (params obj)))
                                                          (andv (>=v init (-v (cdr-assoc :midic-start (params obj)) 3))
                                                                (<=v init (+v (cdr-assoc :midic-start (params obj)) 3))))))))
                             t)
                           (assert! (andv (/=v init prev-mhead)
                                          (orv (=v init prev)
                                               (andv (>=v init (-v prev 3))
                                                     (<=v init (+v prev 3)))
                                               (if (cdr-assoc :midic-start (params obj))
                                                   (orv (=v init (cdr-assoc :midic-start (params obj)))
                                                        (andv (>=v init (-v (cdr-assoc :midic-start (params obj)) 3))
                                                              (<=v init (+v (cdr-assoc :midic-start (params obj)) 3))))))))
                           ;print (format nil "22222 prev-mhead ~A" prev-mhead))
                           )
                          ((possibly?
                             (assert! (/=v init prev-mhead))
                             t)
                           (assert! (/=v init prev-mhead)))
                          (t
                           (let* ((init (an-integer-betweenv 0 127)))
                             (if (cdr-assoc :midic-max (params obj))
                                 (assert! (<=v init (cdr-assoc :midic-max (params obj)))))
                             (if (cdr-assoc :midic-min (params obj))
                                 (assert! (>=v init (cdr-assoc :midic-min (params obj)))))
                             init)))
                         init))
                      (t (let* ((init (an-integer-betweenv 0 127)))
                             (if (cdr-assoc :midic-max (params obj))
                                 (assert! (<=v init (cdr-assoc :midic-max (params obj)))))
                             (if (cdr-assoc :midic-min (params obj))
                                 (assert! (>=v init (cdr-assoc :midic-min (params obj)))))
                             init)))