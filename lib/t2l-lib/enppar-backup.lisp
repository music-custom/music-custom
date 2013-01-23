(defmethod process-enp ((obj multivoice-enppar-sequence))
  (dolist (x (enps obj))
    (unless (expr x) (process-enp x)))
  (let ((parts nil))
    (dotimes (i (apply #'max (mapcar #'length (mapcar #'enps (enps obj)))))
      (lprint 'part 'i i)
      (setf (cdr-assoc i parts) nil))
    (dolist (i (mapcar #'car parts))
      (let ((part-voice-count 0))
        (dolist (e (enps obj))
          (if (and (< i (length (enps e)))
                   (elt (enps e) i)
                   (subtypep (type-of (elt (enps e) i)) 'multi-enppar)
                   (> (length (enps (elt (enps e) i))) part-voice-count))
              (setf part-voice-count (length (enps (elt (enps e) i))))
              (lprint 'part-voice-count part-voice-count)))
        (setf (cdr-assoc i parts) 
              (let ((pl nil))
                (dotimes (j part-voice-count)
                  (setf (cdr-assoc j pl) nil))
                pl))))
    (dolist (i (mapcar #'car parts))
      (dotimes (j (length (cdr-assoc i parts)))
        (dolist (e (enps obj))
          (let* ((pe (if (< i (length (enps e)))
                         (elt (enps e) i)
                       nil))
                 (ve (if (and pe
                              (< j (length (enps pe))))
                         (elt (enps pe) j)))
                 (msexpr (cond ((null ve)
                                (setf (cdr-assoc j (cdr-assoc i parts))
                                      (append (cdr-assoc j (cdr-assoc i parts))
                                              (mapcar #'(lambda (ms) (list (list (car ms) (list -1))
                                                                           :time-signature ms))
                                                      (measures e)))))
                               (t 
                                (setf (cdr-assoc j (cdr-assoc i parts))
                                      (append (cdr-assoc j (cdr-assoc i parts))
                                              (expr ve)))))))
            nil))))
    (let (ps)
      (dotimes (i (length parts))
        (let (p)
          (dotimes (j (length (cdr-assoc i parts)))
            (push-to-end (cdr-assoc j (cdr-assoc i parts)) p))
          (push-to-end p ps)))
      (setf (expr obj) ps))
          
    (lprint 'parts parts))
  obj)









(dolist (e cs)
        (dotimes (i (length (enps e)))
          (let* ((f (elt (enps e) i))
                 (p-index (or (cdr-assoc :index (params f)) i)))
            (unless (assoc p-index ps) (setf (cdr-assoc p-index ps) nil))
            (dotimes (j (length (enps f)))
              (let* ((g (elt (enps f) j)))
                (unless (assoc j (cdr-assoc p-index ps)) 
                  (setf (cdr-assoc j (cdr-assoc p-index ps)) nil)))))))
      (dolist (e (enps obj))
        (let* ((c (collate-part-level-enp e)))
          (dolist (p-index (mapcar #'car ps))
            (dolist (v-index (mapcar #'car (cdr-assoc p-index ps)))
              (print (format nil 
                             "collate-part-level-enp c: ~A p-index: ~A v-index: ~A enp: ~A" 
                             c
                             p-index
                             v-index
                             (cdr-assoc v-index (cdr-assoc p-index c))))
              (setf (cdr-assoc v-index (cdr-assoc p-index ps))
                    (append 
                     (cdr-assoc v-index (cdr-assoc p-index ps))
                     (cond ((or (null (cdr-assoc p-index c))
                                (null (cdr-assoc v-index (cdr-assoc p-index c))))
                            (mapcar #'blank-measure-enp (measures e)))
                           (t 
                            (cdr-assoc v-index (cdr-assoc p-index c))))))))))
      (let (expr)
        (dotimes (i (length ps))
          (let (p)
            (dotimes (j (length (cdr-assoc i ps)))
              (push-to-end (cdr-assoc j (cdr-assoc i ps)) p))
            (push-to-end p expr)))