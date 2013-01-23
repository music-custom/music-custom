(defmethod process-enp ((obj enppar-sequence))
  (print (format nil "calling process-enp::enppar-sequence on ~A" obj))
  (dolist (x (enps obj)) (process-enp x))
  (setf (expr obj)
        (mapcar
         #'(lambda (y) (apply #'append y))
         (mat-trans (mapcar #'expr (enps obj)))))
  obj)

(defmethod process-enp ((obj timepoint-seqc))
  (assert (modulus obj))
  (print (format nil "calling process-enp::timepoint-seqc on ~A modulus ~A prev-enp: ~A " obj (modulus obj) (cdr-assoc :prev-enp (params obj))))
  (let ((enps (collect-multi-enppar #'(lambda (x) (subtypep (type-of x) 't2l::timepoint-seqc-voice)) obj)))
    (dolist (e enps) 
      (if (and (midics e) (timepoints e))
          (let ((msub (subseq (midics e) 0 (min (length (midics e)) (length (timepoints e)))))
                (tsub (subseq (timepoints e) 0 (min (length (midics e)) (length (timepoints e))))))
            (print (format nil "process-enp::timepoint-seqc ms ~A ts ~A" (- (length (midics e)) (length msub)) (- (length (timepoints e)) (length tsub))))
            (setf (midics e) msub)
            (setf (timepoints e) tsub))))
    (mapcar 
     #'(lambda (voice)
         (setf (expr voice)
               (caar 
                (scale-seqc-timepoints (list (timepoints voice))
                                       (or (cdr-assoc :measures (params voice))
                                           (measures voice))
                                       (modulus obj)
                                       :enp (flat (midics voice))))))
     enps)
    (call-next-method)))

(defmethod process-enp :after ((obj timepoint-seqc))
  (if (>= *mess* 2)
      (let ((seqc-enps (collect-multi-enppar #'(lambda (x) (subtypep (type-of x) 'timepoint-seqc-voice)) obj)))
        (print (format nil "calling process-enp::timepoint-seqc :after ~A~%midics: ~A~%timepoints ~A" obj (mapcar #'midics seqc-enps) (mapcar #'timepoints seqc-enps)))))
  obj)

(defmethod process-enp ((obj timepoint-seqc-with-prules))
  
    (call-next-method))

(defmethod process-enp ((obj csptech-timepoint-seqc))
  (ith-value
   (or (cdr-assoc :solution-index (params obj)) 0)
   (let ((seqc-enps (collect-multi-enppar #'(lambda (x) (subtypep (type-of x) 'csptech-timepoint-seqc-voice)) obj)))
     (dolist (e seqc-enps)
       (setf (vars e) (funcall (cspvar-function e) (mergeassoc (params e) (list (cons :enppar e) (:seqc-enppar obj)))))
       (fill-slots e (vars e)))
     (setf (cdr-assoc :midics (vars obj)) (mapcar #'midics seqc-enps))
     (setf (cdr-assoc :timepoints (vars obj)) (mapcar #'timepoints seqc-enps))
     (dolist (fn (csp-rules obj)) (assert! (funcall fn (vars obj))))
     (fill-slots obj (solution (vars obj) (static-ordering #'linear-force)))
     (mapcar
      #'(lambda (e ms ts)
          (setf (midics e) ms)
          (setf (timepoints e) ts))
      seqc-enps
      (midics obj)
      (timepoints obj))))
  (call-next-method))

(defmethod process-enp ((obj timepoint-seqc-voice-with-prules))
  (enppar->timepoint-seqc-voice-with-prules-vars obj)

(defmethod process-enp ((obj timepoint-seqc-expansion))
  (assert (proto obj))
  (assert (subtypep (type-of (proto obj)) 'timepoint-seqc))
  (unless (expand-function obj)
    (setf (expand-function obj)
          #'(lambda (&rest params)
              (print (format nil "null expand function called :("))
              nil)))
  (let* ((proto (process-enp (proto obj)))
         (seqc-enps (collect-multi-enppar #'(lambda (x) (eq (type-of x) 'timepoint-seqc-voice-with-prules)) proto))
         (expand-midics nil)
         (expand-timepoints nil))
    (assert seqc-enps)
    (assert (every #'midics seqc-enps))
    (assert (every #'timepoints seqc-enps))
    (print (format nil "~A timepoint-seqc-expansion parts..." (length seqc-enps)))
    (dotimes (i (length seqc-enps))
      (let* ((penp (elt seqc-enps i))
             (part-index (or (cdr-assoc :index (params penp)) i))
             (expand-part-midics nil)
             (expand-part-timepoints nil))
        (dotimes (j (length (midics penp)))
          (let* ((midic (elt (midics penp) j))
                 (timepoint (elt (timepoints penp) j))
                 (expand-function-params nil))
            (setf (cdr-assoc :enppar expand-function-params) obj)
            (setf (cdr-assoc :index expand-function-params) j)
            (setf (cdr-assoc :midic-max expand-function-params) (cdr-assoc :midic-max (params penp)))
            (setf (cdr-assoc :midic-min expand-function-params) (cdr-assoc :midic-min (params penp)))
            (setf (cdr-assoc :midic expand-function-params) midic)
            (setf (cdr-assoc :midics expand-function-params) (midics penp))
            (setf (cdr-assoc :midic-index expand-function-params) j)
            (setf (cdr-assoc :part-index expand-function-params) part-index)
            (setf (cdr-assoc :register expand-function-params) part-index)
            (setf (cdr-assoc :seqc-enppar expand-function-params) (cdr-assoc :enppar expand-function-params))
            (setf (cdr-assoc :timepoint expand-function-params) timepoint)
            (setf (cdr-assoc :timepoints expand-function-params) (timepoints penp))
            (setf (cdr-assoc :timepoint-index expand-function-params) j)
            (print (format nil "expand-function: ~A" (expand-function obj)))
            (let ((expand-function-out-params (apply (expand-function obj) expand-function-params)))
              (if expand-function-out-params
                  (print (format nil "expand-function-params: ~A" expand-function-out-params)))
              (push (if (cdr-assoc :expand-midic expand-function-out-params)
                        (cdr-assoc :expand-midic expand-function-out-params)
                      midic)
                    expand-part-midics)
              (push (if (cdr-assoc :expand-timepoint expand-function-out-params)
                        (cdr-assoc :expand-timepoint expand-function-out-params)
                      timepoint)
                  expand-part-timepoints))))
        (setf expand-part-midics (reverse expand-part-midics))
        (push expand-part-midics expand-midics)
        (setf expand-part-timepoints (reverse expand-part-timepoints))
        (push expand-part-timepoints expand-timepoints)))
    (setf expand-midics (reverse expand-midics))
    (setf expand-timepoints (reverse expand-timepoints))
    (print (format nil "expand-midics ~A" expand-midics))
    (print (format nil "expand-timepoints ~A" expand-timepoints))
    (setf (cdr-assoc :midics (vars obj)) expand-midics)
    (setf (cdr-assoc :timepoints (vars obj)) expand-timepoints)
    (dolist (fn (csp-rules obj)) (assert! (funcall fn (vars obj))))
    (let ((soln (ith-value
                 (or (cdr-assoc :solution-index (params obj)) 0)
                 (print
                  (solution (vars obj)
                            (reorder #'domain-size
                                     #'(lambda (x) (declare (ignore x)) nil)
                                     #'<
                                     #'linear-force))))))
      (print (format nil
                     "timepoint-seqc-expansion solution ~A"
                     soln))
      (setf (proto obj) (make-instance 'timepoint-seqc 
                                       :enps (enps proto)
                                       :modulus (modulus proto)
                                       :params (params proto)))
      (mapcar
       #'(lambda (o ms ts) 
           (setf (midics o) ms)
           (setf (timepoints o) ts))
       (collect-multi-enppar #'(lambda (x) (subtypep (type-of x) 'timepoint-seqc-voice)) (proto obj))
       (cdr-assoc :midics soln)
       (cdr-assoc :timepoints soln))
      (process-enp (proto obj)))
    obj))


















          

         (let ((tv (lsubs1 timevars nil)))
           (print (format nil "process-enp: (~A) time-csp-predicates" (length (time-csp-predicates obj))))
           (dolist (fn (time-csp-predicates obj)) (assert! (funcall fn tv)))
           (let ((mv (mapcar #'(lambda (x y) (subseq (cdr-assoc :midics (vars x)) 0 (length y))) seqc-enps tv))
                 (vars nil))
             (setf (cdr-assoc :midics vars) mv
                   (cdr-assoc :timepoints vars) tv
                   (vars obj) vars)
             (print (format nil "process-enp: (~A) csp-rules" (length (csp-rules obj))))
             (dolist (fn (csp-rules obj)) (assert! (funcall fn vars)))
             (assert! (apply #'andv (mapcar #'(lambda (xs) (>=v (apply #'+v xs) msbeats)) tv)))
             (print (format nil "process-enp: calling screamer:solution for music timing rules...  ~A" (mapcar #'length tv)))
             
             (let ((timesoln1 (solution tv (static-ordering #'linear-force))))
               (print (format nil "process-enp: timesoln1  ~A" (mapcar #'length timesoln1)))
               (let* ((timesoln (mapcar
                                 #'(lambda (xs)
                                     (let ((c nil))
                                       (dolist (x xs)
                                         (push x c)
                                         (when (>= (apply #'+ c) msbeats) (return)))
                                       (let ((diff (- (apply #'+ c) msbeats)))
                                         (reverse
                                          (cond ((= 0 diff) c)
                                                (t (append (list (- (car c) diff)) (cdr c))))))))
                                 timesoln1))
                      (midicsoln (let ((seqc (mapcar 
                                              #'(lambda (xs ys) 
                                                  (flat
                                                   (mapcar
                                                    #'(lambda (x y) (make-sequence 'list y :initial-element x))
                                                    xs
                                                    ys)))
                                              mv
                                              timesoln)))
                                   (print (format nil "process-enp: midic-csp-predicates (~A)..." (length (midic-csp-predicates obj))))
                                   (dolist (fn (midic-csp-predicates obj)) (assert! (funcall fn seqc)))                                   
                                   (print (format nil "process-enp: calling screamer:solution for seqc (~A) ~A..." (length seqc) (mapcar #'length mv)))
                                   (solution mv  (static-ordering #'linear-force)))))
                 (setf (cdr-assoc :timepoints (vars obj)) timesoln)              
                 (mapcar 
                  #'(lambda (midics timepoints voice)
                      (setf (midics voice) midics
                            (timepoints voice) timepoints))
                  midicsoln
                  timesoln
                  seqc-enps))))))))))