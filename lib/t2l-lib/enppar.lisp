(in-package :t2l)

(defvar *solution* "lf")
(defvar *syntax* :om)

(defclass enppar ()
  ((expr :accessor expr
         :initarg :expr
         :initform nil)
   (params :accessor params
           :initarg :params
           :initform nil)))

(defclass multi-enppar (enppar)
  ((enps :accessor enps
         :initarg :enps
         :initform nil)))

(defclass multipart-enppar (multi-enppar) ())

(defclass multipart-enppar-combination (multipart-enppar) ())

(defclass enppar-sequence (multi-enppar) ())

(defclass multipart-enppar-sequence (enppar-sequence) ())

(defclass enppar-sequence-demo (enppar-sequence) ())

(defclass enppar-wrapper (enppar)
  ((proto :accessor proto
          :initarg :proto
          :initform nil)))

(defclass csptech-enp (enppar)
  ((csp-rules :accessor csp-rules
	      :initarg :csp-rules
	      :initform nil)
   (cspvar-function :accessor cspvar-function
                    :initarg :cspvar-function
                    :initform nil)
   (solution-index :accessor solution-index
                   :initarg :solution-index
                   :initform 0)
   (vars :accessor vars
         :initarg :vars
         :initform nil)))

(defclass midic-enppar (enppar)
  ((measures :accessor measures
             :initarg :measures
             :initform nil)
   (midics :accessor midics
           :initarg :midics
           :initform nil)))

(defclass timepoint-seqc (midic-enppar multipart-enppar)
  ((modulus :accessor modulus
            :initarg :modulus
            :initform nil)
   (timepoints :accessor timepoints
               :initarg :timepoints
               :initform nil)))

(defclass timepoint-seqc-voice (midic-enppar multi-enppar)  
  ((modulus :accessor modulus
            :initarg :modulus
            :initform nil)
   (m* :accessor m*
       :initarg :m*
       :initform 1)
   (timepoints :accessor timepoints
               :initarg :timepoints
               :initform nil)))

(defclass csptech-timepoint-seqc (timepoint-seqc csptech-enp) ())

(defclass csptech-timepoint-seqc-voice (timepoint-seqc-voice csptech-enp) ())

(defclass timepoint-seqc-voice-with-prules (timepoint-seqc-voice csptech-enp)
  ((midicrules :accessor midicrules
               :initarg :midicrules
               :initform nil)
   (timerules :accessor timerules
              :initarg :timerules
              :initform nil))
  (:documentation 
"params: 
[mapprules|midic|time][rules-][input-process-increment|nondeterministic-values-cap]
midic-[start|min|max]"))



(defclass timepoint-seqc-with-prules (timepoint-seqc csptech-enp)
  ((midic-csp-predicates :accessor midic-csp-predicates
                        :initarg :midic-csp-predicates
                        :initform nil)
   (time-csp-predicates :accessor time-csp-predicates
                        :initarg :time-csp-predicates
                        :initform nil)))

(defclass timepoint-seqc-expansion (multipart-enppar csptech-enp enppar-wrapper)
  ((expand-function :accessor expand-function
                    :initarg :expand-function
                    :initform nil
                    :type function
                    :documentation "&rest parmas midic timpoint index part-index proto seqc-enppar")))

(defclass export1-timepoint-seqc (timepoint-seqc csptech-enp) ())

(defclass export1-timepoint-seqc-voice (timepoint-seqc-voice csptech-enp) ())

(defmethod measures ((obj enppar-wrapper)) (measures (proto obj)))


(defgeneric init-enppar (enppar list &optional list))

(defun init-enppar-slots (obj symbols &optional fnassoc)
  (let ((slot-params (fill-slots obj symbols)))
    (setf (params obj)
          (mergeassoc (params obj)
                      (remove-if-not #'(lambda (x) 
                                         (and x
                                              (listp x) 
                                              (atom (car x)) 
                                              (not (position (car x) slot-params))))
                                     symbols)))
    obj))

(defmethod init-enppar ((obj enppar) (symbols list) &optional (fnassoc list))
  (init-enppar-slots obj symbols fnassoc))

(defmethod init-enppar ((obj csptech-timepoint-seqc) (symbols list) &optional (fnassoc list))
  (init-enppar-slots obj symbols fnassoc))

(defmethod init-enppar ((obj multipart-enppar-sequence) (symbols list) &optional (fnassoc list))
  (setf (enps obj)
        (mapcar #'(lambda (syms)
                    (init-enppar (if (cdr-assoc (car syms) fnassoc)
                                     (funcall (cdr-assoc (car syms) fnassoc) (cdr syms))
                                   (make-instance (car syms)))
                                 (cdr syms)
                                 fnassoc))
                symbols))
  obj)

(defmethod init-enppar ((obj export1-timepoint-seqc) (symbols list) &optional (fnassoc list))
  (let ((last-sym (if symbols (car (reverse symbols)))))
    (let ((param-syms (if (and last-sym
                               (listp last-sym)
                               (every #'listp last-sym))
                          (butlast symbols)
                        symbols))
          (enp-syms (if (and last-sym
                             (listp last-sym)
                             (every #'listp last-sym))
                        last-sym)))
      (dotimes (i (length enp-syms))
        (setf (cdr-assoc :index (cdr (elt enp-syms i)))
              (or (cdr-assoc :index (cdr (elt enp-syms i))) i)))
      (let ((slot-syms (fill-slots obj param-syms)))
        (setf (params obj) 
              (remove-if-not #'(lambda (x) (and x (listp x) (not (position (car x) slot-syms))))
                             param-syms)))
      
      (setf (enps obj)
            (mapcar #'(lambda (syms)
                        (let ((voice-params (mergeassoc (cdr syms) param-syms)))
                          (let ((e (init-enppar (if (cdr-assoc (car syms) fnassoc)
                                                    (funcall (cdr-assoc (car syms) fnassoc) voice-params)
                                                  (make-instance (car syms)))
                                                voice-params
                                                fnassoc)))
                            (setf (params e) (mergeassoc (params e) (params obj)))
                            e)))
                        enp-syms))
      obj)))

(defmethod init-enppar ((obj export1-timepoint-seqc-voice) (symbols list) &optional (fnassoc list))
  (let ((slot-syms (fill-slots obj symbols)))
    (setf (params obj) 
          (mergeassoc (params obj)
                      (remove-if-not #'(lambda (x) (and x (listp x) (not (position (car x) slot-syms)))) symbols)))
    obj))


(defgeneric collate-part-level-enp (enppar))

(defmethod collate-part-level-enp ((obj enppar))
  (expr obj))


(defmethod collate-part-level-enp ((obj multi-enppar))
  (print (format nil "collate-part-level-enp >>> multi-enppar ~A" obj))
  (let ((ps nil))
    (dotimes (i (length (enps obj)))
      (let* ((e (elt (enps obj) i))
             (eexpr (collate-part-level-enp e)))
      (setf (cdr-assoc i ps) eexpr)))
    ps))

(defmethod collate-part-level-enp ((obj multipart-enppar))
  (print (format nil "collate-part-level-enp >>> multipart-enppar ~A" obj))
  (let ((ps nil))
    (dotimes (i (length (enps obj)))
      (let* ((e (elt (enps obj) i))
             (index (or (cdr-assoc :index (params e)) i))
             (eexpr (collate-part-level-enp e)))
        (setf (cdr-assoc index ps) eexpr)))
    ps))

(defmethod collate-part-level-enp ((obj multipart-enppar-combination))
  (print (format nil "collate-part-level-enp >>> multipart-enppar-combination ~A" obj))
  (let ((parts (sort 
                (remove-duplicates
                 (flat
                  (loop for i from 0 while (< i (length (enps obj)))
                        collect (loop for j from 0 while (< j (length (enps (elt (enps obj) i))))
                                      collect (let ((e (elt (enps (elt (enps obj) i)) j))
                                                    (params nil))
                                                (setf (cdr-assoc :index params) (or (cdr-assoc :index (params e)) j))
                                                (make-instance 'multi-enppar :params params)))))
                 :test #'=
                 :key #'(lambda (x) (cdr-assoc :index (params x))))
                #'< 
                :key #'(lambda (x) (cdr-assoc :index (params x))))))
    (loop for i from 0 while (< i (length (enps obj)))
          do (loop for j from 0 while (< j (length (enps (elt (enps obj) i))))
                   (let* ((e (elt (enps (elt (enps obj) i)) j))
                          (p (find (or (cdr-assoc :index (params e)) j) 
                                   parts 
                                   :key #'(lambda (x) (cdr-assoc :index x)))))
                     (loop for k from 0 while (< k (length (enps e)))
                           do (push-to-end (elt (enps e) k) (enps p))))))
    (collate-part-level-enp (make-instance 'multipart-enppar
                                           :enps parts))))

(defmethod collate-part-level-enp ((obj enppar-sequence))
  (print (format nil "collate-part-level-enp>>>>enppar-sequence ~A" obj))
  (let ((ps nil))
    (dotimes (i (length (enps obj)))
      (let* ((e (elt (enps obj) i))
             (c (collate-part-level-enp e)))
        (setf (cdr-assoc i ps) c)))
    ps))

(defmethod collate-part-level-enp ((obj multipart-enppar-sequence))
  (print (format nil "collate-part-level-enp >>> multipart-enppar-sequence ~A" obj))
  (labels
      ((blank-measure-enp (signature) 
         (list (list (car signature) (list -1))
               :time-signature signature))
       (blank-om-voice (signatures)
         (list (read-from-string "?")
               (mapcar #'(lambda (signature) (list signature (list -1)))
                       signatures)))
       (collate-enp ()
         (let ((cassoc nil)
               (cs nil)
               (ps nil))
           (dotimes (i (length (enps obj)))
             (push-to-end (cons i (elt (enps obj) i)) cassoc))
           (dotimes (i (length cassoc))
             (push-to-end (collate-part-level-enp (cdr-assoc i cassoc)) cs))
           (dolist (c cs)
             (dolist (pkey (mapcar #'car c))
               (unless (assoc pkey ps)
                 (setf (cdr-assoc pkey ps) nil))
               (dolist (vkey (mapcar #'car (cdr-assoc pkey c)))
                 (unless (assoc vkey (cdr-assoc pkey ps))
                   (setf (cdr-assoc vkey (cdr-assoc pkey ps)) nil)))))
           (dolist (pkey (mapcar #'car ps))
             (dolist (vkey (mapcar #'car (cdr-assoc pkey ps)))
               (dotimes (i (length cassoc))
                 (setf (cdr-assoc vkey (cdr-assoc pkey ps))
                       (append (cdr-assoc vkey (cdr-assoc pkey ps))                          
                               (cond ((and (cdr-assoc pkey (elt cs i))
                                           (cdr-assoc vkey (cdr-assoc pkey (elt cs i))))
                                      (cdr-assoc vkey (cdr-assoc pkey (elt cs i))))
                                     (t 
                                      (mapcar #'blank-measure-enp (measures (cdr-assoc i cassoc))))))))))
           (if (>= *mess* 3) (print (format nil "collate-part-level-enp multipart-enppar-sequence ps: ~A" ps)))
           ps))
       (collate-om ()
         (let ((cs nil)) ; om:voice list 
           (dotimes (i (length (enps obj)))
             (push-to-end (collate-part-level-enp (elt (enps obj) i)) cs))
           (let ((voices (make-sequence 'list (reduce #'max (mapcar #'length cs)))))
             (dotimes (i (length cs))
               (dotimes (j (length voices))
                 (push-to-end (cond ((or (>= j (length (elt cs i)))
                                         (null (elt (elt cs i) j)))
                                     (blank-om-voice (measures (elt cs i))))
                                    (t
                                     (elt (elt cs i) j)))
                              (elt voices j))))

             (print (format nil "voices: ~A" voices))
             (let* ((voice-list (mapcar #'concat-rec voices))
                    (chords (mapcar #'om::chords voice-list)))
               (list (cons :midics (cond ((or (null chords) (every #'null chords))
                                          (make-sequence 'list (length chords)))
                                         (t
                                          (mapcar #'(lambda (xs) 
                                                      (mapcar #'car xs))
                                           (funcall-rec #'(lambda (x) (if x (om::lmidic x) '((6000))))
                                                        (mapcar #'om::chords voice-list))))))
                     (cons :trees (mapcar #'om::tree voice-list))))))))                            
    (cond
     ((eq *syntax* :om) (collate-om))
     (t (collate-enp)))))

(defmethod collate-part-level-enp ((obj csptech-timepoint-seqc))
  (mapcar
   #'(lambda (ms ts)
       (make-instance 'om::voice
                      :chords (om* ms 100)
                      :tree ts))
   (midics obj)
   (timepoints obj)))



(defun enppar-text->poly (input)
  (cond
   ((stringp input) (enppar-text->poly (read-from-string input)))
   (t
    (let ((midics-list (cdr-assoc :midics input))
          (tree-list (cdr-assoc :trees input)))
      (make-instance 'om::poly
                     :voices (mapcar #'(lambda (mcs trs)
                                         (make-instance 'om::voice
                                                        :chords mcs
                                                        :tree trs))
                                     midics-list
                                     tree-list))))))


(defmethod collate-part-level-enp ((obj timepoint-seqc-voice))
  (print (format nil "collate-part-level-enp >>> timepoint-seqc-voice ~A" obj))
  (let ((tps (list (timepoints obj)))
        (measures (or (measures obj)
                      (cdr-assoc :measures (params obj))))
        (modulus (modulus obj))
        (enp (flat (midics obj))))
    (let ((expr (scale-seqc-timepoints tps
                                       measures
                                       modulus
                                       :enp enp)))
      (print (format nil 
                     "collate-part-level-enp >>> timepoint-seqc-voice ~% tps ~A~% measures ~A~% modulus ~A~% enp ~A~% expr ~A" 
                     tps
                     measures
                     modulus
                     enp
                     expr))
      (setf (expr obj) (caar expr)))))
(defmethod collate-part-level-enp ((obj export1-timepoint-seqc))
  (let ((seqc-enps (collect-multi-enppar #'(lambda (x) (subtypep (type-of x) 'export1-timepoint-seqc-voice)) obj)))
    (mapcar #'collate-part-level-enp seqc-enps)))
(defmethod collate-part-level-enp ((obj export1-timepoint-seqc-voice))
  (print (format nil "collate-part-level-enp >>> export1-timepoint-seqc-voice ~A" obj))
  (concat-rec
   (mapcar #'(lambda (tvs mcs)
                      (make-instance 'om::voice
                                     :chords (cond ((or (null (remove nil (flat mcs)))
                                                        (every #'null (flat mcs)))
                                                    (make-sequence 'list (length (flat mcs)) :initial-element 60))
                                                   (t (om* 100 (remove nil (flat mcs)))))
                                     :tree (list (read-from-string "?") (list tvs))))
           (timepoints obj)
           (midics obj))))

(defmethod collate-part-level-enp ((obj timepoint-seqc-expansion))
  (collate-part-level-enp (proto obj)))

(defgeneric collect-multi-enppar (function enppar))

(defmethod collect-multi-enppar ((predicate function) (obj list))
  (remove nil (remove-duplicates (flat (funcall-rec #'(lambda (x) (collect-multi-enppar predicate x)) obj)))))

(defmethod collect-multi-enppar ((predicate function) (obj multi-enppar))
  (let (c)
    (labels
        ((collect-enppar (x)
           (if (and (not (find x c))
                    (funcall predicate x)) (push-to-end x c))
           (progn
             (mapcar #'collect-enppar (remove-if-not #'(lambda (y) (subtypep (type-of y) 't2l::multi-enppar)) (enps x)))
             (if (subtypep (type-of x) 'enppar-wrapper)
                 (collect-enppar (proto x))))))
      (collect-enppar obj)
      (remove nil c))))
  

(defgeneric fill-enppar-slots (enppar vars))

(defmethod fill-enppar-slots ((obj enppar) vars)
  (fill-slots obj vars))

(defmethod fill-enppar-slots ((obj export1-timepoint-seqc) vars)
  (let ((seqc-enps (collect-multi-enppar 
                    #'(lambda (x) 
                        (subtypep (type-of x) 'export1-timepoint-seqc-voice))
                    obj)))
    (print (format nil "fill-enppar-slots export1-timepoint-seqc: ~A vars: ~A seqc-enps: (~A)" obj vars (length seqc-enps)))
    (fill-slots obj vars)
    (mapcar 
     #'(lambda (o midics timepoints)
         (let ((vs nil))
           (setf (cdr-assoc :midics vs) midics)
           (setf (cdr-assoc :timepoints vs) timepoints)
           (fill-slots o vs)))
     seqc-enps
     (or (cdr-assoc :midics vars)
         (cdr-assoc :midicvars vars))
     (or (cdr-assoc :timepoints vars)
         (cdr-assoc :timevars vars)))))

(defmethod fill-enppar-slots ((obj timepoint-seqc-with-prules) vars)
  (let ((seqc-enps (collect-multi-enppar 
                    #'(lambda (x) 
                        (eq (type-of x) 'timepoint-seqc-voice-with-prules)) 
                    obj)))
    (mapcar 
     #'(lambda (o midics timepoints)
         (let ((vs nil))
           (setf (cdr-assoc :midics vs) (flat midics))
           (setf (cdr-assoc :timepoints vs) timepoints)
           (fill-slots o vs)))
     seqc-enps
     (or (cdr-assoc :midics vars)
         (cdr-assoc :midicvars vars))
     (or (cdr-assoc :timepoints vars)
         (cdr-assoc :timevars vars)))))

(defmethod fill-enppar-slots ((obj csptech-timepoint-seqc) vars)
  (setf (vars obj) vars)
  (fill-slots obj vars))

(defmethod fill-enppar-slots ((obj multipart-enppar-sequence) vars)
  (dotimes (i (length (enps obj)))
    (fill-enppar-slots (elt (enps obj) i)
                       (elt (cdr-assoc :vars vars) i)))
  (fill-slots obj vars))

(defmethod fill-enppar-slots ((obj enppar-wrapper) vars)
  (fill-enppar-slots (proto obj) vars)
  (fill-slots obj vars))

(defun enppar->timepoint-seqc-vars (obj)
  (print (format nil "enppar->timepoint-seqc-vars obj: ~A" obj))
  (cond ((null obj) nil)
        ((subtypep (type-of obj) 'timepoint-seqc-expansion)
         (enppar->timepoint-seqc-expansion-vars obj))
        ((subtypep (type-of obj) 'enppar-wrapper)
         (enppar->timepoint-seqc-vars (proto obj)))
        ((subtypep (type-of obj) 'timepoint-seqc-with-prules)
         (enppar->timepoint-seqc-with-prules-vars obj))
        ((subtypep (type-of obj) 'csptech-timepoint-seqc)
         (enppar->csptech-timepoint-seqc-vars obj))
        ((subtypep (type-of obj) 'export1-timepoint-seqc)
         (enppar->export1-timepoint-seqc-vars obj))
        ((subtypep (type-of obj) 'csptech-timepoint-seqc)
         (enppar->csptech-timepoint-seqc-vars obj))
        (t nil)))

(defun enppar->csptech-timepoint-seqc-vars (obj)
  (setf (measures obj) (or (measures obj) (cdr-assoc :measures (params obj))))
  (let ((seqc-enps (collect-multi-enppar #'(lambda (x) (subtypep (type-of x) 'csptech-timepoint-seqc-voice)) obj)))
    (let ((vars-list (enppar->csptech-timepoint-seqc-vars-call-cspvar-function-rec seqc-enps)))
      (let ((vars (list (cons :midics (mapcar
                                       #'(lambda (x) (cdr-assoc :midics x))
                                       vars-list))
                        (cons :timepoints (mapcar
                                           #'(lambda (x) (cdr-assoc :timepoints x))
                                           vars-list)))))
        (assert! (cond
                  ((null (csp-rules obj)) t)
                  ((listp (csp-rules obj)) (funcall (a-random-member-of (csp-rules obj)) vars))
                  (t (funcall (csp-rules obj) vars)))))
      vars-list)))

(defun enppar->multipart-enppar-sequence-vars-internal (enps)
  (cond
   ((null enps) nil)
   (t 
    (print (format nil "enppar->multipart-enppar-sequence-vars-internal ~A (~A)..." (car enps) (length (cdr enps))))
    (append (list 
             (enppar->timepoint-seqc-vars (car enps)))
            (enppar->multipart-enppar-sequence-vars-internal (cdr enps))))))

(defun enppar->multipart-enppar-sequence-vars (obj)
  (list (cons :vars (enppar->multipart-enppar-sequence-vars-internal (enps obj)))))

(defun enppar->csptech-timepoint-seqc-vars-call-cspvar-function-rec (seqc-enps)
  (cond
   ((null seqc-enps) nil)
   (t (append
       (list 
        (cond
         ((null (cspvar-function (car seqc-enps)))
          (list (cons :midics '(60))
                (cons :timepoints '(1))))
         (t 
          (funcall (cond ((listp (cspvar-function (car seqc-enps)))
                          (a-random-member-of (cspvar-function (car seqc-enps))))
                         (t (cspvar-function (car seqc-enps))))
                   (car seqc-enps)))))
       (enppar->csptech-timepoint-seqc-vars-call-cspvar-function-rec (cdr seqc-enps))))))
                 
(defun enppar->timepoint-seqc-with-prules-vars (obj)
  (setf (measures obj) (or (measures obj) (cdr-assoc :measures (params obj))))
    (let ((time-csp-predicates (time-csp-predicates obj))
          (midic-csp-predicates (midic-csp-predicates obj))
          (csp-rules (csp-rules obj)))
      (let ((seqc-enps (collect-multi-enppar #'(lambda (x) (subtypep (type-of x) 'timepoint-seqc-voice)) obj))
            (msbeats (ms-beat-count (measures obj) (modulus obj))))
        (let ((seqc-vars (enppar->timepoint-seqc-voice-csp seqc-enps)))
          (let ((mvs (mapcar
                      #'(lambda (x) (cdr-assoc :midics x))
                      seqc-vars))
                (tvs (mapcar
                      #'(lambda (x) (cdr-assoc :timepoints x))
                      seqc-vars)))
            (assert! (cond
                      ((null time-csp-predicates) t)
                      ((listp time-csp-predicates) (funcall (a-member-of time-csp-predicates) tvs))
                      (t (funcall time-csp-predicates tvs))))
            (let ((tvssoln (om-solution tvs *solution*)))
              (let ((mvsseqc (mapcar 
                              #'(lambda (xs ys) 
                                  (flat
                                   (mapcar
                                    #'(lambda (x y) (make-sequence 'list y :initial-element x))
                                    xs
                                    ys)))
                              (reverse mvs)
                              tvssoln)))
                (assert! (cond
                          ((null midic-csp-predicates) t)
                          ((listp midic-csp-predicates) (funcall (a-member-of midic-csp-predicates) mvsseqc))
                          (t (funcall midic-csp-predicates mvsseqc))))
                (list (cons :midics mvs)
                      (cons :timepoints tvssoln)
                      (cons :seqc-enps seqc-enps)))))))))

(defun enppar->timepoint-seqc-with-prules-vars-seqc-vars-rec (seqc-vars msbeats) ; 
  (if (>= *mess* 8) (print (format nil "enppar->timepoint-seqc-with-prules-vars-seqc-vars-rec seqc-vars (~A) msbeats ~A" (length seqc-vars) msbeats)))
  (cond
   ((null seqc-vars) nil)
   (t (append
       (let ((svs (car seqc-vars)))
         (let ((mv (cdr-assoc :midics svs))
               (tv (cdr-assoc :timepoints svs)))
           (let ((tvidx (an-integer-between 1 (length tv))))
             (let ((mvsubseq (subseq mv 0 tvidx))
                   (tvsubseq (subseq tv 0 tvidx)))
               (unless (>= (apply #'+ (om-solution tvsubseq *solution*)) msbeats) (fail))
               (list
                (list
                 (cons :midics mvsubseq)
                 (cons :timepoints tvsubseq)))))))
       (enppar->timepoint-seqc-with-prules-vars-seqc-vars-rec (cdr seqc-vars) msbeats)))))

(defun enppar->timepoint-seqc-voice-csp (obj)  
  (cond
   ((null obj) nil)
   ((listp obj)
    (if (>= *mess* 1) (print (format nil "enppar->timepoint-seqc-voice-csp (~A)..." (length (cdr obj)))))
    (append (list (enppar->timepoint-seqc-voice-csp (car obj)))
            (enppar->timepoint-seqc-voice-csp (cdr obj))))
   (t (enppar->timepoint-seqc-voice-csp-internal obj))))

(defun enppar->timepoint-seqc-voice-csp-internal (obj)
  (cond
   ((null obj) nil)
   ((subtypep (type-of obj) 'timepoint-seqc-voice-with-prules)
    (enppar->timepoint-seqc-voice-with-prules-vars obj))
   ((subtypep (type-of obj) 'csptech-timepoint-seqc-voice)
    (enppar->csptech-timepoint-seqc-voice-csp obj))
   (t nil)))

(defun enppar->csptech-timepoint-seqc-voice-csp (obj)
    (let ((csp-rules (csp-rules obj))
          (vars (funcall (cspvar-function e) (mergeassoc (params e) (list (cons :enppar e) (:seqc-enppar obj))))))
      (assert! (cond
                ((null csp-rules) t)
                ((listp csp-rules) (funcall (a-member-of csp-rules) vars))
                (t (funcall csp-rules vars))))
      (setf (cdr-assoc :enp vars) obj)
      vars))

(defun enppar->timepoint-seqc-voice-with-prules-vars (obj)
  "[midic|time|mapprules]-incomplete-productions"
  (setf (measures obj) (or (measures obj) (cdr-assoc :measures (params obj))))
  (let ((midicrules-input-process-increment 
         (or (cdr-assoc :midicrules-input-process-increment (params obj))
             (cdr-assoc :mapprules-input-process-increment (params obj))))
        (midicrules-nondeterministic-values-cap
         (or (cdr-assoc :midicrules-nondeterministic-values-cap (params obj))
             (cdr-assoc :mapprules-nondeterministic-values-cap (params obj))))
        (timerules-input-process-increment
         (or (cdr-assoc :timerules-input-process-increment (params obj))
             (cdr-assoc :mapprules-input-process-increment (params obj))))
        (timerules-nondeterministic-values-cap 
         (or (cdr-assoc :timerules-nondeterministic-values-cap (params obj))
             (cdr-assoc :mapprules-nondeterministic-values-cap (params obj))))
        (msbeats (ms-beat-count (measures obj) (modulus obj)))
        (time-min (let* ((nonterms (mapcar #'car (timerules obj)))
                         (min (apply #'min (remove-if #'(lambda (x) (position x nonterms)) (flat (timerules obj))))))
                    (* min (m* obj))))
        (timerules (timerules obj))
        (midicrules (midicrules obj))
        (midic-min (cdr-assoc :midic-min (params obj)))
        (midic-max (cdr-assoc :midic-max (params obj)))
        (midicrules-contains-nil-sym (position nil (flat (mapcar #'cdr (midicrules obj)))))
        (continuation? (and (cdr-assoc :continue (params obj))
                            (cdr-assoc :prev-enp (params obj))
                            (subtypep (type-of (cdr-assoc :prev-enp (params obj))) 'midic-enppar)
                            (midics (cdr-assoc :prev-enp (params obj))))))
    (let ((continue-midic-list (if continuation? (atomsv (flat (midics (cdr-assoc :prev-enp (params obj)))))))
          (continue-time-list (if continuation? (remove nil (flat (timepoints (cdr-assoc :prev-enp (params obj))))))))
      (assert (> msbeats 0))
      (let ((mapprules-input-length-cap (ceiling (/ msbeats time-min)))) ; max # of vars to fill measures
        (let ((timepoints (mapprules (if continuation? (1+ mapprules-input-length-cap) mapprules-input-length-cap)
                                     (funcall-rec #'(lambda (x) (if (numberp x) (* x (m* obj)) x)) timerules)
                                     :input-process-increment timerules-input-process-increment
                                     :continue (or (cdr-assoc :time-incomplete-productions (params obj))
                                                   (cdr-assoc :mapprules-incomplete-productions (params obj)))
                                     :listdxx nil
                                     :symbol-mode nil
                                     :ordered-partitions-nondeterministic-values-cap timerules-nondeterministic-values-cap)))
          (let ((midics 
                 (if midicrules-contains-nil-sym
                     (listdxxv2 
                      (mapprules (if continuation? (1+ mapprules-input-length-cap) mapprules-input-length-cap)
                                 midicrules
                                 :input-process-increment midicrules-input-process-increment
                                 :continue (or (cdr-assoc :midic-incomplete-productions (params obj))
                                               (cdr-assoc :mapprules-incomplete-productions (params obj)))
                                 :ordered-partitions-nondeterministic-values-cap midicrules-nondeterministic-values-cap
                                 :symbol-mode t))
                   (mapprules (if continuation? (1+ mapprules-input-length-cap) mapprules-input-length-cap)
                              midicrules
                              :input-process-increment midicrules-input-process-increment
                              :continue (or (cdr-assoc :midic-incomplete-productions (params obj))
                                            (cdr-assoc :mapprules-incomplete-productions (params obj)))
                              :listdxx t
                              :max midic-max
                              :min midic-min
                              :ordered-partitions-nondeterministic-values-cap midicrules-nondeterministic-values-cap
                              :symbol-mode nil))))
            (lprint 'midicrules-contains-nil-sym midicrules-contains-nil-sym continue-midic-list)
            (if midicrules-contains-nil-sym
                (let ((midic-atoms (atomsv midics)))
                  (dolist (x midic-atoms)
                    (lprint 'xxx x 'midic-min midic-min 'midic-max midic-max)
                    (assert! (>=v x midic-min))
                    (assert! (<=v x midic-max)))))
            (cond
             (continuation?
              (setf (midics obj) (cdr midics))
              (setf (timepoints obj) (cdr timepoints)))
             (t
              (setf (midics obj) midics)
              (setf (timepoints obj) (cdr timepoints))))
            (either
              (progn
                (unless continuation? (fail))
                (assert! (/=v (car midics) (car continue-midic-list)))
                (assert! (=v (car midics) (car (reverse continue-midic-list))))
                (assert! (=v (car timepoints) (car (reverse timepoints))))
                (list (cons :timepoints (cdr timepoints))
                      (cons :midics (cdr midics))))
              (progn
                (unless continuation? (fail))
                (assert! (=v (car midics) (car (reverse continue-midic-list))))
                (assert! (=v (car timepoints) (car (reverse timepoints))))
                (list (cons :timepoints (cdr timepoints))
                      (cons :midics (cdr midics))))
              (progn
                (unless continuation? (fail))
                (assert! (/=v (car midics) (car continue-midic-list)))
                (assert! (=v (car midics) (car (reverse continue-midic-list))))
                (assert! (=v (car timepoints) (car (reverse timepoints))))
                (list (cons :timepoints (cdr timepoints))
                      (cons :midics (cdr midics))))
              (progn
                (unless continuation? (fail))
                (assert! (=v (car midics) (car (reverse continue-midic-list))))
                (list (cons :timepoints (cdr timepoints))
                      (cons :midics (cdr midics))))
              (list (cons :timepoints timepoints)
                    (cons :midics midics)))))))))

(defun enppar->timepoint-seqc-expansion-vars-seqc-enp-expansion (seqc-enps midics-list timepoints-list expand-function)
  (if (>= *mess* 5) (print (format nil "enppar->timepoint-seqc-expansion-vars-seqc-enp-expansion seqc-enps (~A) midics-list (~A) timepoints-list (~A) expand-function (~A)" (length seqc-enps) (length midics-list) (length timepoints-list) expand-function)))
  (cond
   ((null seqc-enps) nil)
   (t (append
       (list
        (let ((exps (enppar->timepoint-seqc-expansion-vars-midic-expansion (car seqc-enps) (car midics-list) (car timepoints-list) 0 expand-function)))
          (let ((midics (mapcar #'car exps))
                (timepoints (mapcar #'cadr exps)))
            (list midics timepoints))))
       (enppar->timepoint-seqc-expansion-vars-seqc-enp-expansion (cdr seqc-enps) (cdr midics-list) (cdr timepoints-list) expand-function)))))

(defun assoc-list? (o) (and o (listp o) (every #'(lambda (x) (and (= (length x) 2) (atom (car x)))) o)))
(defun enppar->timepoint-seqc-expansion-expand-functions (o ts)
  (cond
   ((null o) nil)
   ((functionp o) o)
   ((assoc-list? o)
    (cond
     ((cdr-assoc ts o)
      (enppar->timepoint-seqc-expansion-expand-functions (cdr-assoc ts o) ts))
     (t
      (enppar->timepoint-seqc-expansion-expand-functions (cdr-assoc (caar (sort o #'< :key #'(lambda (x) (abs (- (car x) ts))))) o) ts))))
   ((listp o)
    (a-member-of o))
   (t o)))
(defun enppar->timepoint-seqc-expansion-vars-midic-expansion (enp midics timepoints i expand-function)
     (cond
      ((or  (null midics)(null timepoints)) nil)
      (t 
       (let ((part-index (or (cdr-assoc :index (params enp)) -1)))
         (let ((expand-function-params
                (list (cons :enppar enp)
                      (cons :index i)
                      (cons :midic-max (cdr-assoc :midic-max (params enp)))
                      (cons :midic-min (cdr-assoc :midic-min (params enp)))
                      (cons :midic (car midics))
                      (cons :midics midics)
                      (cons :midic-index i)
                      (cons :part-index part-index)
                      (cons :timepoint (car timepoints))
                      (cons :timepoints timepoints)
                      (cons :timepoint-index i))))
           (let ((expand-fn (if (find part-index (cdr-assoc :expand-function-index-list (params enp))) 
                                (enppar->timepoint-seqc-expansion-expand-functions expand-function (car timepoints)))))
             (if (>= *mess* 5) (print (format nil "enppar->timepoint-seqc-expansion-vars-midic-expansion expand-function: ~A expand-function-params: ~A >>> expand-function-index-list: ~A" expand-function expand-function-params (cdr-assoc :expand-function-index-list (params enp)))))
             (let ((expand-function-output (if expand-fn (apply expand-fn expand-function-params))))
               (if (>= *mess* 5) (print (format nil "enppar->timepoint-seqc-expansion-vars-midic-expansion expand-fn ~A expand-function-output ~A" expand-fn expand-function-output)))
               (append
                (list
                 (if (and expand-function-output
                          (cdr-assoc :expand-midic expand-function-output)
                          (cdr-assoc :expand-timepoint expand-function-output))
                     (list (cdr-assoc :expand-midic expand-function-output)
                           (cdr-assoc :expand-timepoint expand-function-output))
                   (list (car midics) (car timepoints))))
                (enppar->timepoint-seqc-expansion-vars-midic-expansion enp (cdr midics) (cdr timepoints) (1+ i) expand-function)))))))))
        
(defun enppar->timepoint-seqc-expansion-vars (obj)
  (assert (proto obj))
  (assert (subtypep (type-of (proto obj)) 'timepoint-seqc))
  (unless (expand-function obj)
    (setf (expand-function obj)
          #'(lambda (&rest params)
              nil)))
  (let ((vars (enppar->timepoint-seqc-vars (proto obj))))
    (if (>= *mess* 5) (print (format nil "enppar->timepoint-seqc-expansion-vars proto ~A vars ~A" (proto obj) vars)))
    (let ((seqc-enps (cdr-assoc :seqc-enps vars))
          (midics-list (cdr-assoc :midics vars))
          (timepoints-list (cdr-assoc :timepoints vars)))
      (let ((exps (enppar->timepoint-seqc-expansion-vars-seqc-enp-expansion seqc-enps midics-list timepoints-list (expand-function obj))))
          (list (cons :timepoints (mapcar #'cadr exps))
                (cons :midics (mapcar #'car exps)))))))






                
         













(defun enppar->export1-timepoint-seqc-vars (obj)
  (let ((measure-templates (or (cdr-assoc :measures-templates (params obj))
                               '((nil _) (1 _) (_ nil _))))
        (beat-templates (or (cdr-assoc :beat-templates (params obj))
                            '((_ _) (_ nil) (_ _ _ nil))))
        (max-process-levels (or (cdr-assoc :max-process-levels (params obj))
                                (cdr-assoc :levels (params obj))
                                1))
        (midicrules (cdr-assoc :midicrules (params obj)))
        (modulus (modulus obj))
        (ratio-divisibility-param (if (assoc :ratio-divisibility-param (params obj))
                                      (cdr-assoc :ratio-divisibility-param (params obj))
                                    '(4 3 5 7)))
        (ratio-minimum (if (assoc :ratio-minimum (params obj))
                           (cdr-assoc :ratio-minimum (params obj))
                         (/ 1 32)))
        (signatures (measures obj))
        (seqc-enps (collect-multi-enppar #'(lambda (x) 
                                             (subtypep (type-of x) 'export1-timepoint-seqc-voice))
                                         obj)))
    
    (let ((measure-list-structure (list-random-members-of signatures measure-templates)))
      (let ((beat-list-structures (one-value
                                   (export1-time-seqc-beat-list-structures seqc-enps
                                                                          measure-list-structure
                                                                          beat-templates
                                                                          max-process-levels))))
        (print (format nil "beatlist: ~A" beat-list-structures))
        (let ((midics (enppar->export1-timepoint-seqc-midics beat-list-structures seqc-enps midicrules))) 
          (let ((vars 
                 (mapcar 
                  #'(lambda (xs signature) 
                      (multiple-value-bind 
                          (trees ratios) 
                          (seqc->ms-vars xs 
                                         :msn-list (list (car signature))
                                         :msd-list (list (cadr signature)))
                        (list (cons :midics xs)
                              (cons :timepoints trees)
                              (cons :ratios ratios))))
                  (mat-trans midics)
                  (measures obj))))
            (let ((timeratios (mapcar #'(lambda (xs) (cdr-assoc :ratios xs)) vars))
                  (timepoints (mapcar #'(lambda (xs) (reduce #'append (mapcar #'cadr xs)))
                                      (mat-trans (mapcar #'(lambda (xs) (cdr-assoc :timepoints xs)) vars)))))
              (let ((csp-rule-input (list (cons :midics (mapcar #'flat1 midics))
                                          (cons :timepoints timepoints)
                                          (cons :timeratios timeratios)
                                          (cons :measures (measures obj))
                                          (cons :enp obj)
                                          (cons :NOTE "midics param is flattened once")
                                          (cons :beat-template beat-list-structures))))
                (assert! (cond ((null (csp-rules obj)) t)
                               (t (funcall (csp-rules obj) csp-rule-input))))
                (print (format nil "solution (~A)" (length (remove-if-not #'screamer::variable? (flat (list midics timepoints))))))
                (one-value (om-solution (list (cons :timepoints timepoints)
                                              (cons :midics midics))
                                        *solution*))))))))))
(defun export1-time-seqc-beat-list-structures (enp measure-list-structure beat-templates max-process-levels)
  (export1-time-seqc-beat-list-structures-internal enp measure-list-structure beat-templates max-process-levels))

(defun export1-time-seqc-beat-list-structures-internal (enp measure-list-structure beat-templates max-process-levels)
  (cond
   ((null enp) nil)
   (t
    (append
     (list
      (export1-time-seqc-beat-list-structure measure-list-structure 
                                             beat-templates
                                             (or (cdr-assoc :max-process-levels (params (car enp)))
                                                 max-process-levels)))
     (export1-time-seqc-beat-list-structures-internal (cdr enp)
                                                      measure-list-structure 
                                                      beat-templates
                                                      max-process-levels)))))

(defun export1-time-seqc-beat-list-structure (measure-list-structure beat-templates max-process-levels)
  (cond
   ((null measure-list-structure) nil)
   (t
    (append 
     (list      
      (export1-timelist-embedding-measure-beat (car measure-list-structure)
                                                 beat-templates
                                                 max-process-levels))
     (export1-time-seqc-beat-list-structure (cdr measure-list-structure)
                                            beat-templates
                                            max-process-levels)))))
(defun export1-timelist-embedding-measure-beat (template beat-templates max-process-levels)
  (cond
   ((null template) nil)
   (t
    (append
     (list
      (cond ((null (car template)) 
             (a-random-member-of (list 0 (a-random-member-of beat-templates))))
            (t (export1-timelist-beat-embedding (a-random-member-of beat-templates)
                                                 beat-templates
                                                 0
                                                 max-process-levels))))
     (export1-timelist-embedding-measure-beat (cdr template) beat-templates max-process-levels)))))
(defun export1-timelist-beat-embedding (template beat-templates level max-process-levels)
  (cond
   ((null template) nil)
   (t
    (append
     (list
      (cond ((or (null (car template))
                 (and (numberp (car template)) 
                      (< (car template) 0))
                 (= level max-process-levels))
             (car template))
            (t (export1-timelist-beat-embedding (random-list-e beat-templates)
                                                 beat-templates
                                                 (1+ level)
                                                 max-process-levels))))
     (export1-timelist-beat-embedding (cdr template)
                                      beat-templates
                                      level
                                      max-process-levels)))))
(defun enppar->export1-timepoint-seqc-midics (timeratio-list enp-list seqc-midicrules)
  (cond 
   ((or (null timeratio-list) (null enp-list)) nil)
   (t
    (append 
     (list
      (enppar->export1-timepoint-seqc-midic-fn0 (car timeratio-list)
                                                (or (cdr-assoc :midicrules (params (car enp-list)))
                                                    seqc-midicrules)
                                                nil
                                                (params (car enp-list))))
     (enppar->export1-timepoint-seqc-midics (cdr timeratio-list) (cdr enp-list) seqc-midicrules)))))



















(defun export1-map1st (input value)
  (cond
   ((null input) nil)
   ((atom (car input))
    (append (list value)
            (cdr input)))
   (t
    (append (list
             (export1-map1st (car input)
                     value))
            (cdr input)))))
(defun export1-ratios->template (ratios)
         ; _ 1 or nil
  (let ((rests-template
         (funcall-rec #'(lambda (x) nil) 
                      (funcall-rec #'(lambda (x) '_)
                                   ratios)
                      :level-max (if (> (llevels ratios) 2)
                                     2
                                   1))))
    (print (format nil "rests-template: ~A" rests-template))
    (funcall-rec #'(lambda (x)
                     (cond ((null x) nil)
                           ((atom x) x)
                           ((null (first-atom x)) x)
                           (t (export1-map1st x 1))))
                 rests-template
                 :level-min (if (> (llevels ratios) 2) 2 1)
                 :cons-mode t)))
(defun list-structure (x)
  (funcall-rec #'(lambda (y) (if (null y) nil 'x)) x))
(defun enppar->export1-timepoint-seqc-midic-fn0 (time-ratios midicrules &optional input-process-increment params)
  (print (format nil "enppar->export1-timepoint-seqc-midic-fn0~% time-ratios: ~A~% time-ratio-structure: ~A~% midicrules: ~A~% input-process-increment: ~A~%" time-ratios (funcall-rec #'(lambda (x) (cond ((null x) nil) (t '_))) time-ratios) midicrules input-process-increment))
  (let* ((midic-superset (if (or (assoc :midic-sieve params)
                                 (assoc :midic-superset params))
                             (or (cdr-assoc :midic-sieve params)
                                 (cdr-assoc :midic-superset params))
                           (cdr-assoc (a-random-member-of '(0 1 2 3 4)) *export1-ser0127-assoc*)))
         (template (funcall-rec #'(lambda (x) (cond ((and (numberp x) (= x 1)) 
                                                     (if midic-superset 
                                                         (a-member-ofv midic-superset)
                                                       (an-integer-betweenv 0 127)))
                                                     (t x)))
                               (export1-ratios->template time-ratios))))
    ; (print (format nil "set: ~A" midic-superset))
    (print (format nil "mapprules (1/2): ~A" template))
    (print (format nil "mapprules (1/2): (~A)" (length (remove-if-not #'screamer::variable? (flat template)))))
    (mapprules (remove-if-not #'screamer::variable? (flat template))
               midicrules
               :listdxx t
               :input-process-increment (or input-process-increment (if (> (length (remove-if-not #'screamer::variable? (flat template))) 9) 9)))
    (let ((vars (funcall-rec #'(lambda (x) (cond ((null x) nil)
                                                 ((screamer::variable? x) x)
                                                 (t 
                                                  (if midic-superset 
                                                      (a-member-ofv midic-superset)
                                                    (an-integer-betweenv 0 127)))))
                             template)))
      (let ((mflat (remove nil (flat vars))))
        (let ((midic-min (cdr-assoc :midic-min params))
              (midic-max (cdr-assoc :midic-max params))
              (csp-rule (cdr-assoc :csp-rule params))
              (csp-rule-input (mergeassoc (list (append (list :midics) vars))
                                          (cdr-assoc :csp-rule-input params))))
          (print (format nil "csp-rule: ~A csp-rule-input-keys: ~A" csp-rule (mapcar #'car csp-rule-input)))
          (let ((midic-start (or (cdr-assoc :midic-start params)
                                 (a-random-member-ofv
                                  (let ((domain
                                         (all-values
                                           (let ((var (an-integerv)))
                                             (assert! (andv (>=v var (or midic-min 0)) (<=v var (or midic-max 127))))
                                             (solution var (static-ordering #'linear-force))))))
                                    (if (every #'numberp domain) domain (arithm-ser 0 127 1)))))))
            (print (format nil "*************************************~%export1-timepoint-seqc-voice-vars~% midic-min: ~A~% midic-max: ~A~% midic-superset: ~A~%***************************************" midic-min midic-max midic-superset))
            (if (and csp-rule csp-rule-input)
                (let ((csp-rule-var (funcall csp-rule csp-rule-input)))
                  (print (format nil " assert: csp-rule: ~A " csp-rule-var))
                  (assert! csp-rule-var)))

            #|(if (and midic-superset midicrules)
                (progn
                  (print (format nil " assert: midic-superset param"))
                  (assert! (reduce #'andv (mapcar #'(lambda (x) (memberv x midic-superset)) mflat)))))|#
        
            (if (and mflat midic-start)
                (progn
                  (print (format nil " assert: midic-start param: ~A" midic-start))              
                  (assert! (=v (car mflat) midic-start))))

            (if (and mflat midic-min)
                (progn
                  (print (format nil " assert: midic-min param: ~A " midic-min))
                  (assert! (reduce #'andv (mapcar #'(lambda (x) (>=v x midic-min)) mflat)))))

            (if (and mflat midic-max)
                (progn
                  (print (format nil " assert: midic-max param: ~A" midic-max))
                  (assert! (reduce #'andv (mapcar #'(lambda (x) (<=v x midic-max)) mflat)))))

            (print (format nil "mapprules (2/2): ~A" (list-structure vars)))
            (if (= *mess* -33) (print (format nil " >> mapprules input vars: ~A" vars)))
            (funcall-rec #'(lambda (x)
                             (cond ((null x) 
                                    (print (format nil " >> x: ~A" x))
                                    nil)
                                   ((atom x) 
                                    (print (format nil " >> x: ~A" x))
                                    x)
                                   (t (let* ((vs (remove nil (flat x)))
                                             (chunksize (or input-process-increment
                                                            (if (> (length vs) 9) 9))))
                                        (print (format nil " >> (~A) input-process-increment: ~A mapprules input: ~A" (length vs) chunksize (list-structure x)))
                                        (mapprules vs
                                                   midicrules
                                                   :listdxx t
                                                   :input-process-increment chunksize)))))
                         vars
                         :level-min (if (> (llevels time-ratios) 2) 2 1)
                         :cons-mode t)
        
            (print (format nil " done: ~A" (list-structure vars)))
            vars))))))
      
(defun enppar->export1-timepoint-seqc-midic-fn1 (time-ratios list)
  (mapcar #'(lambda (x) (a-random-member-ofv (append (list nil) (om:arithm-ser 0 127 1)))) (flat time-ratios)))


(defun enppar->csptech-timepoint-seqc-vars (obj)
  (setf (vars obj) (funcall (cspvar-function obj) obj)))

(defgeneric process-enp (enppar))

(defmethod process-enp ((obj enppar)) obj)

(defmethod process-enp :before ((obj multi-enppar))
  (unless (cdr-assoc :do-not-propagate-parameters (params obj))
    (dolist (x (enps obj))
      (setf (params x) (mergeassoc (params x) (params obj) t)))))

(defvar *enppar-random-solution-count* 0)
(setf *enppar-random-solution-count* 16)

(defun om-process-enp-multipart-enppar-sequence (obj)
  (process-enp-multipart-enppar-sequence (progn 
                                           (dolist (e (enps obj)) (process-enp e))
                                           obj)))

(defun process-enp-multipart-enppar-sequence (obj)
  (let ((soln (ith-value (or (cdr-assoc :solution-index (params obj)) 0)
                         (om-solution (let ((vars (enppar->multipart-enppar-sequence-vars obj)))                                     
                                        (print (format nil "screamer::solution (~A)..." (length (remove-if-not #'screamer::variable? (flat vars)))))
                                        vars)
                                      *solution*))))
    (if (>= *mess* 1) (print (format nil "solution: ~A" soln)))
    (global 
      (fill-enppar-slots obj soln)                     
      (let ((ps (collate-part-level-enp obj)))
        (cond
         ((eq *syntax* :om) 
          (setf (expr obj) ps))
         (t
          (dotimes (i (length ps))
            (let (p)
              (dotimes (j (length (cdr-assoc i ps)))
                (push-to-end (cdr-assoc j (cdr-assoc i ps)) p))
              (push-to-end p (expr obj)))))
         obj)))))

(defmethod process-enp ((obj multipart-enppar-sequence))
  (global
    (dolist (x (enps obj)) (process-enp x)))
  (process-enp-multipart-enppar-sequence obj)
  obj)

(defmethod process-enp :before ((obj enppar-sequence))
  ;(print (format nil "process-enp before enppar-sequence ~A" obj))
  (dotimes (i (length (enps obj)))
    (unless (= i 0)
      (setf (cdr-assoc :prev-enp (params (elt (enps obj) i)))
            (elt (enps obj) (1- i))))
    (unless (= i (1- (length (enps obj))))
      (setf (cdr-assoc :next-enp (params (elt (enps obj) i)))
            (elt (enps obj) (1+ i)))))
  obj)

(defmethod process-enp :before ((obj enppar-wrapper))
  ;(print (format nil "process-enp before enppar-wrapper ~A params ~A" obj (params obj)))
  (unless (cdr-assoc :do-not-propagate-parameters (params obj))
    (setf (params (proto obj)) (mergeassoc (params (proto obj)) (params obj) t))))

(defmethod process-enp ((obj enppar-wrapper))
  (process-enp (proto obj))
  obj)

(defmethod process-enp :before ((obj midic-enppar))
  ;(print (format nil "process-enp before midic-enppar on ~A" obj))
  (let ((midic-enps (collect-multi-enppar #'(lambda (x) (subtypep (type-of x) 't2l::midic-enppar)) (enps obj)))) 
    (unless (cdr-assoc :measures (params obj))
      (if (some #'(lambda (x) (cdr-assoc :measures (params x))) midic-enps)
          (setf (cdr-assoc :measures (params obj))
                (some #'(lambda (x) (cdr-assoc :measures (params x))) midic-enps))))    
    (if (measures obj)
        (dolist (voice midic-enps)
          (setf (cdr-assoc :measures (params voice)) (measures obj))
          (setf (measures voice) (measures obj))))
  obj))

(defmethod process-enp :before ((obj timepoint-seqc))
  ;(print (format nil "process-enp::timepoint-seqc before ~A (params ~A )" obj (params obj)))
  (dolist (e (collect-multi-enppar #'(lambda (x) (subtypep (type-of x) 't2l::timepoint-seqc-voice)) obj))
    (setf (modulus e) (modulus obj)))
  (if (cdr-assoc :prev-enp (params obj))
      (progn
        (let ((enps (collect-multi-enppar #'(lambda (x) (subtypep (type-of x) 'timepoint-seqc-voice)) obj))
              (prev-enps (collect-multi-enppar #'(lambda (x) (subtypep (type-of x) 't2l::timepoint-seqc-voice)) (cdr-assoc :prev-enp (params obj)))))
          (mapcar
           #'(lambda (p n) (setf (cdr-assoc :prev-enp (params n)) p))
           prev-enps
           enps)))))

(defmethod process-enp :before ((obj timepoint-seqc-expansion))
  (dolist (e (collect-multi-enppar #'(lambda (x) t) obj))
    (setf (cdr-assoc :expand-function-index-list (params e)) (cdr-assoc :expand-function-index-list (params obj)))))
