(in-package :t2l)

(define-box ms-template->vars (k &key (msn-list (12 11 10 9 8 7 6 5 4 3 2)) 
                                      (msd-list (2 4 8 16))
                                      (group-var-domain (1 2 3 4 5 7 8))
                                      (note-var-domain (16 15 14 13 12 11 10 9 8 7 6 5 4 3 2 1))
                                      (rest-var-domain (-16 -15 -14 -13 -12 -11 -10 -9 -8 -7 -6 -5 -4 -3 -2 -1))
                                      randomize-group-domains
                                      randomize-pulse-domains
                                      min
                                      max)
  :indoc '("" "" "" "" "" "" "")
  :icon 225
  :doc ""
  :numouts 2
  (if (>= *mess* 20) (print (format nil "k: ~A msn-list: ~A" k msn-list)))
    (labels ((msn-or-msd-sym? (s)
               (cond ((null s) nil)
                     (t (let* ((sym (write-to-string s))
                               (ssq (subseq sym 0 3)))
                          (and (> (length sym) 2) 
                               (string= "?ms" ssq))))))
             (group-sym? (s)
               (cond ((null s) nil)
                     (t (let ((sym (write-to-string s)))
                          (and (> (length sym) 1)
                               (string= "?G" (subseq sym 0 2)))))))
             (rest-sym? (sym)
               (let ((sym-string (subseq (write-to-string sym) 0 2)))
                 (or (string= "?t" sym-string)
                     (string= "?T" sym-string))))
             (rest-sym->var (sym)
               (let ((sym-string (write-to-string sym)))
                 (read-from-string 
                  (concatenate 'string "?" (subseq sym-string 2 (length sym-string))))))
             (apply-numeric-constraints (l)
               (let ((syms (mapcar #'car l)))
                 (if (>= *mess* 45) (lprint 'syms syms))
                 (mapcar #'(lambda (x)
                             (let ((var (system:cdr-assoc x l)))
                               (cond ;((null x) nil)
                                ((null var) x)
                                ((and (screamer::variable? var)
                                      (not (msn-or-msd-sym? x)))
                                 (assert! (integerpv var))
                                 (cond ((group-sym? x)
                                        (if (>= *mess* 45) (lprint 'group-sym->var x))
                                        (assert! (memberv var (if randomize-group-domains
                                                                  (permut-random group-var-domain)
                                                                group-var-domain))))
                                       ((rest-sym? x) (assert! (memberv var 
                                                                        (if randomize-pulse-domains
                                                                            (permut-random rest-var-domain)
                                                                          rest-var-domain))))
                                       (t (assert! (memberv var (if randomize-pulse-domains
                                                                    (permut-random note-var-domain)
                                                                  note-var-domain)))))
                                 (if (= *mess* -0.5) (print (format nil "ms-template->vars sym: ~A var: ~A" x var)))
                                 var)
                                (t x))))
                         syms))))
      (multiple-value-bind (temp vars) (template k)
        (if (>= *mess* 45)
            (mapcar #'(lambda (x) (lprint x 'msn-or-msd-sym? (msn-or-msd-sym? x))) (mapcar #'car vars)))
        (let ((msn (system:cdr-assoc (read-from-string "?msn") vars))
              (msd (system:cdr-assoc (read-from-string "?msd") vars)))
          (if (>= *mess* 45) 
              (progn
                (print (list 'ms-template->vars 'msn msn 'cdr-assoc-msn (system:cdr-assoc '?msn vars) 'vars vars 'msd msd))
                (mapcar #'(lambda (x)
                            (print (list 'key x (type-of x) 'assoc (assoc x vars))))
                        (mapcar #'car vars))
                (print (list 'another 'try '?msn (assoc '?msn vars)))))
          (assert! (integerpv msn))
          (assert! (integerpv msd))
          (assert! (memberv msn msn-list))
          (assert! (memberv msd msd-list))
          (apply-numeric-constraints vars)
          (let* ((trees (mapcar (lambda (x) 
                                  (cond ((and (stringp (car x))
                                              (string= (car x) "?"))
                                         (setf (car x) (read-from-string "?"))
                                         x)
                                        (t x)))
                                temp))
                 (ratios (mapcar #'(lambda (x)
                                     (mapcar #'(lambda (y) (ms-vars->ratios y))
                                             (cadr x)))
                                 trees)))          
            (if (or min max)
                (let ((ratio-list (flat ratios)))
                  (if min (assert! (apply #'andv (mapcar #'(lambda (x) (>=v x min)) ratio-list))))
                  (if max (assert! (apply #'andv (mapcar #'(lambda (x) (<=v x max)) ratio-list))))))          
            (values trees ratios))))))

(defun timelist->ratios (list modulus)
  (labels
      ((duration (x)
         (cond
          ((null x) 0)
          ((listp x) (car x))
          (t x)))
       (beats->ratios (scale input)
         (let ((s (apply #'+v (mapcar #'duration input))))
           (mapcar
            #'(lambda (x)
                (cond
                 ((listp x) (beats->ratios (*v (/v (car x) s) scale)
                                           (cadr x)))
                 (t (*v (/v x s) scale))))
            input))))
    (let ((timelist (funcall-rec #'(lambda (x)
                                     (let ((var (a-realv)))
                                       (assert! (=v var x))
                                       var))
                                 list))
          (scale (*v (/v 1 (car modulus))
                       (/v (cadadr modulus) (caadr modulus)))))
      (mapcar
       #'(lambda (x)
           (cond
            ((listp x) (beats->ratios (*v scale (car x))
                                      (cadr x)))
            (t (*v scale x))))
       timelist))))

(defun timelist->group-totals (timelist)
  "((2 ((1 (1 4)) 2)))"
  (labels
      ((duration (x)
         (cond
          ((null x) 0)
          ((listp x) (car x))
          (t x)))
       (group? (input)
         (and (listp input)
              (= (length input) 2)
              (atom (car input))
              (listp (cadr input))))
       (group->total (input)
         nil))
    nil))

(defun om-expr->score-enp (list &key midic-list)
  (mapcar #'(lambda (om midics)
              (om-expr->part-enp om :midic-list midics))
          list
          midic-list))
; ((? (((4 4) (1 1 1 1))))
;  (? (((4 4) ((1 (1 1 1)) 1 1 1)))))
;    =>  (((((1 ((2 :notes (60))))
;            (1 ((1 :notes (60))))
;            (1 ((1 :notes (60))))
;            (1 ((1 :notes (60))))
;            :time-signature (4 4)) ...
(defun om-expr->part-enp (list &key midic-list)
  (let ((prev-midics nil)
        (next-midics (copy-seq midic-list)))
    (labels
        ((om-pulse->group (input)
           (cond ((null input) '(1 (-1)))
                 ((floatp input) (list (floor input) '(1.0)))
                 ((listp input) input)
                 ((< input 0) (list (floor (abs input)) '(-1)))
                 (t (list input '(1)))))
         (om-measure->enp (input)
           (append
            (mapcar #'om-beat->enp (print (mapcar #'om-pulse->group (cadr input))))
            (list :time-signature (car input))))
         (om-beat->enp (input)
           (cond 
            ((null input) -1)
            ((floatp input) input)
            ((listp input) (om-group->enp input))
            ((< input 0) input)
            (t
             (cond 
              (next-midics
               (push (pop next-midics) prev-midics)
               (list input :NOTES (cond 
                                   ((listp (car prev-midics)) (car prev-midics))
                                   (t (list (car prev-midics))))))
              (t (* -1 (floor (abs input))))))))
         (om-group->enp (input)
           (list (car input)
                 (mapcar #'om-beat->enp (cadr input)))))
      (list (mapcar #'om-measure->enp (cadr list))))))


(defun om-expr->enp (list &key midic-list)
  (let ((last-midics nil)
        (next-midics (copy-seq midic-list)))
    (labels
        ((om->enp-ms (input)
           (if (>= *mess* 5) (print (format nil "om->enp-ms input: ~A" input)))
           (append
            (list (caar input) (list (mapcar #'om->enp-beat (cadr input))))
            (list :time-signature (car input))))
         (om->enp-beat (input)
           (assert input)
           (if (>= *mess* 5) (print (format nil "om->enp-beat input: ~A" input)))
           (cond
            ((listp input)
             (list (car input)
                   (mapcar #'om->enp-beat (cadr input))))
            (t
             (cond
              (next-midics
               (push (pop next-midics) last-midics)
               (cond
                ((car last-midics)
                 (list input :notes (list (car last-midics))))
                (t
                 (*v -1 (absv input)))))
              (t
               (if (>= *mess* 5) (print (format nil "om->enp-beat rest input: ~A" input)))
               (*v -1 (abs-v input)))))))
         (om->enp-group (input)
           nil))
      (mapcar #'om->enp-ms (cadr list)))))

(define-box seqc->ms-vars (k &key (msn-list (12 11 10 9 8 7 6 5 4 3 2)) 
                                  (msd-list (2 4 8 16))
                                  (group-var-domain (1 2 3 4 5 7 8))
                                  (note-var-domain (16 15 14 13 12 11 10 9 8 7 6 5 4 3 2 1))
                                  (rest-var-domain (-16 -15 -14 -13 -12 -11 -10 -9 -8 -7 -6 -5 -4 -3 -2 -1))
                                  randomize-group-domains
                                  randomize-pulse-domains
                                  min
                                  max)
  :indoc '("" "" "" "" "" "" "")
  :icon 225
  :doc ""  
  :numouts 2
  (multiple-value-bind 
      (trees ratios) 
      (ms-template->vars (init-ms-template k) 
                         :msn-list msn-list
                         :msd-list msd-list
                         :group-var-domain group-var-domain
                         :note-var-domain note-var-domain
                         :rest-var-domain rest-var-domain
                         :randomize-group-domains randomize-group-domains
                         :randomize-pulse-domains randomize-pulse-domains
                         :min min
                         :max max)
    (values trees ratios)))

(define-box get-signaturesv (tree )
  :indoc '("")
  :icon 160
  :doc ""  
  (labels ((seqc? (input) 
             (and (listp input)
                  (every #'(lambda (x) (string= "?" (write-to-string x)))
                         (mapcar #'car input))))
           (tree? (input)
             (and (listp input)
                  (string= "?" (write-to-string (car input))))))
    (cond ((seqc? tree) 
           (mapcar #'(lambda (x)
                       (mapcar #'car x))
                   (mapcar #'cadr tree)))
          (t (mapcar #'car tree)))))






;;;;
(defclass timepoint () 
  ((midic :accessor midic
          :initarg :midic
          :initform nil)
   (value :accessor value
          :initarg :value
          :initform nil)))


(defun pcset-cartx2 (x y)
  (sort 
   (remove-duplicates (mapcar #'(lambda (z) (mod (apply #'+ z) 12)) (cartx x y))
                      :test #'=)
   #'<))

(defun list+mod12v (list value)
  (mapcar #'(lambda (x) (modv (+v x value) 12)) list))

(defun mode->midic0127v (mode)
  (flat
   (mapcar #'(lambda (x) (mapcar #'(lambda (y) (+v y x)) mode))
          '(0 12 24 36 48 60 72 84 96 108 120))))
     
(defun pcset-reorder (s r) 
  (let ((a (make-variable))
        (b (make-variable))) 
    (prolog-map2 #'(lambda (x y) (assert! (equalv y (modv x 12)))) s a)
    (prolog-quicksort a b)
    (either 
      (pcset-reorder-internal b r)    
      (let ((c (make-variable))
            (d (make-variable))
            (e (make-variable))
            (h (make-variable))) 
        (prolog-reverse b c)
        (prolog-nth 0 c h d)
        (prolog-map2 #'(lambda (x y) (assert! (equalv y (integer-absv (-v h x))))) c e)
        (pcset-reorder-internal e r)))))

(defun pcset-reorder-internal (b r) 
  (let ((c (make-variable))
        (d (make-variable))
        (h (make-variable)))
    (prolog-rotate b c)
    (prolog-nth 0 c h d)
    (prolog-map2 #'(lambda (x y) (assert! (equalv y (modv (+v (-v x h) 12) 12)))) c r)))

(defun list->seqc-vars (seqc &key templates (permut-max 32))
  (cond (templates 
         (mapcar #'(lambda (x) (make-template-expansion-vars x templates))
                 seqc))
        (t 
         (mapcar #'(lambda (x) 
                     (funcall-rec (lambda (y) 
                                    (cond ((screamer::variable? y) y)
                                          ((numberp y) (make-intv= y))
                                          (t (an-integer-betweenv 1 127))))
                                  x))
                 seqc))))

(defun midic->pcsetv (ch s &key unsorted xpos-invs-only)
  (let ((ch-deref (mapcar #'(lambda (x) (funcallv #'mod x 12)) ch)))
    (let ((ch-xpos (mapcar #'(lambda (x) (-v x (car ch-deref))) ch-deref)))
      (let ((ch-m12 (mapcar #'an-integerv (make-sequence 'list (length ch-xpos))))
            (n-sequence (arithm-ser 0 (1- (length ch-deref)) 1)))
        (prolog-quicksort ch-xpos ch-m12)
        (let ((a (mapcar #'(lambda (n) (rotate ch-m12 n)) n-sequence)))
          (assert! (equalv s (mapcar #'(lambda (n) (mapcar #'(lambda (x) (funcallv #'mod x 12)) (mapcar #'(lambda (y) (+v y 12)) (mapcar #'(lambda (z) (-v z (car n))) n)))) a))))))))

(defun midic->pcsetv2 (ch s &key unsorted xpos-invs-only)     
 (let ((a (mapcar #'(lambda (x) (an-integerv)) ch))
       (b (mapcar #'(lambda (x) (an-integerv)) ch))
       (c (mapcar #'(lambda (x) (an-integerv)) ch))
       (d (mapcar #'(lambda (x) (an-integerv)) ch))
       (e (mapcar #'(lambda (x) (an-integerv)) ch))
       (f (an-integerv))
       (n (arithm-ser 0 (1- (length ch)) 1)))
   (let* ((g (mapcar #'(lambda (x) (mapcar #'(lambda (y) (an-integerv)) ch)) n))
          (h (funcall-rec #'(lambda (x) (an-integerv)) g)))
     (if (>= *mess* 30) (print (list 'midic->pcsetv2 'ch ch 's s)))
     (prolog-maplist3a #'(lambda (x) (mod x 12)) ch a)
     (if (>= *mess* 30) (print (list 'midic->pcsetv2 10 'ch ch 's s)))
     (prolog-quicksort a b)
     (if (>= *mess* 30) (print (list 'midic->pcsetv2 20 'ch ch 'b b (mapcar #'type-of b))))
     (prolog-maplist3a #'(lambda (x) (rotate b x)) n g)
     (prolog-maplist3 #'(lambda (x)
                          (let ((y (an-integer)))
                            (prolog-first x y)
                            (list-v x y)))
                      g
                      s))))
     ;(if (>= *mess* 30) (print (list 'midic->pcsetv2-prcs-ch 10 'ch ch 's s))))))

(defun midic->pcsetv2-prcs-ch-inversion (ch s n)
  (if (>= *mess* 30) (print (list 'midic->pcsetv2-prcs-ch-inversion 10 'ch ch 's s)))
  (let ((a (mapcar #'(lambda (x) (an-integerv)) ch)))
    (prolog-rotate-list ch a n)
    (midic->pcsetv2-prcs-ch a s)))

(defun midic->pcsetv2-prcs-ch (ch s)
  (let ((a (mapcar #'(lambda (x) (an-integerv)) ch))
        (b (mapcar #'(lambda (x) (an-integerv)) ch))
        (f (an-integerv)))
    (prolog-first ch f)
    (if (>= *mess* 30) (print (list 'midic->pcsetv2-prcs-ch 10 'ch ch 's s)))
    (prolog-maplist3a #'(lambda (x) (-v x f)) ch a)
    (if (>= *mess* 30) (print (list 'midic->pcsetv2-prcs-ch 20 'ch ch 's s)))
    (prolog-maplist3a #'(lambda (x) (+v x 12)) a b)
    (if (>= *mess* 30) (print (list 'midic->pcsetv2-prcs-ch 30 'ch ch 's s)))
    (prolog-maplist3a #'(lambda (x) (funcallv #'mod x 12)) b s)))

(defun pcsets-equalv (ch1 ch2 s)
  (let ((sets1 (make-variable))
        (sets2 (make-variable))
        (isect (make-variable)))
    (midic->pcsetv ch1 sets1)
    (midic->pcsetv ch2 sets2)
    (prolog-intersect sets1 sets2 isect)
    (if (>= *mess* 30) (print (list 'pcsets-equalv 'sets1 sets1 'sets2 sets2 'isect isect)))
    (assert! (equalv s (notv (equalv nil isect))))))    

(define-box om-jjf-1st-species1 ((cf list) &optional (i 0) mode)
  :initvals '((62 65 67 65 64 62) 0)
  :indoc '("" "" )
  :icon 150 
  :doc "" 
  (ith-value i 
             (solution (jjf-1st-species1 cf mode)
                       (static-ordering #'linear-force))
   'fail))

(defun templates->paths (templates &key (test #'list-eq))
  (sequences->paths (mapcar #'flat templates) :test test))

(defun seqcit (fn l)
  (either
    (let ((a (make-variable))
          (b (make-variable)))
      (prolog-maplist3 #'lengthv l a)
      (assert! (<=v (list-maxv a) 1))
      (prolog-maplist3 (lambda (x) (funcallv #'car x)) l b)
      (funcall-nondeterministic fn b))
    (let ((a (make-variable))
          (b (make-variable))
          (c (make-variable))
          (d (make-variable)))
      (pad-listsv l a)
      (prolog-lists-firsts-rests a b c)
      (cons-atomsv b d)
      (seqcit fn d)
      (seqcit fn c))))

(defun map-seqc (fn l)
  ;(print (list 'map-seqc 'l l))
  (cond ((null l) nil)
        ((and (= (list-max (mapcar #'length (atoms2list l))) 1)
              (not (contains-list (mapcar #'car l))))
         (funcall-nondeterministic fn l)
         l)
        (t (let ((m (apply #'pad-lists (atoms2list l))))
             (mapcar #'append
                     (map-seqc fn (atoms2list (mapcar #'car m)))
                     (map-seqc fn (mapcar #'cdr m)))))))

(defun map-seqcv (fn l l1)
  (either
    (let ((a (make-variable))
          (b (make-variable)))
      (prolog-maplist3 #'lengthv l a)
      (assert! (<=v (list-maxv a) 1))
      (prolog-maplist3 (lambda (x) (funcallv #'car x)) l b)
      (assert! (equalv l1 (funcall-nondeterministic fn b))))
    (let ((a (make-variable))
          (b (make-variable))
          (c (make-variable))
          (d (make-variable))
          (c1 (make-variable))
          (d1 (make-variable))
          (e (make-variable)))
      (pad-listsv l a)
      (prolog-lists-firsts-rests a b c)
      (cons-atomsv b d)
      (map-seqcv fn d d1)
      (map-seqcv fn c c1)
      (cons-atomsv e d1)
      (prolog-lists-firsts-rests l1 e c1))))

(define-box format-mssign ((ms list))
  :initvals '((5 8))    ; init 
  :indoc '("") ; an string list with short docs
  :icon 410  ; the icon
  :doc "" 
  (read-from-string (concatenate 'string (write-to-string (car ms)) "//" (write-to-string (cadr ms)))))


(defstruct timee value flag)
(defun process-duration-groups-internal (l segs)
  (labels
      ((strip-zeros (l)
         (mapcar (lambda (x) (remove 0 x :test 'list-eq)) l))
       (duration (group)
         (cond ((null group) 0)
               ((and (listp group)
                     (> (length group) 1)
                     (atom (car group))
                     (listp (cadr group)))
                (abs (car group)))
               ((listp group) 0)
               (t (abs group))))
       (group-value (group)
         (cond ((null group) 0)
               ((and (listp group)
                     (> (length group) 1)
                     (atom (car group))
                     (listp (cadr group)))
                (car group))
               ((listp group) 0)
               (t group)))
       (process-durations (ll segs-l segs-r)
         (cond ((and (null ll)
                     (not (null segs-l))
                     (null segs-r)) 
                (list segs-l))
               ((null ll) nil)
               ((and (null segs-l)
                     (null segs-r)) 
                (process-durations ll (list (* (float -1) (float (car ll)))) nil))
               ((null segs-l) 
                (process-durations ll (list (car segs-r)) (cdr segs-r)))
               ((and (null segs-r)
                     (< (reduce #'+ (mapcar #'duration segs-l))
                        (car ll)))
                (let* ((segs-l-duration (reduce #'+ (mapcar #'duration segs-l)))
                       (diff (float (* -1 (- (car ll) segs-l-duration)))))
                  (if (>= *mess* 15)
                      (print (format nil 
                                     "process-durations add rest syms ll ~A segs-l ~A segs-l-duration ~A diff ~A"
                                     ll
                                     segs-l
                                     segs-l-duration
                                     diff)))
                  (process-durations ll ;(cdr ll)                             
                          (append segs-l (list diff))
                          nil)))
               (t 
                (let ((diff (- (duration (car ll)) (reduce #'+ (mapcar #'duration segs-l)))))
                  (cond ((> diff 0)
                         (process-durations ll 
                                 (append segs-l (list (car segs-r)))
                                 (cdr segs-r)))
                        ((< diff 0)
                         (cond
                          ((listp (car (reverse segs-l)))
                           (let* ((a (duration (car (reverse segs-l))))
                                  (c (/ (* -1 diff) a))
                                  (b (/ (+ (duration (car (reverse segs-l))) diff) a))
                                  (s (reduce #'+ (mapcar #'duration (cadar (reverse segs-l)))))
                                  (d (* b s))
                                  (e (* c s))
                                  (groups (process-duration-groups-internal (list d e) (cadar (reverse segs-l)))))
                             (if (>= *mess* 5) (print (format nil "a ~A b ~A c ~A s ~A d ~A e ~A groups ~A s-group ~A last-durations ~A" a b c s d e groups (cadar (reverse segs-l)) (mapcar #'duration (cadar (reverse segs-l))))))
                             (let* ((stak (copy-seq (cadar (reverse segs-l))))
                                    (groups2
                                     (funcall-rec
                                      #'(lambda (x)
                                          (let ((top (pop stak)))
                                            (cond ((floatp top) (float x))
                                                  (t x))))
                                      groups)))
                               (process-durations ll 
                                       (append (butlast segs-l) 
                                               (list (list (+ (duration (car (reverse segs-l))) diff)
                                                           (car groups2))))
                                       (append (list (list (* -1 diff) (cadr groups2)))
                                             segs-r)))))
                          (t (process-durations ll
                                     (append (butlast segs-l)
                                             (list (+ (duration (car (reverse segs-l))) diff)))
                                     (append (list (* -1 diff))
                                             segs-r)))))
                        (t
                         (let ((r (append (list segs-l)
                                          (process-durations (cdr ll)
                                                  nil
                                                  segs-r))))
                           r)))))))
       (process-durations->timees (ps)
         (mapcar ; ((1 2) (1 1))
          #'(lambda (m)
              (mapcar
               #'(lambda (n)
                   (make-timee :value n :flag t))
               m))
          ps))
       (process-timee-flags (re)
         (let ((init-s (funcall-rec #'abs segs)) ; 
               (init-p (flat re)))
           (labels ((prcs (s p &optional (cont nil))
                      (cond ((and (null s) (null p)) nil)
                            ((null p) nil)
                            ((null s) (mapcar #'(lambda (x) 
                                                  (setf (timee-flag x) nil))
                                              p))
                            (t (let ((diff (- (duration (car s))
                                              (duration (timee-value (car p))))))
                                 (cond ((> diff 0)
                                        (progn 
                                          (setf (timee-flag (car p)) (not cont))
                                          (prcs (append (list diff) (cdr s))
                                                (cdr p)
                                                t)))
                                       ((< diff 0)
                                        (progn 
                                          (setf (timee-flag (car p)) (not cont))
                                          (prcs (cdr s)
                                                (cdr p))))
                                       (t
                                        (progn 
                                          (setf (timee-flag (car p)) (not cont))
                                          (prcs (cdr s) (cdr p))))))))))
             (prcs init-s init-p))))
       (setf-timee-value (x d)
         (cond ((listp (timee-value x))
                (setf (car (timee-value x)) d))
               (t 
                (setf (timee-value x) d))))
       (expand-timee-groups (re)
         (mapcar #'(lambda (xs)
                     (let ((x-values (scale-to-int (mapcar #'group-value (mapcar #'timee-value xs)))))
                       (if (>= *mess* 20) (print (format nil "expand-timee-groups x-values ~A xs ~A"
                                                         x-values
                                                         (mapcar #'timee-value xs))))
                       (mapcar (lambda (a b)
                                 (progn 
                                   (setf-timee-value a b)
                                   a))
                               xs
                               x-values)))
                 re))
       (convert-timee-values (re) 
         (mapcar #'(lambda (x)
                     (setf-timee-value x (if (timee-flag x)
                                               (floor (group-value (timee-value x)))
                                             (float (group-value (timee-value x))))))
                 (flat re)))
       (process-timee-signs (re segs)
         (let ((stak (copy-seq segs)))
           (push nil stak)
           (if (>= *mess* 15) (print (format nil "process-timee-signs stak: ~A" stak)))
           (mapcar (lambda (x)
                     (if (timee-flag x) (pop stak))
                     (cond ((null stak) x)
                           ((and (< (group-value (car stak)) 0)
                                 (> (timee-value x) 0))
                            (if (>= *mess* 15) (print (format nil "converting 1- value ~A" (car stak))))
                            (setf (timee-value x) (* -1 (timee-value x)))
                                  
                            x)
                           (t x)))
                   (flat re)))))
    (let* ((durations (strip-zeros (process-durations l nil segs)))
           (duration-obj-list (process-durations->timees durations)))
      (process-timee-flags duration-obj-list)
      (expand-timee-groups duration-obj-list)
      (process-timee-signs duration-obj-list segs)
      (convert-timee-values duration-obj-list)
      (funcall-rec #'timee-value duration-obj-list))))

(define-box process-duration-groups ((ms ((5 8) (6 8) (7 8) (4 8))) (timepoints (1 2 3 4 5 6 7 8 9 10)) (modulus 16) (ratio '(3 2)) &key proportional-mode)
  :indoc '("" "" "" "") ; an string list with short docs
  :icon 225  ; the icon
  :doc "" 
  (if (>= *mess* 10) (print (format nil
                                    "process-duration-groups ms ~A ts ~A modulus ~A ratio ~A" 
                                    ms
                                    timepoints
                                    modulus
                                    ratio)))
  (let* ((rratio (/ (car ratio) (cadr ratio)))
         (msdmax (apply #'max (mapcar #'cadr ms)))
         (tscale (/ msdmax modulus))
         (beat-partitions (mapcar #'(lambda (p ms) (om* rratio
                                                        (om* p (/ msdmax (cadr ms)))))
                                  (if proportional-mode
                                      (mapcar #'list (mapcar #'car ms))
                                    (partn-list '(2 3) (mapcar #'car ms)))
                                  ms))
         (timex (mapcar #'(lambda (xs)
                            (cond ((listp xs)
                                   (list (* (car xs) tscale) (cadr xs)))
                                  (t (* xs tscale))))
                        timepoints)))
    (if (>= *mess* 5) (print (format nil "process-duration-groups beat-partitions: ~A timex: ~A" beat-partitions timex)))
    (process-duration-groups-internal (flat beat-partitions) timex)))
   


(define-box process-measure-durations ((ms ((5 8) (6 8) (7 8) (4 8))) 
                                (durations (1 2 3 4 5 6 7 8 9 10))
                                &key enp
                                     list-mode
                                     proportional-mode)
  :indoc '("" "" "enp part mode") ; an string list with short docs
  :icon 225  ; the icon
  :doc ""         
  (if (>= *mess* 10) (print (format nil
                                    "process-measure-durations ms ~A ts ~A " 
                                    ms
                                    durations)))
  (let* ((ms-max-denom (apply #'max (mapcar #'cadr ms)))
         (partns (if proportional-mode (mapcar #'list (mapcar #'car ms)) (partn-list '(2 3) (mapcar #'car ms))))
         (beats (if (> (treelen partns) (length durations))
                    (append durations (make-sequence 'list
                                                (- (treelen partns) (length durations)) 
                                                :initial-element (list -1)))
                  durations))
         (partns-mrg (merge-ms-partns partns beats))
         (enp-midics (if (listp enp) (funcall-rec #'(lambda (x) (cond ((null x) -1) (t x))) (copy-seq enp))))
         (last-enp-midics nil))
    (labels
        ((make-enp-pulse (input)
           (cond
            ((listp input) (make-enp-beat input))
            (t
             (cond
              ((and (listp enp)
                    (floatp input)
                    (or (and last-enp-midics
                             (< (car last-enp-midics) 0))
                        (and enp-midics
                             (< (car enp-midics) 0))))
               (* -1 (floor (abs input))))
              ((and (listp enp)
                    enp-midics
                    (< (car enp-midics) 0))
               (let ((m (pop enp-midics)))
                 (push m last-enp-midics)
                 (* -1 (floor (abs input)))))
              ((and (listp enp)
                    (null enp-midics))
               (* -1 (floor (abs input))))
              (t
               (list 
                input
                :NOTES (list
                        (cond
                         ((listp enp)
                          (cond
                           ((floatp input)
                            (cond
                             (last-enp-midics (floor (car last-enp-midics)))
                             (enp-midics (floor (car enp-midics)))
                             (t 60)))
                           (t
                            (cond
                             (enp-midics 
                              (let ((m (pop enp-midics)))
                                (push m last-enp-midics)
                                (floor m)))
                             (t 60)))))
                         (t 60)))))))))
         (make-enp-beat (beat)
           (list (floor (car beat)) (mapcar #'make-enp-pulse (cadr beat))))
         (make-enp-measure (signature beats)
           (append (mapcar #'make-enp-beat beats) (list :time-signature signature))))
      (let ((tree (cond 
                   (enp (list (mapcar #'make-enp-measure ms partns-mrg)))
                   (list-mode (prcs-ms-timepoint-sigs ms partns-mrg :list-mode t))
                   (t (append (list (read-from-string "?"))
                              (list (prcs-ms-timepoint-sigs ms partns-mrg)))))))
        (values tree partns-mrg partns beats)))))

(cl:defun adjust-durations (list total)
  (labels
      ((duration (x) 
         (cond
          ((null x) 0)
          ((listp x) (duration (car x)))
          (t (abs x))))
       (adjusted-duration (x value)
         (assert x)
         (cond
          ((listp x)
           (list (adjusted-duration (car x) value)
                 (cadr x)))
          ((and (floatp x)
                (< x 0))
           (float (* -1 (abs value))))
          ((floatp x)
           (float (abs value)))
          ((< x 0)
           (floor (* -1 (abs value))))
          (t (abs value)))))
    (let ((seq nil))
      (dotimes (i (length list))
        (let* ((j (- (length list) i))
               (subseq (subseq list 0 j))
               (s (apply #'+ (mapcar #'duration subseq))))
          ;(lprint 'subseq subseq 's s 'total total)
          (when (or (<= s total)
                    (and (> s total)
                         (< (apply #'+ (mapcar #'duration (subseq subseq 0 (1- (length subseq)))))
                            total)))
            ;(lprint 'returning 's s)
            (setf seq subseq)
            (RETURN))))
      (let ((duration (if seq (apply #'+ (mapcar #'duration seq)) 0)))
        (assert (> duration 0))
        (cond
         ((> duration total)
          (append (butlast seq)
                  (let ((last (car (reverse seq))))
                    (list (adjusted-duration last (- (duration last) (- duration total)))))))
         (t seq))))))

(define-box scale-ms-events ((durations ((1 2 3 4 5 6 7)))
                             (ms ((5 4) (3 4) (3 4) (4 4)))
                             (modulus (8 (1 1)))
                             &key enp
                             list-mode
                             print-warnings
                             proportional-mode)
  :icon 225
  :doc "returns (openmusic or enp format) tree, beats, beat sizes, pulses in eac beat"
  (if (>= *mess* 5) (print (format nil "scale-seqc-timepoints modulus/ratio ~A m ~A r ~A" modulus (car modulus) (cadr modulus))))
  (let ((tree-list nil)
        (partns-mrg-list nil)
        (partns-list nil)
        (beats-list nil)
        (voice-enp (cond
                    ((null enp) (make-sequence 'list (length durations)))
                    ((and (listp enp) (every #'listp enp)) enp)
                    (t (make-sequence 'list (length durations) :initial-element enp))))
        (msbeats (ms-beat-count ms modulus)))
    (let ((tps (mapcar #'(lambda (x) (adjust-durations x msbeats)) durations)))
      (dotimes (i (length tps))
        (let ((x (elt tps i)))
          (multiple-value-bind 
              (tree partns-mrg partns beats)
              (process-measure-durations ms                                   
                                  (process-duration-groups ms
                                                         x
                                                         (car modulus)
                                                         (cadr modulus)
                                                         :proportional-mode proportional-mode)
                                  :enp (elt voice-enp i)
                                  :list-mode list-mode
                                  :proportional-mode proportional-mode)
            (push-to-end tree tree-list)
            (push-to-end partns-mrg partns-mrg-list)
            (push-to-end partns partns-list)
            (push-to-end beats beats-list))))
      (values tree-list
              partns-mrg-list
              partns-list
              beats-list))))
(defun enp-ms-groups (voice)
  (labels      
      ((process-ms (input)
         (mapcar #'process-beat input))
       (process-beat (input)
         (cond
          ((and (listp input)
                (= (length input) 2)
                (atom (car input))
                (listp (cadr input)))
           (append
            (list (car input))
            (mapcar #'process-beat (cadr input))))
          ((numberp input) input)
          ((and (listp input)
                (position :notes input))
           (car input))
          (t nil))))
    (mapcar #'process-ms voice)))

(defun enp-part-groups (part)
  (mapcar #'enp-ms-groups part))

(defun prcs-ms-timepoint-sigs (ms partns &key list-mode)
  (if (car ms)
      (append (list (append (list (cond (list-mode ms)
                                        (t (format-mssign (car ms)))))
			    (list (car partns)))) 
	      (prcs-ms-timepoint-sigs (cdr ms) (cdr partns)))
    nil))

(defun flatten-seqc-prcs-sublist (x) 
  (cond ((contains-list x) 
         (match-sublist-lens (atoms2list x)
                             (find-largest-sublist-len (atoms2list x))
                             nil))
        (t (mapcar 'list x))))

(cl:defun flatten-seqc (list &optional pad-int2float)
  (let ((r (let* ((ll (funcall-rec #'(lambda (x)
                                       (cond ((null x) "nil")
                                             (t x)))
                                   list)))
             (if (>= *mess* 30) (lprint 'flatten-seqc ll))
             (labels ((prcs-sublist (x) 
                        (cond ((contains-list x) 
                               (match-sublist-lens (atoms2list x)
                                                   (find-largest-sublist-len (atoms2list x))
                                                   (not pad-int2float)))
                              (t (mapcar 'list x))))
                      (flatn (x)
                        (cond ((null x) nil)
                              (t (append (car x) (flatn (cdr x)))))))
               (let ((flat (mapcar #'flatn
                                   (mat-trans (let ((a 
                                                     (mapcar #'(lambda (x) 
                                                                 
                                                                 (cond ((contains-list (flat x 1)) 
                                                                        (flatten-seqc x pad-int2float))
                                                                       (t (prcs-sublist x))))
                                                             (mat-trans (prcs-sublist ll)))))
                                                a)))))
                 (if pad-int2float           
                     (mapcar 'convert-mnlist-repeats flat)
                   flat))))))
    (funcall-rec #'(lambda (x)
                     (cond ((and (stringp x)
                                 (string= x "nil"))
                            nil)
                           (t x)))
                 r)))


(defun flatten-seqcv (seqc)
  (let ((c)
        (var (make-variable)))
    (map-seqc #'(lambda (x) 
                  (push (mapcar #'car x) c)) seqc)
    ;(print (reverse c))
    (prolog-transpose (reverse c) var)
    var))
    
                   
  


(cl:defun seqc-templates->set (seqc templates)
  (labels ((remove-duplicates (seqc)
             (sort 
              (remove-duplicates 
               (mapcar (lambda (x) (mod x 12))
                       (flat seqc))
               :test '=))))
    (remove-duplicates 
     (loop for p in (remove-duplicates seqc)
                   collect (loop for q in templates
                                 collect (om+ p q))))))

(cl:defun collect-seqc-sets (seqc templates)
  (let (s)
    (loop for i from 0
          while (< i (length seqc))
          do 
          (let ((sl (subseq seqc 0 i))
                (sc (subseq seqc i (+ 1 i)))
                (sr (subseq seqc (+ 1 i) (length seqc))))
            (loop for tmp in templates
                  do 
                  (loop for set in (mat-trans
                                    (flatten-seqc 
                                     (append sl 
                                              (list 
                                               (append (car sc)
                                                       (om+ (car (reverse sc)) tmp)))
                                              sr)))
                        do (push (m-chd2norm set t) s)))))
    (sort (remove-duplicates s :test #'list-eq) #'list<)))

(defun convert-mnlist-repeats (mnl)
  (reverse (convert-mnlist-repeats-rec (reverse mnl))))
(defun convert-mnlist-repeats-rec (mnl)
  (cond ((null mnl) nil)
        ((cdr mnl) (append (list (cond ((list-eq (car mnl) (cadr mnl)) 
                                        (om* (car mnl) 1.0))
                                       (t (car mnl))))
                           (convert-mnlist-repeats-rec (cdr mnl))))
        (t (list (car mnl)))))

(cl:defun remove-flat-mntree-repeat-floats (tr) 
  (mapcar 'remove-flat-mntree-voice-repeat-floats tr))

(cl:defun remove-flat-mntree-voice-repeat-floats (v)
  (let* ((stack nil)
	 (r (labels ((prcs-car (ve) (cond ((null ve) nil)
                                          ((and (floatp (car ve))
                                                (or (and (cdr ve)
                                                         (not (= (car ve) (cadr ve))))
                                                    (not (cdr ve))))
                                           (push (floor (car ve)) stack))
                                          ((not (floatp (car ve))) (push (car ve) stack))
                                          (t nil)))
		     (prcs-list (ll) (cond ((null ll) nil)
					   ((listp ll) (append (prcs-car ll) (prcs-list (cdr ll))))
					   (t nil))))
	      (prcs-list (reverse v)))))
    stack))

(define-box m-chd2norm ((ch list) &optional floor)
  :initvals '((60 64 67))
  :indoc '("midi chd")
  :icon 225
  :doc "use ch-to-normal-form"
  (let ((cnf (ch-to-normal-form ch)))
    (if floor
        (mapcar #'floor cnf)
      cnf)))

(defun m-chd2normv (ch &optional floor)
  (let ((cnf (chd-n-invsv ch)))
    (if floor
        (let ((var (make-variable)))
          (prolog-maplist3 (lambda (x)
                             (funcallv #'floor x))
                           cnf
                           var)
          var)
      cnf)))

(define-box ch-to-normal-form ((ch list))
  :initvals '((60 64 67))
  :indoc '("midi chd")
  :icon 225
  :doc ""
  (car (chd-n-invs ch :xpos-invs-only t)))

(define-box m-chd-eq ((ch1list list) (ch2list list) &optional (subset-eq nil) (test-inversions t))
  :initvals '((60 64 67) (0 4 7) nil t)
  :indoc '("midi chd" "midi chd" "" "")
  :icon 225
  :doc ""
  (labels ((as-lists (ch) 
	     (cond ((listp (car ch)) ch)
		   (t (list ch))))
           (init-chlist-invs (chlist)
             (flat (mapcar (lambda (cc) (chd-n-invs cc 
                                                        :unsorted t
                                                        :xpos-invs-only (not test-inversions))) 
                               chlist) 1))
	   (list-has-non-null (ll) (loop for n in ll when (not (null n)) return t))

	   (ch-equal (c01 c02) (if subset-eq 
                                   (ch-subset-equal c01 c02)
                                 (ch-list-equal c01 c02)))
           (ch-list-equal (c01 c02) (list-eq c01 c02))
           (ch-subset-equal (c01 c02) 
             (if (ch-c02-remove-compare c01 c02)
                 t
               (loop for p in c01 
                     when (ch-c02-remove-compare c01 (om+ p c02))
                     return t)))
           (ch-c02-remove-compare (c01 c02) (null (remove-all c02 c01)))

	   (ch-equal-to-any (chd sets) (loop for s in sets when (ch-equal chd s) return t)))
    (let ((ch1 (init-chlist-invs (as-lists ch1list)))
	  (ch2 (init-chlist-invs (as-lists ch2list))))
      (not (position NIL (loop for c1 in ch1 collect (ch-equal-to-any c1 ch2)))))))

(defun m-chd-eqv (ch1list ch2list &optional (subset-eq nil) (test-inversions t))
  (funcallv #'m-chd-eq
            (apply-substitution ch1list)
            (apply-substitution ch2list)
            subset-eq
            test-inversions))
  
(define-box chd-n-invs ((ch list) &key unsorted xpos-invs-only)
  :initvals '((0 4 7) nil nil)
  :indoc '("midi chd" "sorting by span")
  :icon 225
  :doc "inversions of the normalzed form of ch"
  (labels ((to-mod12-ch (chs) (remove-duplicates (sort (mapcar (lambda (x) (mod x 12)) (om- chs (car chs))))
                                                 :test '=))
           (to-invsns (s) 
             (loop for n from 0 to (- (list-length s) 1) 
                   collect (let* ((r (rotate s n))
                                  (r-l (subseq r 0 (- (list-length r) n)))
                                  (r-r (subseq r (- (list-length r) n) (list-length r)))
                                  (r-t (sort (append r-l (om+ r-r 12)))))
                             (om- r-t (car r-t)))))
           (ch-span (norm-ch) (- (car (reverse norm-ch)) (car norm-ch)))
           (ch-span-comparator (ch1 ch2)  ; returns the ch with higher  span           
             (let ((ch1span (ch-span ch1))
                   (ch2span (ch-span ch2)))
               (cond ((< ch1span ch2span) t)
                     ((> ch1span ch2span) nil)
                     (t (let ((ch1sum (find-sumof ch1))
                              (ch2sum (find-sumof ch2)))
                          (cond ((< ch1sum ch2sum) t)
                                ((> ch1sum ch2sum) nil)
                                (t nil))))))))
    (let* ((chsort (sort ch))
           (mod12ch (to-mod12-ch chsort)) 
           (invs (if xpos-invs-only
                     (to-invsns mod12ch)
                   (append (to-invsns mod12ch)
                           (to-invsns (to-mod12-ch (sort (invert-directn mod12ch))))))))
      (remove-duplicates (if unsorted 
                          invs
                        (sort invs :test (lambda (c1 c2) (ch-span-comparator c1 c2))))
                      :test 'list-eq))))

(defun chd-n-invsv (ch &key unsorted xpos-invs-only)
  (funcallv #'chd-n-invs (apply-substitution ch) :unsorted unsorted :xpos-invs-only xpos-invs-only))


(defun one-subset-fill2 (v elems &key gap default)
  (an-expanded-list nil elems (lambda (x) (= (tree-sum x) v))))

(cl:defun match-sublist-lens (x len &optional ignore-midi-ties) 
  (cond ((null (car x)) nil)
	((< (length (car x)) len) 
	 (append (list (append (car x)
			       (make-sequence 'list
                                              (- len (length (car x)))
                                              :initial-element (if ignore-midi-ties 
                                                                   (car (reverse (flat (car x))))
                                                                 (car (reverse (flat (car x))))))))
		 (match-sublist-lens (cdr x) len ignore-midi-ties)))
	(t (append (list (car x)) (match-sublist-lens (cdr x) len ignore-midi-ties)))))


(define-box om-seqc2timepoints-basic ((seqc list) &optional modulus)
  :initvals '(((1 2) 3) 1)    ; an initial values list
  :indoc '("" "converts duplicated integer atoms to float") ; an string list with short docs
  :icon 225 ; the icon
  :doc ""
  (om-voice2timepoints-basic seqc modulus))

(define-box set->midic0127 ((s list) &optional modulus)
  :initvals '((0 2 3 5 7 8 10))    
  :indoc '("") ; an string list with short docs
  :icon 225 ; the icon
  :doc ""
  (flat 
   (loop for n from 0 
         while (< n 11)
         collect (om+ (mapcar #'(lambda (x) (mod x 12)) s) (* n 12)))))

(defun set->midic0127v (set)
  (flat
   (mapcar #'(lambda (x) (list+v set (* 12 x))) 
           (arithm-ser 0 10 1))))


(define-box om-voice2timepoints-basic ((tree list) &optional modulus)
  :initvals '(((1 2) 3) 1)    ; an initial values list
  :indoc '("" "converts duplicated integer atoms to float") ; an string list with short docs
  :icon 225 ; the icon
  :doc ""
  (mapcar (lambda (x) (voice2timepoints-basic x modulus)) (flatten-seqc tree)))

(define-box seqc->timepoints ((seqc list) &optional modulus)
  :initvals '(((1 2) 3) 1)    ; an initial values list
  :indoc '("" "converts duplicated integer atoms to float") ; an string list with short docs
  :icon 225 ; the icon
  :doc ""
  (mapcar (lambda (x) (voice2timepoints-basic x modulus)) (flatten-seqc seqc)))

(define-box voice2timepoints-basic (v &optional modulus)
  :initvals '(nil nil)    
  :indoc '("list" "modulus") ; an string list with short docs
  :icon 225 ; the icon
  :doc ""
  (let* ((m (if modulus modulus 1))
         (stack nil))
    (labels ((prcs-time-rec (l c r)
               (if (>= *mess* 20) (lprint 'voice2timepoints-basic
                                          'prcs-time-rec
                                          'l
                                          l
                                          'c
                                          c
                                          'r
                                          r))
               (cond ((and (null c)
                           (null r)) nil)
                     ((null c) (prcs-time-rec l (car r) (cdr r)))
                     ((and l 
                           (or (and (floatp c)
                                    (= c (car (reverse l))))
                               (and (screamer::variable? c)
                                    (eq c (car (reverse l))))))
                      (append (list (float m))
                              (prcs-time-rec (append l (list c))
                                             (car r)
                                             (cdr r))))
                     (t 
                      (append (list (floor m))
                              (prcs-time-rec (append l (list c))
                                             (car r)
                                             (cdr r))))))
             (time2stack (tl) 
               (loop for n in tl do (if (and stack 
                                             (floatp n)) 
                                        (push (+ n (pop stack)) stack)
                                      (push (floor n) stack)))))
      (let ((r (time2stack (prcs-time-rec nil nil v))))
        (mapcar 'floor (reverse stack))))))

(define-box om-mseqcmerge ((tree list) &optional scan-mode)
  :initvals '((60 (70 72) 75) 1)    ; an initial values list
  :indoc '("" "") ; an string list with short docs
  :icon 225 ; the icon
  :doc "(sublist) scan modes are nil left-to-right, -1 right-to-left, 0 random permutation scan, > 10 rotate by scan-mode-minus-10 places"
  (mseqcmerge tree scan-mode))

(cl:defun seqc2chunks (seqc)
  (loop for i from 0 
        while (< i (list-max (mapcar 'length seqc)))
        collect (nthseqc seqc i)))
(defun mseqcmerge2 (input &key partition-fn
                               region
                               scan-fns
                               after-scan-rules
                               after-merge-rules
                               final-merge-rules
                               delete-tied-midic)
  (one-value 
   (solution (process-seqc-merge input
                                 :region region
                                 :scan-fns scan-fns 
                                 :after-scan-rules after-scan-rules
                                 :after-merge-rules after-merge-rules
                                 :final-merge-rules final-merge-rules
                                 :delete-tied-midic delete-tied-midic)
             (static-ordering #'divide-and-conquer-force))))

(defun process-seqc-merge (input &key partition-fn
                                      region
                                      scan-fns
                                      after-scan-rules
                                      after-merge-rules
                                      final-merge-rules
                                      delete-tied-midic)
  (let* ((init-scan-fns (if scan-fns 
			    (if (listp scan-fns)
				scan-fns 
				(list scan-fns))
			    (list (lambda (x) (let ((r (a-subset-of x)))
                                                (assert! (>=v (lengthv r) 
                                                             (maxv 1 (-v (lengthv x) 2))))
                                                r)))))
         (merge (a-seqc-merger (extract-seqc-regionv input region)
                               :partition-fn partition-fn
                               :scan-fns init-scan-fns
                               :after-scan-rules after-scan-rules
                               :after-merge-rules after-merge-rules
                               :delete-tied-midic delete-tied-midic))
         (merge-mess (if (>= *mess* 12)
                         (print (list "process-seqc-merge#merge" merge))))
         (r (append (extract-seqc-regionv input region t)
                    (if merge (list merge)))))
    (assert! (notv (equalv nil (evaluatev r final-merge-rules))))
    (if delete-tied-midic 
        (funcall-rec #'floor r)
      r)))

(defun a-seqc-merger-default-partition-fn (seqc-xl &key (max-partition-size 3) 
                                                        (max-single-element-partition-count 2))
  (let* ((a seqc-xl)
         (b (a-partition-of a))
         (c (mapcar #'lengthv b)))
    (assert! (<=v (-v (lengthv c)
                      (lengthv (remove 1 c :test #'=v)))
                  max-single-element-partition-count))
    (assert! (<v (list-maxv c) max-partition-size))
    b))

(defun a-seqc-merger (input &key partition-fn
                                 scan-fns
                                 after-scan-rules
                                 after-merge-rules 
                                 delete-tied-midic)
  (let* ((init-partition-fn (if partition-fn 
                                partition-fn
                              #'a-seqc-merger-default-partition-fn))
         (inputxl (funcall-nondeterministic init-partition-fn (mat-trans (flatten-seqc input delete-tied-midic))))
         (r (a-seqc-merger-rec inputxl
                               scan-fns
                               :after-merge-rules after-merge-rules
                               :after-scan-rules after-scan-rules)))
    (if (>= *mess* 20)
             (print (list "a-seqc-merger#r: " r)))
    (assert! (notv (equalv nil (evaluatev r after-merge-rules))))
    r))

(defun a-seqc-merger-rec (seqcx scan-fns &key after-merge-rules after-scan-rules)
  (if (null seqcx) 
      nil
    (let* ((m (a-seqc-elem-merger (car seqcx) (a-member-of scan-fns) after-scan-rules)))
      (append (if m (list m))
              (a-seqc-merger-rec (cdr seqcx) 
                                 scan-fns
                                 :after-merge-rules after-merge-rules
                                 :after-scan-rules after-scan-rules)))))

(defun a-seqc-elem-merger (input fn &optional after-scan-rules)
  (let ((em (funcall-nondeterministic fn input)))
    (if (>= *mess* 20)
        (print (list "a-seqc-elem-merger#em: " em)))
    (assert! (notv (equalv nil (evaluatev em after-scan-rules))))
    em))

(defun delete-tied-mn (seqcxl)
  (mapcar (lambda (x)
            (remove-if-not 
             (lambda (x) (not (floatp x)))
             x))
          seqcxl))

(define-box om-join-seqc-list ((list list))
  :initvals '((((60 62) (64 65)) ((63 64) (67 69))))    ; an initial values list
  :indoc '("" ) ; an string list with short docs
  :icon 230 ; the icon
  :doc ""
  (join-seqc-list list))

(define-box join-seqc-list ((list list))
  :initvals '(nil )    ; an initial values list
  :indoc '("" ) ; an string list with short docs
  :icon 230 ; the icon
  :doc ""
  (let ((countmax (list-max (mapcar #'length list))))
    (apply #'mapcar 
           #'append
           (loop for s in list
                 collect (append s (if (= countmax (length s))
                                       nil
                                     (list 
                                      (make-sequence 'list 
                                                     (- countmax (length s))
                                                     :initial-element '(-60)))))))))

; general
(define-box ms-beat-count ((measures '((2 4) (2 4))) (modulus '(8 (3 2))))
  :indoc '("measures" "modulus")
  :icon 225
  :doc ""
  (apply #'+ 
         (mapcar 
          #'(lambda (ms)
              (ceiling
               (* (car ms)
                  (/ (car modulus)
                     (cadr ms))
                  (/ (caadr modulus)
                     (cadadr modulus)))))
          measures)))

(define-box clear-rhythm-tree-ms (rt)
  :indoc '("rhythm tree")
  :icon 225
  :doc ""
  (append (list (car rt)) 
          (list (mapcar (lambda (x) (append (list (car x))
                                            (list (list -1)))) (cadr rt)))))

(define-box map-to-rhythm-tree-ms ((rt list) (ll number))
  :initvals '((? (((4 4) (-1)))) -1)
  :indoc '("rhythm tree")
  :icon 225
  :doc ""
  (mapcar (lambda (x) ll) (cadr rt)))

(cl:defun append-1seqc-templates (templates)
  (remove-duplicates 
   (flat    
    (apply 'mapcar 'list 
           (list 
            templates
            (mapcar (lambda (x) 
                      (om* -1 x))
                    templates)))
    1)
   :test 'list-eq))

(defun scale-ms-list (ms n)
  (mapcar (lambda (x) 
            (list (* (car x) (/ n (cadr x)))
                  n))
          ms))

(cl:defun seqc-region->idx-list (region)
  (labels ((init-region-elem (r)
             (cond ((numberp r) (list r (+ 1 r)))
                   ((and (listp r)
                         (= (length r) 1)) (list (car r) (+ (car r) 1)))
                   (t r))))
    (sort 
     (remove-duplicates 
      (flat
       (mapcar (lambda (x) 
                 (loop for i from (car x)
                       while (<= i (cadr x))
                       collect i))
               (mapcar (lambda (x) 
                         (init-region-elem x))
                       region)))
      #'eq))))

(define-box extract-seqc-region ((input list) region &optional prcs-excluded)
  :initvals '(((60) (65 68) (70) (75 76 77)) (0 (2 3)) nil)
  :indoc '("input seqc" "zero-based sublist index(es) list (integers, one or two element lists) ")
  :icon 225
  :doc ""
  (cond ((null region) 
         (cond (prcs-excluded nil)
               (t input)))
        (t (mapcar (lambda (x) (elt input x))
                   (if prcs-excluded
                       (set-difference (arithm-ser 0 (1- (length input)) 1)
                                       (seqc-region->idx-list region))
                     (seqc-region->idx-list region))))))

(defun extract-seqc-regionv (input region &optional prcs-excluded)
  (if (or (>= *mess* 15) (= *mess* -3)) (print (list 'extract-seqc-regionv 'input (apply-substitution input) 'region region)))
  (either
    (progn
      (assert! (equalv nil input))
      nil)
    (let ((idx-list (if prcs-excluded
                        (set-difference (funcallv #'arithm-ser 0 (-v (lengthv input) 1) 1)
                                        (funcallv #'seqc-region->idx-list region))
                      (funcallv #'seqc-region->idx-list (apply-substitution region))))
          (input-region (make-variable)))
      (list-eltv input idx-list input-region)
      (either
        (progn
          (assert! (equalv nil idx-list))
          (assert! (equalv nil input-region))
          nil)
        input-region))))

(cl:defun copy-instance (i)
 (loop with i-class = (class-of i)
 with c = (allocate-instance i-class)
 for sd in (clos:class-slots i-class)
 for sn = (clos:slot-definition-name sd)
 when (slot-boundp i sn)
 do (setf (slot-value c sn)
 (slot-value i sn))
 finally (return c)))

(cl:defun nthseqc (input n)
  (loop for k in input
        collect (cond ((< n (length k))
                       (subseq k n (+ 1 n)))
                      (t nil))))

(cl:defun seqc-eq (s1 s2)
  (labels ((prcs-sublists (s)
             (mapcar (lambda (x) (find-sumof x))
                     s)))
    (or (list-eq s1 s2)
        (and (= (length s1)
                (length s2))
             (= (find-sumof s1)
                (find-sumof s2))
             (list-eq (prcs-sublists s1)
                      (prcs-sublists s1))))))

(cl:defun seqc-eqv (s1 s2)
  (orv (equalv s1 s2)
       (andv (=v (lengthv s1)
                 (lengthv s2))
             (=v (sumv s1)
                 (sumv s2))
             (equalv (mapcar #'sumv s1)
                     (mapcar #'sumv s2)))))

(define-box seqc-norm-sets ((seqc list))
  :initvals '(((60) (65) (70)))
  :indoc '("seqc")
  :icon 410
  :doc ""
  (sort 
   (remove-duplicates (mapcar (lambda (x) 
                             (mapcar 'floor (m-chd2norm x)))
                           (mat-trans
                            (flatten-seqc 
                             (atoms2list seqc))))
                   :test 'list-eq)
   #'list<))

(cl:defun all-modal-xposn (mode)
  (mapcar (lambda (x)
            (mapcar (lambda (y)
                      (mod y 12))
                    (om+ mode x)))
          (loop for n from 0
                while (< n 12)
                collect n)))
(cl:defun mapm12 (l)
  (mapcar (lambda (x) (floor (mod x 12))) l))         

(defun seqc->lengths (seqc)
  (mapcar (lambda (x) 
            (let ((v (make-variable)))
              (assert! (integerpv v))
              (assert! (=v v x))
              v))
          (mapcar 'length seqc)))

(defun seqc->counts (seqc)
  (mapcar (lambda (x) (if (listp x) (find-countof x) 1)) seqc))

(cl:defun partn-to-level (tr)
  (labels ((tr-lens (tr) (remove-duplicates (mapcar 'list-length tr) :test #'=)))
    (let ((partn-tr (partn-to-level-rec tr)))
      (let ((r (if (and partn-tr 
                        (not (list-eq (tr-lens tr) (tr-lens partn-tr))))
                   partn-tr
                 tr)))
        (if (>= *mess* 22)
            (print (list 'partn-to-level 'seqc r)))
        r))))

(cl:defun partn-to-level-rec (tr)
  (let ((ii (remove-duplicates (mapcar 'list-length tr) :test #'=)))
        (let ((trlens (lreduce '- (sort ii) :initial-value 0)))
          (mapcar (lambda (x) (subseq-rec x (lfill trlens (list-length x))))
                  tr))))

(cl:defun partn-to-sublist (seqc idx)
  (let ((seqc-l (subseq seqc 0 idx))
        (seqc-c (subseq seqc idx (+ 1 idx)))
        (seqc-r (subseq seqc (+ 1 idx) (length seqc))))
    (append (mapcar 'list seqc-l)
            seqc-c
            (mapcar 'list seqc-r))))


(define-box one-modulus-and-ratio-to-fit-timepoints ((duration-and-modulus list) 
							 (timepoint-values list) 
							 (modulus-list list)
							 (ratio-list list)
							 &key min-duration)
  :initvals '((16 8) (1 2 3 4 5 6 7 8) (1 2 4 8 16 32) ((7 8) (5 4) (3 2)) (3 16))
  :indoc '("" "" "" "" "" )
  :icon 150 
  :doc "TODO cartx for > 2 lists" 
  (one-value (a-modulus-and-ratio-to-fit-timepoints duration-and-modulus
						    timepoint-values
						    modulus-list
						    ratio-list
						    (if min-duration
                                                        min-duration
                                                      '(1 8)))))

(defun a-modulus-and-ratio-to-fit-timepoints (dam tp modulus-list ratio-list min-duration)
  (let ((tpsum (find-sumof (funcall-rec #'abs tp)))
	(tp-modulus-list (cartx modulus-list (sort-ratio-list ratio-list))))
    (a-modulus-and-ratio-to-fit-timepoints-rec dam
					       tpsum
					       tp-modulus-list
					       (float (/ (car min-duration)
							 (cadr min-duration))))))

(defun a-modulus-and-ratio-to-fit-timepoints-rec (dam tpsum tp-modulus-list min-duration-float)
  (let* ((tp-modulus (either
                       (if tp-modulus-list 
                           (car tp-modulus-list)
                         (fail)) 
                       (if (cdr tp-modulus-list)
                           (a-modulus-and-ratio-to-fit-timepoints-rec dam 
                                                                      tpsum
                                                                      (cdr tp-modulus-list)
                                                                      min-duration-float)
                         (fail))))
         (scaled-tps (find-scaled-tps dam tpsum tp-modulus)))

    (if (or (> scaled-tps (car dam))
	    (< scaled-tps min-duration-float))
      (fail)
      tp-modulus)))

(defun find-scaled-tps (dam tpsum tp-modulus)
  (* tpsum
     (float (/ (cadar (cdr tp-modulus))
	       (caadr tp-modulus)))
     (float (/ (cadr dam)
                      (car tp-modulus)))))

(cl:defun group-trelems (tree)
  (let ((trlens (remove 1 (remove-duplicates (mapcar 'list-length (atoms2list tree)) :test #'list-eq))))
    (if trlens
        (mapcar (lambda (x) 
                  (if (and (< 1 (list-length x))
                           (< (list-min trlens) (list-length x)))
                      (append (subseq x 0 (- (list-min trlens) 1))
                              (list (subseq x (- (list-min trlens) 1) (list-length x))))
                    x)) tree)
      tree)))

(defun ms-list-values (x)
  (mapcar (lambda (y) 
            (cond ((listp y) (car y))
                  (t y)))
          x))

(defun init-ms-template (s &optional debug 
                                     match-init-group-to-signature
                                     (include-duration-symbol t)
                                     (process-seqc t))
  (let ((dummy-var-count 0)
        (vars nil))
    (labels ((make-var (&optional (prefix "x"))
               (car
                (push
                 (read-from-string
                  (concatenate 'string
                               "?"
                               prefix
                               (write-to-string (incf dummy-var-count)))) vars)))
             (process-groups (list)
               (mapcar #'(lambda (x)
                           (cond ((null x) nil)
                                 ((listp x) (list (read-from-string "g") (process-groups x)))
                                 (t x)))
                       list))
             (make-rest-sym (var)
               (let ((varS (write-to-string var)))
                 (read-from-string 
                  (concatenate 'string 
                               "?T"
                               (subseq varS 1 (length varS))))))
             (make-rest-var ()
               (make-var "T"))
             (list->vars (l) 
               (let ((vv (make-var)))
                 (mapcar (lambda (x) (cond ((var? x) x)
                                           (t vv)))
                         l)))
             (var? (a) (let ((str (write-to-string a)))
                         (cond ((and (> (length str) 1)
                                     (or (string= "?T" (subseq str 0 2))
                                         (string= "?t" (subseq str 0 2))))
                                (contains1 (read-from-string (concatenate 'string
                                                                          "?"
                                                                          (subseq str 2 (length str))))
                                           vars))
                               (t 
                                (contains1 a vars)))))
             (sublists-contain-lists? (lists)
               (dolist (x (remove nil lists))
                 (if (null x) (return nil))
                 (if (contains-list x) (return t))))
             (process-seqc-elem (x)
               (let ((var (make-var)))
                 (cond ((null x) (make-rest-sym var))
                       ((eq x 'p) nil)        
                       ((var? x) x)
                       (t var)))))
      (cond (process-seqc
             (let* ((groups (mapcar #'process-groups s))
                    (groups-mess (if (>= *mess* 30) (lprint 's s 'groups groups)))
                    (ms (mat-trans-rec
                         (lambda (x)
                           (if (>= *mess* 30) (lprint 'init-ms-templates 'mat-trans-rec 'x x))
                           (cond ((sublists-contain-lists? x)
                                  (let ((xT (mapcar (lambda (y)
                                               ;(lprint 'y y) 
                                                      (if (>= *mess* 30) (lprint 'init-ms-template 'mat-trans-rec "   " 'y y))
                                                      (let* ((var (make-var "g")))
                                                        (mapcar (lambda (z) 
                                                                  (if (>= *mess* 30) (lprint 'init-ms-template 'mat-trans-rec "   " "   " 'z z 'eq-g (eq z 'g)))
                                                                  (cond ((null z) nil)
                                                                        ((listp z) z)
                                                                        ((eq z 'p) 'p)
                                                                        (t var)))
                                                                y)))
                                                    x)))
                                    xT))
                                 (t 
                                  (funcall-rec #'(lambda (y) (cond ((null y) (make-rest-sym (make-var)))
                                                                   ((eq y 'p) 'p)
                                                                   (t (make-var))))
                                               x))))
                         groups
                         'p))
                    (ms-vars (funcall-rec #'process-seqc-elem ms))
                    (r (cond (debug (process-groups s))
                             (t (remove nil (mapcar (lambda (x) (list (list (read-from-string "?msn") (read-from-string "?msd")) x)) ms-vars)))))
                    (r1 (cond (match-init-group-to-signature 
                               (mapcar (lambda (x)
                                         (cond ((and (= (length (cadr x)) 1)
                                                     (= (length (caadr x)) 2))
                                                (list (car x)
                                                      (list (list (read-from-string "?msn") (cadr (caadr x))))))
                                               (t x)))
                                       r))
                              (t r))))
               (if include-duration-symbol
                   (mapcar #'add-ms-duration-symbol r1)
                 r1)))
            (t 
             (let* ((groups (process-groups s))
                    (ms (list (list (read-from-string "?msn") (read-from-string "?msd")) (funcall-rec #'process-seqc-elem groups))))
               (if (>= *mess* 30) (lprint 's s 'groups groups))
               (cond (include-duration-symbol
                      (add-ms-duration-symbol ms))
                     (t
                      ms))))))))

(defun add-ms-duration-symbol (list)
  (list "?" (list list)))

(defun ms-vars->ratios (list &optional (mode 0))
  (ms-vars->ratios-internal (funcall-rec #'(lambda (x) (let ((var (a-realv)))
                                                         (assert! (=v var x))
                                                         var))
                                         list)
                            mode))
(defun ms-vars->ratios-internal (list &optional (mode 0))
  (labels
      ((list->elems (list)
         (mapcar #'(lambda (x)
                     (real-absv (cond ((listp x) (car x))
                                 (t x))))
                 list))
       (process-list (m list)
         (let* ((terms (list->elems list))
                (s (apply #'+v terms)))
           (mapcar #'(lambda (term x) 
                       (cond ((listp x) 
                              (process-list (*v m (/v term s))
                                            (cadr x)))
                             (t
                              (*v m (/v term s)))))
                   terms
                   list))))
    (let ((msn (caar list))
          (msd (cadar list)))
      (process-list (/v msn msd) (cadr list)))))     
      

(cl:defun enp-measure->om-measure (list)
  (labels
      ((group? (input)
         (and (listp input)
              (some #'listp input)))
       (unnest-single-elem-lists (list) 
         (mapcar #'process-element list))
       (process-element (input)
         (cond ((group? input) (list (car input)
                                     (unnest-single-elem-lists (cadr input))))
               ((and (listp input)
                     (= (length input) 1)
                     (not (listp (car input))))
                (car input))
               (t input))))
  (let ((declared-time-signature (getf (plist-subseq list) :time-signature '(4 4)))
        (beats (mapcar 
                #'(lambda (x)
                    ;(lprint 'xx x)
                    (integer-absv (cond ((listp x) (car x))
                                (t x))))
                (non-plist-subseq list))))
    (list (list (apply #'+v beats)
                (cadr declared-time-signature))
          (unnest-single-elem-lists (strip-plist-sections list))))))

(cl:defun enp-voice-constructor->ratios (enp)
"returns variables constrained to equal the ratio values of
in ENP (Expressive Notation Package) beats
score -* part -* voice -* var"
  (flat1 
   (mapcar #'ms-vars->ratios
           (mapcar #'enp-measure->om-measure 
                   (non-plist-subseq enp)))))

(cl:defun enp-part-constructor->ratios (enp)
  "returns variables constrained to equal the ratio values of
in ENP (Expressive Notation Package) beats
score -* part -* voice -* var"
  (mapcar #'enp-voice-constructor->ratios 
          (non-plist-subseq enp)))

(cl:defun enp-score-constructor->ratios (enp)
"returns variables constrained to equal the ratio values of
in ENP (Expressive Notation Package) beats
score -* part -* voice -* var"
  (mapcar
   #'enp-part-constructor->ratios
   (non-plist-subseq enp)))

(defun ms-vars->ratios1 (list)
  (labels
      ((list->elems (list)
         (mapcar #'(lambda (x)
                     (abs 
                      (cond ((listp x) (car x))
                            (t x))))
                 list))
       (process-list (m list)
         (lprint 'process-list 'm m 'list list)
         (let* ((s (find-sumof (list->elems list))))
           (mapcar #'(lambda (x) 
                       (cond ((listp x) 
                              (process-list (* m (/ (abs (car x)) s))
                                            (cadr x)))
                             (t
                              (* m (/ (abs x) s)))))
                   list))))
    (let ((msn (caar list))
          (msd (cadar list)))
      (process-list (/ msn msd) (cadr list)))))

(defun seqc-ms->ratios (seqc)
  (mapcar #'(lambda (voice) 
              (mapcar #'ms-vars->ratios (cadr voice)))
          seqc))

(defun ms-vars->elems (list)
  (labels ((list->elems (list)
             (mapcar #'(lambda (x)
                         (cond ((listp x) (car x))
                               (t x)))
                     list))
           (process-list (list)
             (mapcar #'(lambda (x) 
                         (cond ((listp x) (process-list (cadr x)))
                               (t (integer-absv x))))
                     list)))
    (flat (process-list (cadr list)))))

(defun seqc-ms->elems (seqc)
  (mapcar #'(lambda (x) 
              (mapcar #'(lambda (y)
                          (ms-vars->elems y))
                      (cadr x)))
          seqc))

;; xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
(defun list-modv12 (list)
  (mapcar #'(lambda (x) (modv x 12)) list))

(defun unnest-seqc (seqc)
  (let ((r (apply #'unnest-seqc-internal seqc)))
    (if r r seqc)))

(defun unnest-seqc-internal (&rest seqc)
  (cond ((contains-atom seqc) seqc)
        ((null (set-difference (mapcar #'length seqc) '(1)))
         (apply #'unnest-seqc-internal (mapcar #'car seqc)))
        (t seqc)))

(define-box om-apply-cps (cps-function vars &key force-function fail-function)
  :initvals '(nil nil)
  :indoc '("" "")
  :icon 150
  :doc ""
  (one-value 
   (solution (apply-cps cps-function vars)
             (cond (force-function force-function)
                   (t (reorder #'domain-size
                               #'(lambda (x) (declare (ignore x) nil))
                               #'<
                               #'linear-force))))))

(define-box join-seqc-ms ((trees list))
  :initvals '(nil)
  :indoc '("")
  :icon 230
  :doc ""
  (let ((countmax (list-max (mapcar #'length trees))))
    (labels
        ((make-empty-ms (signature)
           (list (read-from-string "?")
                 (list (list (list (car signature)
                                   (cadr signature))
                             (list -1)))))
         (pad-trees (x)
           (loop for s in x 
                 collect (let ((signature (caar (get-signaturesv s))))
                            (append s 
                                   (cond ((= countmax (length s)) nil)
                                         (t (make-sequence 'list
                                                            (- countmax (length s))
                                                            :initial-element (make-empty-ms signature)))))))))
      (mapcar #'(lambda (x)
                  (list (read-from-string "?") (apply #'append x)))
              (mat-trans (mapcar #'(lambda (x) (mapcar #'cadr x))
                                 (pad-trees trees)))))))

(define-box join-seqc-rhythm-trees ((trees list))
  :initvals '(nil)
  :indoc '("")
  :icon 230
  :doc ""
  (join-seqc-ms trees))




