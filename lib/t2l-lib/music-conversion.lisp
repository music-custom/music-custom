(in-package :t2l)



(define-box pcset-filter (&key (card 3) ivs<= ivs>= process-pcset-inverse)
  :icon 324
  (labels
      ((pcset< (list1 list2)
         (let ((set1 (sort list1 #'<))
               (set2 (sort list2 #'<)))
           (let ((span1 (- (car (reverse set1)) (car set1)))
                 (span2 (- (car (reverse set2)) (car set2))))
             (cond ((= span1 span2)
                    (< (apply #'+ set1) (apply #'+ set2)))
                   (t
                    (< span1 span2)))))))
    (let ((setlist
           (all-values 
             (let ((pcset (mapcar #'(lambda (x) (an-integer-betweenv 0 11)) (make-sequence 'list card))))
               (assert! (=v (car pcset) 0))
               (assert! (apply #'<v pcset))
               (let ((ivs (mapcar #'(lambda (x) (modv x 12))
                                  (mapcar #'(lambda (x) (-v (cadr x) (car x)))
                                          (nPr pcset 2))))
                     (keys (remove-duplicates (append (mapcar #'car ivs<=) (mapcar #'car ivs>=)))))
                 (if (>= *mess* 20) (print (format nil "ivs <> keys: ~A" keys)))
                 (assert!
                  (cond
                   ((null keys) t)
                   (t
                    (apply 
                     #'andv
                     (mapcar
                      #'(lambda (i)
                          (let ((i<= (system:cdr-assoc i ivs<=))
                                (i>= (system:cdr-assoc i ivs>=))
                                (c (apply #'count-truesv (mapcar #'(lambda (x) (=v i x)) ivs))))
                            (andv (if i<= (<=v c i<=) t)
                                  (if i>= (>=v c i>=) t))))
                      keys)))))
                 (solution pcset (static-ordering #'linear-force)))))))
      (cond ((>= *mess* 30) (print (format nil "unfiltered set list: ~A" setlist)))
            ((>= *mess* 20) (print (format nil "sets: (~A)" (length setlist)))))
      (remove-duplicates (sort setlist #'pcset<)
                         :test #'pcset=
                         :from-end t))))

(define-box pcset= ((set1 (0 4 7)) (set2 (60 64 67)) &optional process-pcset-inverse)
  :icon 324
  (labels
      ((list- (list value) (mapcar #'(lambda (x) (- x value)) list)))
    (cond
     (process-pcset-inverse
      (or (pcset= set1 set2 nil)
          (pcset= (listdxx (reverse (listdx set1))) set2 nil)))
     (t      
      (let ((pcs1 (mapcar #'%12 (list- set1 (car set1)))))
        (some
         #'(lambda (x) (not (null x)))
         (mapcar #'(lambda (pcs2) (null (set-difference pcs1 pcs2)))
                 (mapcar #'(lambda (x) (mapcar #'%12 (list- x (car x))))
                         (mapcar #'(lambda (i) (rotate set2 i))
                                 (arithm-ser 0 (1- (length set2)) 1))))))))))

(define-box pcset=v ((set1 (0 4 7)) (set2 (60 64 67)) &optional process-pcset-inverse)
  :icon 324
  (cond
   (process-pcset-inverse
    (orv (pcset=v set1 set2 nil)
         (let ((inv1 (listdxxv (reverse (listdxv (mapcar #'%12v set1))))))
           (assert! (=v (car inv1) 0))
           (pcset=v inv1 set2 nil))))
   (t
    (let ((pcs1 (mapcar #'%12v (list-v set1 (car set1)))))
      (apply #'orv (mapcar #'(lambda (pcs2) (set-equalv pcs1 pcs2))
                           (mapcar #'(lambda (i) (mapcar #'%12v (list-v set2 (elt set2 i))))
                                   (arithm-ser 0 (1- (length set2)) 1))))))))

   
(define-box seqc-xl-pcsets=v ((seqc ((60 60 60)
                                     (64 64 64)
                                     (67 67 67)))
                              (sets ((0 3 5) 
                                     (0 4 7)
                                     (0 2 5)))
                              &key process-pcset-inverse)
  :icon 324
  (let ((seqcx (remove-duplicates (mat-trans (flatten-seqc seqc)) :test #'set-equal)))
    (reduce-chunks
     #'andv
     (maplist
      #'(lambda (x)
          (if (>= *mess* 5) (print (format nil "seqc-xl-pcsets=v ~A / ~A" (1+ (- (length seqcx) (length x))) (length seqcx))))
          (reduce-chunks
           #'orv
           (mapcar #'(lambda (y) (pcset=v y (car x) process-pcset-inverse)) sets)))
      seqcx))))

(define-box list-nsucc<>v (list step)
  :icon 324
  (apply #'andv (mapcar #'all<>v (nsucc list step))))

(define-box ratio-divisibility-var (ratios &optional (d (2 3)))
  :icon 324
  (let ((ds (if (listp d) d (list d))))
    (reduce 
     #'andv
     (mapcar
      #'(lambda (x) 
          (equalv (modv x (a-member-ofv ds)) 0))
      (mapcar #'(lambda (x) (ifv (<v x 1) (funcallv #'/ 1 x) x)) (flat ratios))))))

(define-box simultaneous-event-count-minv (seqc &key (ratio 0.2))
  :indoc '("input" "mode")
  :icon 324
  :doc ""  
  (if (or (null seqc)
          (<= (length (remove nil seqc)) 1))
      t
    (let ((ts (mapcar
               #'(lambda (xs) 
                   (reverse
                    (maplist #'(lambda (ys) (apply #'+v ys)) 
                             (reverse xs))))
               (remove nil seqc))))
      ;(lprint 'ts ts)
      (let ((pairs (nPr ts 2)))
        ;(lprint 'pairs pairs)
        (let ((counts (mapcar
                       #'(lambda (lists)
                           (apply #'+v
                                  (mapcar
                                   #'(lambda (x)
                                       (apply
                                        #'count-truesv
                                        (mapcar 
                                         #'(lambda (y) (=v x y))
                                         (cadr lists))))
                                   (car lists))))
                       pairs)))
          (let ((totals (mapcar 
                         #'(lambda (x) (length (car x)))
                         pairs)))
                ;(total-count (apply #'+ (mapcar 
                ;                         #'(lambda (x) (* (length (car x)) (length (cadr x))))
                ;                         pairs))))
            (if (>= *mess* 30) (lprint 'counts counts 'totals totals))
            (apply
             #'andv
             (mapcar 
              #'(lambda (x y)
                  (<=v x y))
              counts
              (mapcar #'(lambda (x) (*v ratio x)) totals)))))))))

(define-box seqcx-ival-countv (seqc &key ivs<= ivs>= mode)
  :icon 324
  :doc "" 
  (let ((seqc-xl (remove-duplicates (mat-trans (flatten-seqc seqc)) 
                                    :test #'set-equal)))
    (cond
     ((and mode (= mode 1))
      (seqcx-ival-countv-internal2 seqc-xl ivs<= ivs>=))
     (t
      (seqcx-ival-countv-internal seqc-xl ivs<= ivs>=)))))

(defun seqcx-ival-countv-internal (seqc-xl ivs<= ivs>=)
  (if (or (null seqc-xl) (= (length (car seqc-xl)) 1))
      t    
    (let* ((pairs (remove-duplicates
                   (flat1 (mapcar #'(lambda (x) (nPr x 2)) seqc-xl))
                   :test #'set-difference-eq))
           (ivs (remove nil (mapcar 
                             #'(lambda (x) 
                                 (cond ((or (null x)
                                            (null (car x))
                                            (null (cadr x)))
                                        nil)
                                       (t (modv (-v (car x) (cadr x)) 12))))
                             pairs))))
      (cond 
       ((null ivs) t)
       (t 
        (let* ((seqc-ivals (flat ivs))
               (targets<= (mapcar
                           #'(lambda (i) (let ((c (cdr-assoc i ivs<=)))
                                           (cond ((null c) nil)
                                                 ((atom c) (* c (length seqc-ivals)))
                                                 (t (* (car c) (length seqc-ivals))))))
                           '(0 1 2 3 4 5 6)))
               (targets>= (mapcar
                           #'(lambda (i) (let ((c (cdr-assoc i ivs>=)))
                                           (cond ((null c) nil)
                                                 ((atom c) (* c (length seqc-ivals)))
                                                 (t (* (car c) (length seqc-ivals))))))
                           '(0 1 2 3 4 5 6)))
               (totals (mapcar
                        #'(lambda (i) 
                            (cond
                             ((and (null (elt targets<= i)) (null (elt targets>= i))) nil)
                             ;; to stay within Lispworks CALL-ARGUMENTS-LIMIT of 2047 this is processed in chunks
                             ((= i 1)
                              (reduce #'+v (mapcar #'(lambda (x) (apply #'count-truesv (mapcar #'(lambda (y) (orv (=v y 1) (=v y 11))) x))) (nsucc seqc-ivals 512 :step 512))))
                             ((= i 2)
                              (reduce #'+v (mapcar #'(lambda (x) (apply #'count-truesv (mapcar #'(lambda (y) (orv (=v y 2) (=v y 10))) x))) (nsucc seqc-ivals 512 :step 512))))
                             ((= i 3)
                              (reduce #'+v (mapcar #'(lambda (x) (apply #'count-truesv (mapcar #'(lambda (y) (orv (=v y 3) (=v y 9))) x))) (nsucc seqc-ivals 512 :step 512))))
                             ((= i 4)
                              (reduce #'+v (mapcar #'(lambda (x) (apply #'count-truesv (mapcar #'(lambda (y) (orv (=v y 4) (=v y 8))) x))) (nsucc seqc-ivals 512 :step 512))))
                             ((= i 5)
                              (reduce #'+v (mapcar #'(lambda (x) (apply #'count-truesv (mapcar #'(lambda (y) (orv (=v y 5) (=v y 7))) x))) (nsucc seqc-ivals 512 :step 512))))
                             (t
                              (reduce #'+v (mapcar #'(lambda (x) (apply #'count-truesv (mapcar #'(lambda (y) (=v y i)) x))) (nsucc seqc-ivals 512 :step 512))))))
                        '(0 1 2 3 4 5 6))))
          (if (>= *mess* 7) (print (format nil "targets<=: ~A~%targets>=: ~A~%totals: ~A" targets<= targets>= totals)))
          (apply 
           #'andv
           (mapcar
            #'(lambda (i) 
                (cond
                 ((null (elt totals i)) t)
                 ((and (elt targets<= i) (elt targets>= i))
                  (andv (<=v (elt targets<= i) (elt totals i))
                        (>=v (elt targets>= i) (elt totals i))))
                 ((elt targets<= i)
                  (<=v (elt totals i) (elt targets<= i)))
                 (t 
                  (>=v (elt totals i) (elt targets>= i)))))
            '(0 1 2 3 4 5 6)))))))))

(defun seqcx-ival-countv-internal2 (seqc-xl ivs<= ivs>=)
  (let ((pairs (remove-duplicates (flat1 (mapcar #'(lambda (x) (nPr x 2)) seqc-xl))
                                  :test #'set-equal)))
    (let ((pair-ivs-assoc (mapcar #'(lambda (p) (cons p (modv (-v (cadr p) (car p)) 12))) pairs)))
      (reduce-chunks 
       #'andv
       (mapcar
        #'(lambda (s)
            (let ((ivs (mapcar #'(lambda (x) (cdr-assoc x pair-ivs-assoc :test #'set-equal))
                               (nPr s 2)))
                  (keys (remove-duplicates (append (mapcar #'car ivs<=) (mapcar #'car ivs>=)))))
              (cond
               ((null keys) t)
               (t
                (apply 
                 #'andv
                 (mapcar
                  #'(lambda (i)
                      (let ((i<= (system:cdr-assoc i ivs<=))
                            (i>= (system:cdr-assoc i ivs>=))
                            (c (apply #'count-truesv (mapcar #'(lambda (x) (cond ((= i 0) (=v x 0))
                                                                                 ((= i 1) (orv (=v x 1) (=v x 11)))
                                                                                 ((= i 2) (orv (=v x 2) (=v x 10)))
                                                                                 ((= i 3) (orv (=v x 3) (=v x 9)))
                                                                                 ((= i 4) (orv (=v x 4) (=v x 8)))
                                                                                 ((= i 5) (orv (=v x 5) (=v x 7)))
                                                                                 (t (=v x i))))
                                                             ivs))))
                        (andv (if i<= (<=v c i<=) t)
                              (if i>= (>=v c i>=) t))))
                  keys))))))
        seqc-xl)))))

(define-box seqc-xl-ival-members-var (list &key (ivalset (3 4 5 7 8 9)) debug)
  :indoc '("" "")
  :icon 324
  :doc ""
  (let ((pairs (remove-duplicates  
                (flat1
                 (mapcar 
                  #'(lambda (xs) (nPr xs 2))
                  (mat-trans (flatten-seqc (remove nil list)))))
                :test #'set-equal)))
    (let ((ivs (mapcar #'(lambda (xs) (modv (-v (cadr xs) (car xs)) 12))
                       pairs)))
      (all-memberv ivs ivalset))))

(defun seqc-ms-ratios-funcallv (fn seqc)
  (apply #'andv (mapcar fn (mapcar #'flat (seqc-ms->ratios (remove nil seqc))))))

(define-box seqc-ms-ratios>v (seqc)
  :initvals '(nil)
  :indoc '("")
  :icon 324
  :doc ""
  (seqc-ms-ratios-funcallv #'all>v seqc))

(define-box seqc-ms-ratios>=v (seqc)
  :initvals '(nil)
  :indoc '("")
  :icon 324
  :doc ""
  (seqc-ms-ratios-funcallv #'all>=v seqc))

(define-box seqc-ms-ratios<v (seqc)
  :initvals '(nil)
  :indoc '("")
  :icon 324
  :doc ""
  (seqc-ms-ratios-funcallv #'all<v seqc))

(define-box seqc-ms-ratios<=v (seqc)
  :initvals '(nil)
  :indoc '("")
  :icon 324
  :doc ""
  (seqc-ms-ratios-funcallv #'all<=v seqc))

(define-box seqc-ms-ratios<>v (seqc)
  :initvals '(nil)
  :indoc '("")
  :icon 324
  :doc ""
  (seqc-ms-ratios-funcallv #'(lambda (x)
                               (orv (all<v x)
                                    (all>v x))) 
                           seqc))

(define-box seqc-ms-ratios<>=v (seqc)
  :initvals '(nil)
  :indoc '("")
  :icon 324
  :doc ""
  (seqc-ms-ratios-funcallv #'(lambda (x)
                               (orv (all<v x)
                                    (all>v x))) 
                           seqc))

(define-box seqc-ms-ratios=v (seqc)
  :initvals '(nil)
  :indoc '("")
  :icon 324
  :doc ""
  (seqc-ms-ratios-funcallv #'all=v seqc))

(define-box seqc-ms-ratios/=v (seqc)
  :initvals '(nil)
  :indoc '("")
  :icon 324
  :doc ""
  (seqc-ms-ratios-funcallv #'all/=v seqc))

(define-box seqc-ms-ratios*=v (ms &optional (m '(0.0625 0.125 0.25 0.5 1 2 4 8 16)))
  :initvals '(nil)
  :indoc '("")
  :icon 324
  :doc ""
  (let ((s (a-realv)))
    (apply #'andv
           (mapcar #'(lambda (x) 
                       (equalv x (*v s (a-member-ofv m))))
                   (flat (seqc-ms->ratios ms))))))

(define-box seqc-ms-elemcount-is-multiple-of-msnv ((seqc-ms list) &key (multiplier 1) index)
  :initvals '(nil 1 nil)
  :indoc '("" "" "")
  :icon 324
  :doc ""
  (let ((measure-counts (mapcar #'(lambda (ms) 
                                    (cond ((and index
                                                (listp index)) 
                                           (let ((counts (mapcar #'tree-count ms)))
                                             (list-max 
                                              (mapcar #'(lambda (i) (elt counts i)) counts))))
                                          ((and index
                                                (integerp index))
                                           (elt (mapcar #'tree-count ms) index))
                                          (t (list-max (mapcar #'tree-count ms)))))
                                (mat-trans (seqc-ms->elems seqc-ms))))
        (signatures (mapcar #'car (car (get-signaturesv seqc-ms)))))
    (if (>= *mess* 40)
        (progn
          (lprint 'seqc-ms-elemcount-is-multiple-of-msnv 'seqc-ms seqc-ms 'multiplier multiplier 'index index)
          (lprint 'seqc-ms-elemcount-is-multiple-of-msnv 'measure-counts measure-counts)
          (lprint 'seqc-ms-elemcount-is-multiple-of-msnv 'signatures signatures)))
    (apply #'andv (mapcar #'(lambda (count msn) (=v msn count))
                          (mapcar #'(lambda (x) (*v x (cond ((null multiplier) 1)
                                                            ((numberp multiplier) multiplier)
                                                            ((listp multiplier) (a-member-ofv multiplier))
                                                            (t multiplier))))
                                  measure-counts)
                          signatures))))

(define-box seqc-ms-elemcount-div-ms-num-var ((seqc-ms list) &key index)
  :initvals '(nil nil)
  :indoc '("" "" "")
  :icon 324
  :doc ""
  (let ((measure-counts (mapcar #'(lambda (ms) 
                                    (cond ((and index
                                                (listp index)) 
                                           (let ((counts (mapcar #'tree-count ms)))
                                             (list-max 
                                              (mapcar #'(lambda (i) (elt counts i)) counts))))
                                          ((and index
                                                (integerp index))
                                           (elt (mapcar #'tree-count ms) index))
                                          (t (list-max (mapcar #'tree-count ms)))))
                                (mat-trans (seqc-ms->elems seqc-ms))))
        (signatures (mapcar #'car (car (get-signaturesv seqc-ms)))))
    (apply #'andv (mapcar #'(lambda (count msn) 
                              (if (>= *mess* 15) (lprint 'seqc-ms-elemcount-div 'count count 'msn msn))
                              (=v (modv count msn) 0))
                          measure-counts
                          signatures))))

(define-box seqc-ms-signatures-mapv (fn (seqc-ms-list list) (seqc-list list))
  :initvals '(nil nil nil)
  :indoc '("fn (ms seqc)" "seqc-ms-list" "seqc-list")
  :icon 324
  :doc ""
  (let ((signatures (mapcar #'car (get-signaturesv seqc-ms-list))))
    (apply #'andv
           (mapcar fn signatures seqc-list))))


(define-box listdx-similarity-var ((lists list))
  :initvals '(nil)
  :indoc '("lists")
  :icon 324
  :doc ""
  (let ((pairs (mapcar #'(lambda (x) (nsucc x 2)) 
                       (mapcar #'flat lists))))
    (apply
     #'andv
     (mapcar
      #'(lambda (xs)
          (if (every #'(lambda (x) (= (length x) 2)) xs)
              (apply
               #'orv
               (apply
                #'andv
                (mapcar 
                 #'(lambda (y) ; pair
                     (<=v (car y) (cadr y)))
                 xs))
               (apply
                #'andv
                (mapcar
                 #'(lambda (y) ; pair
                     (>=v (car y) (cadr y)))
                 xs)))
            t))
      pairs))))


(define-box list-n!pairs-/=v ((list '(1 2 3 4 5)) &optional (min 3))
  :indoc '("lists" "min")
  :icon 324
  :doc ""
  (if (< (length list) 3)
      t
    (apply
     #'andv
     (let ((len (length list))
           (len12 (floor (/ (length list) 2))))
       (let ((is (cond ((= len 3) (cons 2 1))
                       ((= len 4) (cons 2 2))
                       ((= len 5) (cons 2 2))
                       ((= len 6) (cons 3 2))
                       ((= len 7) (cons 3 2))
                       ((= len 8) (cons 4 3))
                       (t (cons len12 min)))))
         (loop for i downfrom (car is) while (>= i (cdr is))
               collect (apply
                          #'andv
                          (mapcar
                           #'(lambda (xs) (apply #'/=v xs))
                           (nsucc (mapcar #'car (nsucc list i :step i)) 2)))))))))


(define-box seqc-n!pairs-/=v ((seqc '((1 2 3 4 5))) &optional (min 3))
  :indoc '("lists")
  :icon 324
  :doc ""
  (apply 
   #'andv
   (mapcar
    #'(lambda (i)
        (let ((n (mod (* (ceiling (/ (length seqc) 3)) i) (length seqc))))
          (list-n!pairs-/=v (om:rotate (elt seqc n) i) min)))
    (om:arithm-ser 0 (1- (length seqc)) 1))))

(define-box seqc-voices-not-equalv ((seqc '((1 2 3 4 5))) &optional (car/=v t))
  :indoc '("lists")
  :icon 324
  :doc ""
  (andv
   (if car/=v
       (apply #'andv (mapcar #'(lambda (xs)
                                 (let* ((l1 (car xs))
                                        (l2 (cadr xs)))
                                   (/=v l1 l2)))
                             (nPr (mapcar #'car seqc) 2)))
     t)
   (apply #'andv (mapcar #'(lambda (xs)
                            (let* ((len (min (length (car xs)) (length (cadr xs))))
                                   (l1 (subseq (car xs) 0 len))
                                   (l2 (subseq (cadr xs) 0 len)))
                              (notv (list=v l1 l2))))
                        (nPr (if car/=v (mapcar #'cdr seqc) seqc)
                             2)))))


(define-box jjf-1st-species1 (cf &optional mode) 
  :icon 324
  (let* ((v (mapcar #'(lambda (x) (an-integer-betweenv 1 127)) cf))
         (seqc (list v cf))
         (ivals (mapcar #'(lambda (x) (-v (car x) (cadr x))) (mat-trans seqc)))
         (hivls (remove nil (maplist #'(lambda (x) (if (cdr x) (-v (car x) (cadr x)) nil)) v)))
         (mode0127 (if mode (set->midic0127 mode))))
    (if mode0127
        (progn 
          (mapcar #'(lambda (x) (assert! (memberv x mode0127))) cf)          
          (mapcar #'(lambda (x) (assert! (memberv x mode0127))) v)))
    (assert! (memberv (car v) (list (car cf)
                                    (+v (car cf) 7)
                                    (+v (car cf) 12))))
    (mapcar #'(lambda (x) (assert! (memberv (integer-absv x) '(1 2 3 4 5)))) hivls)
    (mapcar #'(lambda (x) (assert! (>=v x 0))) (list (car ivals) (car (reverse ivals))))
    (mapcar #'(lambda (x) (assert! (memberv x '(3 4 5 7 8 9 15)))) (subseq ivals 1 (1- (length ivals))))
    (mapcar #'(lambda (x) (assert! (notv (=v (modv x 12) 0)))) (subseq ivals 1 (1- (length ivals))))
    (mapcar #'(lambda (x) (assert! (<=v x 16))) ivals)
    (maplist #'(lambda (x) (if (cdr x) (assert! (notv (=v (car x) (cadr x)))))) v)
    (maplist #'(lambda (x) 
                 (if (cdr x)
                     (progn 
                       (assert! (notv (andv (=v 7 (modv (car x) 12)) (=v 7 (modv (cadr x) 12)))))
                       (assert! (notv (andv (=v 0 (modv (car x) 12)) (=v 0 (modv (cadr x) 12)))))))) ivals)
    ;(mapcar #'(lambda (x) (assert! (memberv x '(3 4 5 7 8 9)))) ivals)
    (assert! (=v (car (reverse ivals)) 12))
    (assert! (memberv (-v (car (reverse v)) (cadr (reverse v))) '(2 1 -1 -2)))
    seqc)) 



(defun %12 (x) (mod x 12))
(defun mod12 (x) (mod x 12))
(defun %12v (x) (modv x 12))
(defun mod12v (x) (modv x 12))   