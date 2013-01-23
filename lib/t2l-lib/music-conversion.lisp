(in-package :t2l)

(defun jjf-1st-species1 (cf &optional mode) 
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

(define-box ratio-divisibility-var (ratios &optional (d (2 3)))
  (let ((ds (if (listp d) d (list d))))
    (reduce 
     #'andv
     (mapcar
      #'(lambda (x) 
          (equalv (modv x (a-member-ofv ds)) 0))
      (mapcar #'(lambda (x) (ifv (<v x 1) (funcallv #'/ 1 x) x)) (flat ratios))))))

(define-box seqc-xl-ival-members-var (list &key (ivalset (3 4 5 7 8 9 -9 -8 -7 -5 -4 -3)) debug)
  :indoc '("" "")
  :icon 324
  :doc ""
  (let ((vars (mapcar #'(lambda (x)
                          (let ((a (remove-nil x)))
                            (cond 
                             ((and debug a)
                              (lprint 'seqc-xl-ival-members-var
                                      (mapcar #'(lambda (y) 
                                                  (modv y 12))
                                              (listdxv a)))
                              t)
                             (a 
                              (all-memberv (mapcar #'(lambda (y)
                                                       (modv y 12)) 
                                                   (listdxv a)) 
                                           ivalset))
                             (t t))))
                        (mat-trans (flatten-seqc (remove nil list))))))
    (if (>= *mess* 33) (lprint 'seqc-xl-ival-members-var 'vars vars))
    (apply #'andv vars)))

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

(define-box seqcx-ival-countv (seqc ival-assoc)
  :indoc '("" "" )
  :icon 324
  :doc "" 
  (apply #'andv (mapcar #'(lambda (xs) (seqcx-ival-countv-internal xs ival-assoc)) (nsucc (mat-trans (flatten-seqc  (remove nil seqc))) 128 :step 128))))

(defun seqcx-ival-countv-internal (seqc-xl ival-assoc)
  (if (or (null seqc-xl) (= (length (car seqc-xl)) 1))
      t    
    (let ((interval-vars (mapcar 
                          #'(lambda (list)
                              (let ((pairs (nPr list 2)))
                                (remove nil (mapcar
                                             #'(lambda (x) 
                                                 (ifv (orv (equalv (car x) nil) (equalv (cadr x) nil))
                                                      nil
                                                   (modv (-v (cadr x) (car x)) 12)))
                                             pairs))))
                          seqc-xl))
          (nP2-count (nPr (length (car seqc-xl)) 2)))
      (cond 
       ((null interval-vars) t)
       (t 
        (assert (< (apply #'max (mapcar #'car ival-assoc)) 7))
        (let* ((seqc-ivals (flat interval-vars))
               (targets (mapcar
                         #'(lambda (i) (let ((c (cdr-assoc i ival-assoc)))
                                         (cond ((null c) nil)
                                               ((atom c) (* c (length seqc-ivals)))
                                               (t (* (car c) (length seqc-ivals))))))
                         '(0 1 2 3 4 5 6)))
               (totals (mapcar
                        #'(lambda (i) 
                            (cond
                             ((null (elt targets i)) nil)
                             ;; to stay within Lispworks CALL-ARGUMENTS-LIMIT of 2047 this is processed in chunks
                             ((= i 1)
                              (reduce #'+v (mapcar #'(lambda (x) (apply #'count-truesv (mapcar #'(lambda (y) (orv (equalv y 1) (equalv y 11))) x))) (nsucc seqc-ivals 512 :step 512))))
                             ((= i 2)
                              (reduce #'+v (mapcar #'(lambda (x) (apply #'count-truesv (mapcar #'(lambda (y) (orv (equalv y 2) (equalv y 10))) x))) (nsucc seqc-ivals 512 :step 512))))
                             ((= i 3)
                              (reduce #'+v (mapcar #'(lambda (x) (apply #'count-truesv (mapcar #'(lambda (y) (orv (equalv y 3) (equalv y 9))) x))) (nsucc seqc-ivals 512 :step 512))))
                             ((= i 4)
                              (reduce #'+v (mapcar #'(lambda (x) (apply #'count-truesv (mapcar #'(lambda (y) (orv (equalv y 4) (equalv y 8))) x))) (nsucc seqc-ivals 512 :step 512))))
                             ((= i 5)
                              (reduce #'+v (mapcar #'(lambda (x) (apply #'count-truesv (mapcar #'(lambda (y) (orv (equalv y 5) (equalv y 7))) x))) (nsucc seqc-ivals 512 :step 512))))
                             (t
                              (reduce #'+v (mapcar #'(lambda (x) (apply #'count-truesv (mapcar #'(lambda (y) (equalv y i)) x))) (nsucc seqc-ivals 512 :step 512))))))
                        '(0 1 2 3 4 5 6))))
          (if (>= *mess* 4) (print (apply
                                    #'concatenate
                                    (append 
                                     (list 'string)
                                     (list (format nil "seqcx-ival-countv~%"))
                                     (mapcar
                                      #'(lambda (i)
                                          (format nil
                                                  "i: ~A ~A ?<= ~A~%"
                                                  i
                                                  (if (elt totals i) (value-of (elt totals i)))
                                                  (if (elt targets i) (value-of (elt targets i)))))
                                      '(0 1 2 3 4 5 6))))))                                            
          (apply #'andv
                 (mapcar
                  #'(lambda (i) (if (and (elt totals i) (elt targets i)) 
                                    (if (< (elt targets i) 0)
                                        (>=v (elt totals i) (* -1 (elt targets i)))
                                      (<=v (elt totals i) (elt targets i)))
                                  t))
                  '(0 1 2 3 4 5 6)))))))))

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

(define-box read-textfile (filename)
  :indoc '("filename")
  :icon 908
  :doc ""
  (let ((in (open filename :if-does-not-exist nil)))
    (when in
      (let ((string (loop for line = (read-line in nil)
                          while line collect line)))
        (close in)
        (print string)
        (let ((out (apply
                    #'concatenate
                    (append (list 'string)
                            (mapcar #'(lambda (s)
                                        (format nil (concatenate 'string s "~%")))
                                    (butlast string))
                            (last string)))))
          out)))))

(define-box write-textfile (input label ext &optional timezone)
  :indoc '("input" "label" "ext" "timezone")
  :icon 908
  :doc ""
  (labels
      ((format-filename (label) 
         (multiple-value-bind 
             (second minute hour date month year day) 
             (decode-universal-time (get-universal-time))
           (format nil "~A_~A-~A-~A-~A_~A_~A.~A" label month day year hour minute second ext))))
    (let ((filename (format-filename label)))
      (with-open-file (str (om::outfile filename)
                           :direction :output
                           :if-exists :supersede
                           :if-does-not-exist :create)
        (format str (write-to-string input)))
      filename)))