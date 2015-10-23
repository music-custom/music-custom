(in-package :t2l)
(define-box one-partition-having ((x list) &optional partition-fn element-fn fail-form)
  :initvals '((1 2 3 4 5) nil nil 'fail)    ; an initial values list
  :indoc '("" "" "" "" ) ; an string list with short docs
  :icon 150  ; the icon
  :doc ""
  (cond (fail-form         
         (one-value 
          (a-partition-having x partition-fn element-fn)
          fail-form))
        (t 
         (one-value 
          (a-partition-having x partition-fn element-fn)))))

(define-box subsets ((list list) &key rules n)
  :initvals '(nil nil nil nil)
  :indoc '("list" "rules" "n")
  :icon 150
  :doc ""
  (remove nil
          (cond (n 
                 (n-values n
                   (let ((a (a-subset-of list)))
                     (unless (evaluate a rules) (fail))
                     a)))
                (t
                 (all-values
                   (let ((a (a-subset-of list)))
                     (unless (evaluate a rules) (fail))
                     a))))))

(define-box om-allperms (list &key test force-function)
  :initvals '((p a i n t) nil nil)
  :indoc '("" "" "")
  :icon 150
  :doc ""  
  (all-values   
    (om-solution 
     (let ((z (funcall-rec #'(lambda (x) (make-variable)) list)))
       (prolog-perm2 list z)
       (unless (evaluate (apply-substitution z) test) (fail))
       z) force-function)))

(define-box allpairs ((list list) &key (test 'eq))
  :initvals '((1 2) 'eq)
  :indoc '("" "test")
  :icon 150
  :doc ""
  (remove-duplicates 
   (all-values
    (let* ((a (a-member-of list))
           (b (a-member-of (remove a list :test test))))
      (list a b)))
   :test #'(lambda (x y) (not (set-difference x y :test test)))))

(define-box om-allpartitions (list &key test force-function)
  :initvals '((p a i n t) nil nil)
  :indoc '("" "" "")
  :icon 150
  :doc ""  
  (all-values   
    (om-solution (perms list :test test) force-function)))

(define-box om-oneperm (list &key test force-function fail-form)
  :initvals '((p a i n t) nil nil)
  :indoc '("" "" "")
  :icon 150
  :doc ""
  (let ((z (funcall-rec #'(lambda (x) (make-variable)) list)))
    (one-value
      (progn 
        (prolog-perm2 list z)
        (unless (evaluate (apply-substitution z) test) (fail))
        (om-solution z force-function))
      fail-form)))

(define-box om-ithperm (list i &key test force-function fail-form)
  :initvals '((p a i n t) 0 nil nil nil)
  :indoc '("" "" "" "" "")
  :icon 150
  :doc ""
  (let ((z (funcall-rec #'(lambda (x) (make-variable)) list)))
    (ith-value
      i
      (progn 
        (prolog-perm2 list z)
        (unless (evaluate (apply-substitution z) test) (fail))
        (om-solution z force-function))
      fail-form)))

(define-box om-all-solutions (x &optional force-function)
  :icon 150
  (all-solutions x force-function))

(define-box om-one-solution (x &optional force-function fail-form)
  :icon 150
  (one-solution x force-function fail-form))

(define-box om-reorder-one-solution (x cost-function terminate? force-function &optional fail-form)
  :icon 150
  (one-value (solution x 
                       (reorder (cond ((null cost-function) #'domain-size)
                                      ((functionp cost-function) cost-function)
                                      ((or (string= (format nil "~A" cost-function) "domain-size")
                                           (string= (format nil "~A" cost-function) "ds")) #'domain-size)   
                                      ((or (string= (format nil "~A" cost-function) "range-size")
                                           (string= (format nil "~A" cost-function) "rs")) #'range-size)
                                      (t #'domain-size))
                                (cond ((null terminate?) #'(lambda (x) (declare (ignore x)) nil))
                                      ((functionp terminate?) terminate?)
                                      (t #'(lambda (x) (declare (ignore x)) nil)))
                                #'<
                                (cond ((null force-function) #'linear-force)
                                      ((functionp force-function) force-function)
                                      ((or (string= (format nil "~A" force-function) "linear-force")
                                           (string= (format nil "~A" force-function) "lf")) #'linear-force)   
                                      ((or (string= (format nil "~A" force-function) "print-linear-force")
                                           (string= (format nil "~A" force-function) "plf")) #'print-linear-force)
                                      ((or (string= (format nil "~A" force-function) "print-divide-and-conquer-force")
                                           (string= (format nil "~A" force-function) "pdacf")) #'print-divide-and-conquer-force)
                                      ((or (string= (format nil "~A" force-function) "divide-and-conquer-force")
                                           (string= (format nil "~A" force-function) "dacf")) #'divide-and-conquer-force)
                                      (t #'linear-force))))
             (if fail-form fail-form (fail))))

(define-box om-ith-solution (i x &optional force-function fail-form)
  :initvals '(0 nil)
  :indoc '("" "" )
  :icon 150
  :doc ""
  (ith-solution (if i i 0) 
                x 
                force-function 
                fail-form))

(define-box om-best-solution (form1 objective-form &optional force-function form2)
  :initvals '(nil nil nil nil)
  :indoc '("form1" "objective-form" "force-function (lf dacf reorder" "form2")
  :icon 150
  :doc ""
  (best-solution form1 objective-form force-function form2))


(define-box om-one-value (x &optional fail-form)
  :initvals '(nil)
  :indoc '("" )
  :icon 150
  :doc ""
  (cond (fail-form (one-value x fail-form))
        (t (one-value x))))


(define-box om-all-values (x)
  :initvals '(nil)
  :indoc '("" )
  :icon 150
  :doc ""
  (all-values x))


(define-box om-nperms ((input list) &optional n)
  :initvals '((1 2 3) 1)
  :indoc '("" "")
  :icon 410
  :doc ""  
  (if n 
      n
    (setf n (cond ((< (length input) 4) (n! (length input)))
                  (t 24))))
  (one-value
   (let ((s (make-variable)))
     (prolog-nperms n input s)
     (solution s (static-ordering #'linear-force)))
   'fail))