(in-package :t2l)
(setq sudoku-1 
'(8 _ _  _ _ _  _ _ _    
_ _ 3  6 _ _  _ _ _   
_ 7 _  _ 9 1  2 8 _    
_ 5 _  _ _ 7  _ _ _    
_ _ _  _ 4 5  7 _ _
_ _ _  1 _ _  _ 3 _ 
_ _ 1  _ _ _  _ 6 8 
_ _ 8  5 _ _  _ 1 _
_ 9 _  _ _ _  4 _ _))
(defun diagonals (l) (diag l))
(defun diag (l)  
  (let ((rows (rows l))
        (d1)
        (d2))
    (dotimes (i (length rows))
      (push (elt (elt rows i) i) d1)
      (push (elt (elt rows (- (1- (length rows)) i)) i) d2))
    (list (reverse d1)
          (reverse d2))))
      
(defun rows (l)
  (let ((len (floor (sqrt (length l))))
        (c))
    (dotimes (i len)
      (let ((i0 (* i len)))             
        (push (subseq l i0 (+ i0 len)) c)))
    (reverse c)))
(defun cols (l)
  (mat-trans (rows l)))
(defun chunks (l &optional (cdimen 3))
  (let* ((clen (expt cdimen 2))
         (ccount (/ (length l) clen))
         (c-list nil))
    (dotimes (n ccount)
      (push 
       (let ((offset (* clen
                        (floor (/ n cdimen))))
             (ccc nil))
         (dotimes (r cdimen)
           (dotimes (c cdimen)
             (let ((rr (+ (* (floor (/ n cdimen))
                             cdimen)
                          r))
                   (cc (+ (* (mod n cdimen)
                             cdimen)
                          c)))
             (push (elt l (+ (* rr clen) cc)) ccc))))
        (reverse ccc))
      c-list))
    (reverse c-list)))

(define-box apply-sudoku-csp ((list list))
  :initvals '(nil)
  :indoc '("" )
  :icon 410
  :doc ""
  (assert! (notv (memberv nil (mapcar (lambda (x) (apply #'/=v x)) (rows list)))))
  (assert! (notv (memberv nil (mapcar (lambda (x) (apply #'/=v x)) (cols list)))))
  (assert! (notv (memberv nil (mapcar (lambda (x) (apply #'/=v x)) (chunks list)))))
  list)

(define-box om-sudoku-csp ((list list))
  :initvals '(nil)
  :indoc '("" )
  :icon 150
  :doc ""
  (andv (apply #'andv (mapcar #'(lambda (x) (apply #'/=v x)) (rows list)))
        (apply #'andv (mapcar #'(lambda (x) (apply #'/=v x)) (cols list)))
        (apply #'andv (mapcar #'(lambda (x) (apply #'/=v x)) (chunks list)))))

(defun assert!-sudoku-csp (list)
  :initvals '(nil)
  :indoc '("" )
  :icon 150
  :doc ""
  (dolist (x (list (rows list) (cols list) (chunks list)))
    (dolist (y x) (assert! (apply #'/=v y)))))

(defun sudoku (s)
  (let ((v (mapcar (lambda (x)
                     (cond ((numberp x) x)
                           (t (an-integer-betweenv 1 9))))
                   s)))
    (assert! (om-sudoku-csp v))
    ;(assert!-sudoku-csp v)
    v))

(defun magic-square (n)
  (let* ((n2 (expt n 2))
         (msum (/ (* n (1+ n2)) 2))
         (vars (mapcar #'(lambda (x) (an-integer-betweenv 1 n2))
                      (make-sequence 'list n2))))
    (assert! (apply #'/=v vars))
    (assert! (apply #'=v (append (list msum)
                                 (mapcar #'(lambda (x) (apply #'+v x))
                                         (append (rows vars) 
                                                 (cols vars)
                                                 (diagonals vars))))))
    (rows vars)))