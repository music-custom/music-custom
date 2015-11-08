(in-package :t2l)

;; screamer+ macro Copyright 1998-2000 University of Aberdeen 
(defmacro ifv (condition exp1 &optional (exp2 nil))
  ;; If the condition is bound then there is no need to create additional
  ;; constraint variables
  `(let ((c ,condition))
     (assert! (booleanpv c))
     (if (bound? c)
	 (if (screamer::known?-true c)
	     ,exp1
	   ,exp2)
       (let ((z (make-variable)))
	 (screamer::attach-noticer!
	  #'(lambda ()
	      ;; Change 31/8/99, SAW
	      ;; Used to check that z is not bound too, but that seemed wrong
	      (when (bound? c)
		(if (equal (value-of c) t)
		    (screamer::assert!-equalv z ,exp1)
		  (screamer::assert!-equalv z ,exp2))))
	  c)
	 z))))

;; screamer+ macro Copyright 1998-2000 University of Aberdeen 
(defun quote-up (f &rest args)
  (cond
   ((= (length args) 0)
    `(quote ,f))
   (t
    `(,f ,@args))))

;;; This function is also used by solution to return the values
;;; found by the search. If objects were explored by the search
;;; NEW instances of the same type are generated and returned.

#|(defun apply-substitution (x &aux retobj)
  (let ((val (value-of x)))
    ;; Changed from a cond to a typecase, 8/7/00
    (typecase val
      (cons
       (cons (apply-substitution (car val)) (apply-substitution (cdr val)))
       )
      (standard-object
       (setq retobj (make-instance (class-name (class-of val))))
       (copy-slots val retobj)
       retobj 
       )
      (array
       (setq retobj (make-array (array-dimensions val)))
       (copy-cells val retobj)
       retobj
       )
      (t val)
      )
    )
  )|#

;;; This function returns a variable which is constrained to return a list
;;; containing the distinct elements of x. The argument x can be either a
;;; value or a constraint variable at the time of function invocation.

(defun members-ofv (x)
  (let ((z (make-variable)))
    (screamer::attach-noticer!
     #'(lambda()
         (when (and (bound? x) (every #'bound? (value-of x)))
           (do* ((dec (apply-substitution x) (cdr dec))
                 (curr (car dec) (car dec))
                 (vals nil))
                ((endp dec) 
                 (assert! (equalv z vals)))
             (pushnew (value-of curr) vals :test #'equal))))
       x)
     z))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Function: set-equalv
;;;
;;; This function returns a boolean variable constrained to indicate whether
;;; the lists x and y have the same members
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|(defun set-equalv (x y)
  (let ((z (a-booleanv))
	noticer)
    
    (flet (;; This is just a function which can be used by 'sort' to
	   ;; derive some well-defined ordering for any known values x and y
	   (strcmp (x y)
		  (numberp (string> (format nil "~s" x)
				    (format nil "~s" y)))))
      (setq noticer
	    #'(lambda()
		(when (and (ground? x) (ground? y))
		  (assert!
		   (equalv z
			   (equalv
			    (sort
			     (value-of (members-ofv (apply-substitution x)))
			     #'strcmp)
			    (sort
			     (value-of (members-ofv (apply-substitution y)))
			     #'strcmp)))))))
      (screamer::attach-noticer! noticer x)
      (screamer::attach-noticer! noticer y))
    z))|#

(cl:defun screamer< (x y)
  (labels
      ((hash (arg1) (cond ((null arg1) 0)
                          (t (sxhash (cond ((screamer::variable? arg1) 
                                            (screamer::variable-name arg1))
                                           (t arg1)))))))
    (cond ((and (null x) (null y)) nil)
          ((null x) t)
          ((null y) nil)
          (t (< (hash x) (hash y))))))
         
         

(defun set-equalv (x y)
  (let ((z (a-booleanv)))
    (let ((noticer #'(lambda ()
                       (when (and (ground? x) (ground? y))
                         (local
                           (screamer::assert!-equalv z
                                                     (equalv (sort (value-of (members-ofv (apply-substitution x)))
                                                                   #'screamer<)
                                                             (sort (value-of (members-ofv (apply-substitution y)))
                                                                   #'screamer<))))))))
      (screamer::attach-noticer! noticer x)
      (screamer::attach-noticer! noticer y)
      z)))


;;; This version of funcallv uses ground? to test the boundness of its arguments
;;; instead of bound?
#|(defun funcallgv (f &rest x)
  (let ((f (value-of f)))
    (if (screamer::variable? f)
        (error "The current implementation does not allow the first argument~%~
              of FUNCALLV to be an unbound variable"))
    (unless (functionp f)
      (error "The first argument to FUNCALLV must be a deterministic function"))
    (if (every #'ground? x)
        (apply f (mapcar #'value-of x))
      (let ((z (make-variable)))
        (screamer::assert!-constraint
         #'(lambda (&rest x) (equal (first x) (apply f (rest x)))) t (cons z x))
        (dolist (argument x)
          (screamer::attach-noticer!
           #'(lambda ()
               (if (every #'ground? x)
                   (screamer::assert!-equalv z (apply f (mapcar #'value-of x)))))
           argument))
        z))))|#
(defun funcallgv (f &rest x)
  (let ((f (value-of f)))
    (if (screamer::variable? f)
        (error "The current implementation does not allow the first argument~%~
              of FUNCALLV to be an unbound variable"))
    (unless (functionp f)
      (error "The first argument to FUNCALLV must be a deterministic function"))
    (if (ground? x)
        (apply f (mapcar #'value-of x))
      (let ((z (make-variable)))
        (screamer::assert!-constraint
         #'(lambda (&rest x) (equal (first x) (apply f (rest x)))) t (cons z x))
        (dolist (argument x)
          (screamer::attach-noticer!
           #'(lambda ()
               (if (ground? x)
                   (screamer::assert!-equalv z (apply f (mapcar #'value-of x)))))
           argument))
        z))))
#|(defun funcallv (f &rest x)
 (let ((f (value-of f)))
  (if (variable? f)
      (error "The current implementation does not allow the first argument~%~
              of FUNCALLV to be an unbound variable"))
  (unless (functionp f)
   (error "The first argument to FUNCALLV must be a deterministic function"))
  (if (every #'bound? x)
      (apply f (mapcar #'value-of x))
      (let ((z (make-variable)))
       (assert!-constraint
        #'(lambda (&rest x) (equal (first x) (apply f (rest x)))) t (cons z x))
       (dolist (argument x)
        (attach-noticer!
         #'(lambda ()
            (if (every #'bound? x)
                (assert!-equalv z (apply f (mapcar #'value-of x)))))
         argument))
       z))))|#