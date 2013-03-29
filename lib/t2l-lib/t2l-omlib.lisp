(in-package :t2l)

(defun tree->signatures (rt)
  (mapcar (lambda (x) 
            (read-from-string 
             (concatenate 'string 
                          "(" 
                          (replace-all (replace-all (write-to-string x) "//" " ") 
                                       "|" 
                                       "") 
                          ")")))
          (om::get-signatures rt)))

(define-box seqc->voices ((seqc list) (rhythm-trees list) &key bpm)
  :initvals '(((60)) ((? (((4 4) (-1)))) -1))
  :indoc '("seqc" "trees")
  :icon 225
  :doc ""
  (let ((init-seqc (mapcar #'(lambda (x) (remove nil x)) (reverse (mapcar #'flat seqc))))
        (init-rhythm-trees (reverse rhythm-trees)))
    (mapcar #'(lambda (x y) 
                (let ((v (make-instance 'om::voice
                                        :chords (if (some #'(lambda (y) (> y 127)) (flat x)) (flat x) (om* (flat x) 100))
                                        :tree y)))
                  (if bpm (setf (om::tempo v) bpm))
                  v))
            init-seqc
            init-rhythm-trees)))

(define-box seqc->poly ((seqc ((60))) (rhythm-trees ((? (((4 4) (-1)))) -1)) &key bpm)
  :indoc '("seqc" "trees")
  :icon 225
  :doc ""
  (make-instance 'om::poly 
                 :voices (seqc->voices seqc rhythm-trees :bpm bpm)))

(define-box concat-list ((obj-list list))
  :icon 230
  (reduce #'om:concat obj-list))

(define-box concat-rec ((obj-list list))
  :icon 230
  (concat-list obj-list))


(cl:defun adjust-timelist (list total)
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

(defun prcs-ms-timepoint-sigs (ms partns &key list-mode)
  (if (car ms)
      (append (list (append (list (cond (list-mode ms)
                                        (t (format-mssign (car ms)))))
			    (list (car partns)))) 
	      (prcs-ms-timepoint-sigs (cdr ms) (cdr partns)))
    nil))