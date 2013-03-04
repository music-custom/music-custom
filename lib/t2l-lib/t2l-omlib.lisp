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
  (let ((init-seqc (mapcar #'remove-nil (reverse (mapcar #'flat seqc))))
        (init-rhythm-trees (reverse rhythm-trees)))
    (if (>= *mess* 20)
        (progn
          (lprint 'seqc->voices 'init-seqc init-seqc)
          (lprint 'seqc->voices 'init-rhythm-trees init-rhythm-trees)))
    (mapcar #'(lambda (x y) 
                (let ((v (make-instance 'om::voice
                                        :chords (om* (flat x) 100)
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

(define-box concat-rec ((obj-list list))
  :initvals '(nil )    ; an initial values list
  :indoc '("" ) ; an string list with short docs
  :icon 230 ; the icon
  :doc ""
  (cond ((null obj-list) nil)
        ((atom obj-list) obj-list)
        ((> (length obj-list) 2) (om::concat (car obj-list) (concat-rec (cdr obj-list))))
        ((= (length obj-list) 2) (om::concat (car obj-list) (cadr obj-list)))
        (t (car obj-list))))