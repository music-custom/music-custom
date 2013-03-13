(in-package :t2l)








; OMTristan 3.0 TMlibrairie-OM.lisp
;===Rename escalier as stairs 26-06-2007===================
(define-box stairs ((list (1 2 3 4 5 6)) (step 2))
  :indoc '("List" "Step")
  :icon 136
  :doc   "permute en escalier : si pas = 2 ,(1 2 3 4 5 6)  devient (1 3 2 4 3 5 4 6)"
  (let (c)
    (dotimes (i (- (length list)  step))
      (push (nth i list) c)
      (push (nth (+ i pas) list) c))
    (nreverse c)))