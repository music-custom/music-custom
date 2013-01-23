(in-package :t2l)
(setf *mess* 0)
(setf *mess* 10)
(setf *mess* 20)
(progn 
  (setq mapprules-input-process-increment 9)
  (setq mapprules-nondeterministic-values-cap 6)
  (setq init (an-integer-betweenv 55 85)))
(setq midicrules '((:S 3 -3 :C :B)
                   (:C 2 3)
                   (:C -5)
                   (:S :S :S)
                   (:B -5)
                   (:B -3)
                   (:B 0)
                   (:B 3)
                   (:B 5)))
(setq timerules '((:S 1 1 :C 1)
                  (:C 2 3)
                  (:C 5)
                  (:S :S :S)))
(setq vars (mapprules 20
                      midicrules
                      :init init
                      :listdxx t
                      :ordered-partitions-nondeterministic-values-cap mapprules-nondeterministic-values-cap
                      :input-process-increment mapprules-input-process-increment))