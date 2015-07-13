; 
; Graph representation of context-free grammars
; Alex Shkotin arXiv:cs/0703015 http://arxiv.org/abs/cs/0703015
; 
; jan 2013 
; 
(cl:defun mapprules-internal (list 
                              prules
                              &key continuation-mode
                                   ordered-partitions-nondeterministic-values-cap
                                   symbol-mode
                                   params)
  "reads a template list and a list of production rules, of the form S -> w where S is a nonterminal symbol and w is a sequence of terminal or nonterminal symbols, and attempts to express every possible combination of symbols that fit the template:
(all-values (solution (mapprules '(_ _ _ _ _)
                                 '((:S x)
                                   (:S y)
                                   (:S :S + :S)
				   (:S :S - :S)
                                   (:S [ :S ]))
                                   :symbol-mode t)
                      (static-ordering #'linear-force))) =
(([ [ y ] ]) ([ [ x ] ]) ([ y ] - y) ([ y ] - x) ([ y ] + y) ([ y ] + x) ([ y - y ]) ([ y - x ]) ([ y + y ]) ([ y + x ]) ([ x ] - y) ([ x ] - x) ([ x ] + y) ([ x ] + x) ([ x - y ]) ([ x - x ]) ([ x + y ]) ([ x + x ]) (y - [ y ]) (y - [ x ]) (y - y - y) (y - y - x) (y - y + y) (y - y + x) (y - x - y) (y - x - x) (y - x + y) (y - x + x) (y + [ y ]) (y + [ x ]) (y + y - y) (y + y - x) (y + y + y) (y + y + x) (y + x - y) (y + x - x) (y + x + y) (y + x + x) (x - [ y ]) (x - [ x ]) (x - y - y) (x - y - x) (x - y + y) (x - y + x) (x - x - y) (x - x - x) (x - x + y) (x - x + x) (x + [ y ]) (x + [ x ]) (x + y - y) (x + y - x) (x + y + y) (x + y + x) (x + x - y) (x + x - x) (x + x + y) (x + x + x))

(all-values (solution (mapprules '([ _ _ _ _)
                                 '((:S x)
                                   (:S y)
                                   (:S :S + :S)
				   (:S :S - :S)
                                   (:S [ :S ]))
                                   :symbol-mode t)
                      (static-ordering #'linear-force))) = 
 (([ [ y ] ]) ([ [ x ] ]) ([ y ] - y) ([ y ] - x) ([ y ] + y) ([ y ] + x) ([ y - y ]) ([ y - x ]) ([ y + y ]) ([ y + x ]) ([ x ] - y) ([ x ] - x) ([ x ] + y) ([ x ] + x) ([ x - y ]) ([ x - x ]) ([ x + y ]) ([ x + x ]))"
  (labels
      ((rule-label (sym) 
         (cond ((null sym) (gensym))
               ((not (stringp sym)) (rule-label (write-to-string sym)))
               ((and (> (length sym) 1)
                     (string= (subseq sym 0 1) ":"))
                (gensym (subseq sym 1 (length sym))))
               (t (gensym sym))))
       (znd-rule? (rule) 
         (> (length (remove-if-not #'(lambda (x) (eq x (car rule))) (mapcar #'car prules))) 1))
       (zd-rule? (rule)
         (= (length (remove-if-not #'(lambda (x) (eq x (car rule))) (mapcar #'car prules))) 1))
       (one-to-one-transform (rule)
         (cond ((and (znd-rule? rule)
                     (> (length (cdr rule)) 1))
                (let* ((sym (rule-label (car rule))))
                  (list (list (car rule) sym)
                        (append (list sym) (cdr rule)))))
               (t (list rule))))
       (init-label (string)
         (cond ((not (stringp string)) (init-label (write-to-string string)))
               ((= 0 (length string)) string)
               ((string= ":" (subseq string 0 1)) string)
               (t (concatenate 'string ":" string))))
       (symeq (x y) (eql x y))
       (underscore? (x) (symeq x '_)))
    (let* ((dnd (flat1 (mapcar #'one-to-one-transform prules)))
           (syms (remove-duplicates (flat dnd) :test #'symeq :from-end t))
           (nonterminals (mapcar #'car dnd))
           (terminals (remove-duplicates
                       (remove-if-not
                        #'(lambda (sym)
                            (null (intersection (list sym) nonterminals :test #'symeq)))
                        syms)
                       :test #'list-eq
                       :from-end t)))
      (let ((and-syms (remove-if-not
                       #'(lambda (sym)
                           (or (and (not (position sym terminals :test #'symeq))
                                    (= (length (remove-if-not #'(lambda (x) (symeq sym x)) nonterminals)) 1))))
                       nonterminals))
            (or-syms (remove-if-not
                      #'(lambda (sym)                         
                          (and (not (position sym terminals :test #'symeq))
                               (> (length (remove-if-not #'(lambda (x) (symeq sym x)) nonterminals)) 1)))
                      nonterminals))
            (dmgassoc (mapcar #'(lambda (sym) 
                                  (cons sym (make-instance 't2l::node :name sym)))
                              syms)))
        (labels 
            ((terminal-sym? (sym) (position sym terminals))             
             (or-sym? (sym) (position sym or-syms))
             (and-sym? (sym) (position sym and-syms)))
          (let ((dmg (cdar dmgassoc))
                (dmg-sym-assoc nil)
                (dmg-wordsize-assoc nil))
            (labels 
                ((make-edges (node)
                   (cond ((or-sym? (name node))
                          (mapcar #'(lambda (sym) 
                                      (push (cdr-assoc sym dmgassoc) (next-nodes node)))
                                  (remove-duplicates
                                   (flat1
                                    (mapcar 
                                     #'cdr
                                     (remove-if-not
                                      #'(lambda (x) (symeq (car x) (name node)))
                                      dnd)))
                                   :test #'symeq
                                   :from-end t)))
                         ((and-sym? (name node))
                          (mapcar #'(lambda (sym)
                                      (push-to-end (cdr-assoc sym dmgassoc) (next-nodes node)))
                                  (flat1
                                   (mapcar 
                                    #'cdr
                                    (remove-if-not
                                     #'(lambda (x) (symeq (car x) (name node)))
                                     dnd)))))
                         (t nil)))
                 (make-list-input-variable () 
                   (cond (symbol-mode (make-variable))
                         (t (make-variable)))))
              (mapcar #'make-edges (mapcar #'cdr dmgassoc))
              (dolist (n (mapcar #'car dmgassoc))
                (setf (cdr-assoc n dmg-sym-assoc) 
                      (mapcar #'name (next-nodes (cdr-assoc n dmgassoc)))))
              (labels
                  ((dmg-max-wordsize (sym)
                     (let ((syms nil))
                       (labels
                           ((findmax (s)
                              (cond
                               ((terminal-sym? s) 1)
                               ((position s syms) nil)
                               (t
                                (push s syms)
                                (let ((rs (mapcar #'findmax (cdr-assoc s dmg-sym-assoc))))
                                  (cond
                                   ((position nil rs) nil)
                                   (t
                                    (cond
                                     ((and-sym? s)
                                      (reduce #'+ rs))
                                     (t
                                      (reduce #'max rs))))))))))
                         (findmax sym))))
                   (dmg-min-wordsize (sym)
                     (let ((syms nil))
                       (labels
                           ((findmin (s)
                              (cond
                               ((terminal-sym? s) 1)
                               ((position s syms) nil)
                               (t
                                (push s syms)
                                (let ((rs (mapcar #'findmin (cdr-assoc s dmg-sym-assoc))))
                                  (cond
                                   ((and-sym? s)
                                    (reduce #'+ (mapcar #'(lambda (x) (if (null x) 1 x)) rs)))
                                   (t
                                    (reduce #'min (remove nil rs)))))))))
                         (findmin sym))))
                   (dmg-and-syms (or-sym)                     
                     (let ((syms nil))
                       (labels
                           ((findsyms (s)
                              (cond
                               ((position s syms) (list s))
                               (t
                                (push s syms)
                                (append
                                 (remove-if-not #'and-sym? (cdr-assoc s dmg-sym-assoc))
                                 (mapcar #'findsyms (remove-if-not #'or-sym? (cdr-assoc s dmg-sym-assoc))))
                                ))))
                        (let ((list (remove-duplicates (flat (findsyms or-sym)) :test #'symeq :from-end t)))
                          (if (and-sym? nil) list (remove nil list))))))
                   (dmg-sym-domain (or-sym)
                     (let ((syms nil))
                       (labels
                           ((findsyms (s)
                              (if (= *mess* -1) (print (format nil " > dmg-sym-domain findsyms s: ~A" s)))
                              (cond
                               ((position s syms) (list s))
                               (t
                                (push s syms)
                                (append
                                 (remove-if-not #'terminal-sym? (cdr-assoc s dmg-sym-assoc))
                                 (mapcar #'findsyms (remove-if-not #'or-sym? (cdr-assoc s dmg-sym-assoc))))))))
                        (remove-duplicates (flat (findsyms or-sym)) :test #'symeq :from-end t)))))
                (dolist (n (mapcar #'car dmgassoc))
                  (setf (cdr-assoc n dmg-wordsize-assoc)
                        (cons (dmg-min-wordsize n)
                              (dmg-max-wordsize n))))
                (labels
                   ((min-wordsize (sym) (car (cdr-assoc sym dmg-wordsize-assoc)))
                    (max-wordsize (sym) (cdr (cdr-assoc sym dmg-wordsize-assoc))))
                (let ((vars (funcall-rec
                             #'(lambda (x) 
                                 (cond ((null x) nil)
                                       ((screamer::variable? x) x)
                                       ((underscore? x) (make-list-input-variable))
                                       ((integerp x) (make-intv= x))
                                       (t (let ((var (make-list-input-variable)))
                                            (assert! (equalv var x))
                                            var))))
                             list))
                      (rule-card-assoc
                       (mapcar
                        #'(lambda (sym)
                            (append (list sym)
                                    (mapcar #'(lambda (x) (cdr-assoc x dmg-wordsize-assoc))
                                            (cdr-assoc sym dmg-sym-assoc))))
                        (remove-if-not #'and-sym? (mapcar #'car dmgassoc))))
                      (or-sym-xs-assoc nil)
                      (term-sym-xs-assoc nil)
                      (or-and-sym-assoc nil)
                      (or-sym-domain-assoc nil))
                  (dolist (x (remove-if-not #'terminal-sym? (mapcar #'car dmg-sym-assoc)))
                    (setf (cdr-assoc x term-sym-xs-assoc) nil))
                  (dolist (x (remove-if-not #'or-sym? (mapcar #'car dmg-sym-assoc)))
                    (let ((and-syms (dmg-and-syms x)))
                      (let ((cards (remove-duplicates
                                    (mapcar #'(lambda (y) (cdr-assoc y rule-card-assoc)) 
                                            (dmg-and-syms x))
                                    :test #'list-eq
                                    :from-end t)))
                        (setf (cdr-assoc x or-and-sym-assoc)
                              (mapcar
                               #'(lambda (c)
                                   (remove-if-not 
                                    #'(lambda (y) 
                                        (list-eq c (cdr-assoc y rule-card-assoc))) 
                                    and-syms))
                               cards)))))
                  (dolist (x (remove-if-not #'or-sym? (mapcar #'car dmg-sym-assoc)))
                    (setf (cdr-assoc x or-sym-domain-assoc) (dmg-sym-domain x)))
                  (let ((first-var (car vars))
                        (last-var (last-atom vars)))
                    (labels
                        ((csp-variable-name (x) 
                           (cond ((null x) nil)
                                 ((screamer::variable? x) (screamer::variable-name x))
                                 (t x)))
                         (ordered-partitions-of (list card)
                           (all-values
                             (let ((p (an-ordered-partition-of list)))
                               (unless (and (= (length card) (length p))
                                            (every
                                             #'(lambda (x y)
                                                 (and (or (null (car x))
                                                          (>= y (car x)))
                                                      (or (null (cdr x))
                                                          (<= y (cdr x)))))
                                             card
                                             (mapcar #'length p)))
                                 (fail))
                               p)))
                         (maprule (xs r)
                           (cond
                            ((null xs) nil)
                            ((listp r)
                             (reduce
                              #'orv
                              (mapcar
                               #'(lambda (p)
                                   (apply
                                    #'orv
                                    (mapcar
                                     #'(lambda (r1)
                                         (apply
                                          #'andv
                                          (mapcar
                                           #'(lambda (x y) (maprule x y))
                                           p
                                           (cdr-assoc r1 dmg-sym-assoc))))
                                     r)))
                               xs)))
                            ((and (terminal-sym? r)
                                  (not (cdr xs)))
                             (cond
                              ((assoc (car xs) (cdr-assoc r term-sym-xs-assoc))
                               (cdr-assoc (car xs) (cdr-assoc r term-sym-xs-assoc)))
                              (t
                               (setf (cdr-assoc (car xs) (cdr-assoc r term-sym-xs-assoc))                                     
                                     (cond
                                                 ;((cdr xs) nil)
                                      ((or symbol-mode
                                           (not (numberp r)))
                                       (equalv (car xs) r))
                                      (t 
                                       (=v (car xs) r))))
                               (cdr-assoc (car xs) (cdr-assoc r term-sym-xs-assoc)))))
                            ((terminal-sym? r) nil)
                            ((and-sym? r) 
                             (print (format nil " WARNING : ~A" r))
                             (let* ((xs-length (length xs))
                                    (rcard (cond
                                            ((and continuation-mode
                                                  (< xs-length (length (cdr-assoc r rule-card-assoc)))
                                                  (position last-var xs))
                                             (subseq (cdr-assoc r rule-card-assoc) 0 xs-length))
                                            (t
                                             (cdr-assoc r rule-card-assoc))))
                                    (rcard-length (length rcard)))
                               (cond
                                ((< xs-length rcard-length)
                                 nil)
                                (t 
                                 (let* ((xs-partitions (ordered-partitions-of xs rcard))
                                        (next-syms
                                         (cond
                                          ((and (= (length xs) 1)
                                                continuation-mode
                                                (eq (car xs) last-var))
                                           (remove-if #'or-sym? (cdr-assoc r dmg-sym-assoc)))
                                          (t (cdr-assoc r dmg-sym-assoc)))))
                                   (unless (null xs-partitions)
                                     (let ((vs (mapcar
                                                #'(lambda (p) ; ordered partition of xs
                                                    (apply 
                                                     #'andv
                                                     (mapcar
                                                      #'(lambda (x y) (maprule x y))
                                                      p
                                                      next-syms)))
                                                xs-partitions)))
                                       (if (> (length xs-partitions) 1)
                                           (reduce #'orv vs)
                                         (car vs)))))))))
                            ((or-sym? r)
                             (cond ((cdr xs)
                                    (apply
                                     #'orv
                                     (mapcar
                                      #'(lambda (x) (maprule (ordered-partitions-of xs (cdr-assoc (car x) rule-card-assoc)) x))
                                      (cdr-assoc r or-and-sym-assoc))))
                                   (t 
                                    (let ((existing-var (find (cons r xs) or-sym-xs-assoc :key #'car :test #'list-eq)))
                                      (cond (existing-var (cadr existing-var))
                                            (t 
                                             (let ((var (reduce 
                                                         #'orv
                                                         (mapcar
                                                          #'(lambda (term) (maprule xs term))
                                                          (cdr-assoc r or-sym-domain-assoc)))))
                                               (push (list (cons r xs) var) or-sym-xs-assoc)
                                               var)))))))
                            (t nil))))
                      (values (andv (all-memberv vars terminals)
                                    (maprule vars (name dmg))) 
                              vars                                                                                 
                              dmg)))))))))))))

(defstruct (dmg-cursor (:conc-name nil) (:print-function print-dmg-cursor)) label sym stack rules)

(defvar *mapprules-default-input-process-increment* 4)
(cl:defun mapprules (input 
                     prules 
                     &key input-process-increment
                          continue
                          init
                          listdxx
                          max
                          min
                          ordered-partitions-nondeterministic-values-cap
                          superset
                          symbol-mode
                          params)
  (assert (not (and symbol-mode listdxx)))
  (labels
      ((init-label (string)
         (cond ((not (stringp string)) (init-label (write-to-string string)))
               ((= 0 (length string)) string)
               ((string= ":" (subseq string 0 1)) string)
               (t (concatenate 'string ":" string))))
       (symeq (x y)
         (let ((xS (init-label x))
               (yS (init-label y)))
           (string= xS yS)))
       (underscore? (x) (symeq x "_"))
       (atom->var (x)
         (cond ((null x) nil) 
               ((screamer::variable? x) x)
               ((underscore? x) 
                (cond (symbol-mode (make-variable))
                      (t (an-integerv))))
               ((integerp x) (make-intv= x))
               ((floatp x) (make-realv= x))
               ((numberp x) (make-numberv= x))
               (t
                (let ((v (make-variable)))                  
                  (assert! (equalv v x))
                  v))))
       (process-increment-adjusted-length (length) 
         (if input-process-increment
             (* (ceiling (/ length (if (numberp input-process-increment)
                                       input-process-increment
                                     *mapprules-default-input-process-increment*)))
                (if (numberp input-process-increment)
                    input-process-increment
                  *mapprules-default-input-process-increment*))
           length))
       (make-input-sequence (length)
         (make-sequence 'list (process-increment-adjusted-length length) :initial-element '_)))
    (cond
     ((or (null input) (and (numberp input) (= 0 input)) (and (listp input) (every #'null (flat input)))) input)
     (t
      (let* ((list (cond ((null input) nil)
                         ((listp input) input)
                         ((numberp input) (make-input-sequence input))
                         (t input)))
             (list-vars (funcall-rec #'atom->var list))
             (list-vars-flat-subseq (if init
                                        (append (list init) (remove nil (flat list-vars)))
                                      (remove nil (flat list-vars))))
             (list-vars-flat (if (and input-process-increment
                                      (< (length list-vars-flat-subseq) (process-increment-adjusted-length (length list-vars-flat-subseq))))
                                 (append list-vars-flat-subseq 
                                         (mapcar #'atom->var (make-sequence 'list (- (process-increment-adjusted-length (length list-vars-flat-subseq)) (length list-vars-flat-subseq)) :initial-element '_)))
                               list-vars-flat-subseq))
             (map-fn-input (if listdxx
                               (listdxv (append list-vars-flat (list (atom->var '_))))
                             list-vars-flat))
             (c nil))        

        (if (not symbol-mode)
            (progn 
              (if min (push (reduce-chunks #'andv (mapcar #'(lambda (x) (>=v x min)) list-vars-flat) :default t) c))
              (if max (push (reduce-chunks #'andv (mapcar #'(lambda (x) (<=v x max)) list-vars-flat) :default t) c))))
        (if superset (push (reduce-chunks #'andv (mapcar #'(lambda (x) (memberv x superset)) list-vars-flat) :default t) c))

        (cond
         (input-process-increment
          (let ((n (if (numberp input-process-increment) 
                       input-process-increment
                     *mapprules-default-input-process-increment*)))
            (let ((map-fn-input-chunks (nsucc map-fn-input n :step (1- n))))
              (let (incs)
                (dotimes (i (length map-fn-input-chunks))
                  (multiple-value-bind
                      (mvar list)
                      (mapprules-internal (elt map-fn-input-chunks i)
                                          prules
                                          :continuation-mode continue
                                          :symbol-mode symbol-mode
                                          :ordered-partitions-nondeterministic-values-cap ordered-partitions-nondeterministic-values-cap
                                          :params params)
                    (push mvar c)))
                (cond ((numberp input)
                       (assert! (reduce-chunks #'andv c :default t))
                       (subseq list-vars 0 input))
                      (t
                       (values (reduce-chunks #'andv c :default t)
                               list-vars)))))))
         (t 
          (multiple-value-bind 
              (var list dmg)
              (mapprules-internal map-fn-input                                            
                                  prules
                                  :continuation-mode continue
                                  :symbol-mode symbol-mode
                                  :ordered-partitions-nondeterministic-values-cap ordered-partitions-nondeterministic-values-cap
                                  :params params) 
            (values var list-vars dmg)))))))))