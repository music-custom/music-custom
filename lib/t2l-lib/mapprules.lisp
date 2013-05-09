(in-package :t2l)


; Graph representation of context-free grammars
; Alex Shkotin arXiv:cs/0703015 http://arxiv.org/abs/cs/0703015
; 
; jan 2013 
;                                                         ocannon@gmail.com
(cl:defun mapprules-internal (list 
                              prules
                              &key continuation-mode
                                   ordered-partitions-nondeterministic-values-cap
                                   symbol-mode
                                   params
                                   print-graph-info)
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
            (if print-graph-info
                (dolist (x (mapcar #'car dmgassoc))
                  (print (format nil 
                                 "~A: terminal? ~A or-sym? ~A and-sym? ~A syms: ~A" 
                                 x
                                 (terminal-sym? x)
                                 (or-sym? x)
                                 (and-sym? x)
                                 (mapcar #'name (next-nodes (cdr-assoc x dmgassoc)))))))
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
                                     ((and-sym? s) (reduce #'+ rs))
                                     (t (reduce #'max rs))))))))))
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
                                   ((and-sym? s) (reduce #'+ (mapcar #'(lambda (x) (if (null x) 1 x)) rs)))
                                   ((null (remove nil rs)) 1)
                                   (t (reduce #'min (mapcar #'(lambda (x) (if (null x) 1 x)) rs)))))))))
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
                  (if print-graph-info
                      (progn
                        (print (format nil "~A graph symbols" (length dmgassoc)))
                        (dolist (x (mapcar #'car dmgassoc))
                          (print (format nil
                                         "~A min-wordsize: ~A max-wordsize: ~A"
                                         x
                                         (min-wordsize x)
                                         (max-wordsize x))))))
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
                      (setf (cdr-assoc x or-sym-domain-assoc) (remove nil (dmg-sym-domain x))))
                    (if print-graph-info
                        (progn
                          (print (format nil "___________~%~A" rule-card-assoc))
                          (mapcar #'(lambda (s) (print (format nil "sym: ~A  ~A" s (cdr-assoc s rule-card-assoc))))
                                (mapcar #'car rule-card-assoc))))
                    (let ((first-var (car vars))
                          (last-var (last-atom vars))
                          (temporary-sym-assoc (make-hash-table :test #'equalp)))
                      (labels
                          ((csp-variable-name (x) 
                             (cond ((null x) nil)
                                   ((screamer::variable? x) (screamer::variable-name x))
                                   (t x)))
                           (ordered-partitions-of (list card)
                             (all-values (n-partitions-of2 card list)))
                           (maprule (xs r)
                             (cond
                              ((null xs) nil)
                              ((null r) (error "maprule called with NIL as a rule: ~A" xs))
                              ((listp r)
                               (reduce-chunks
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
                              ((gethash (cons xs r) temporary-sym-assoc)
                               (if (>= *mess* 30) (print (format nil "mapprules maprule existing variable found for sym: ~A for ~A vars (~A ...)" r (length xs) (car xs))))
                               (gethash (cons xs r) temporary-sym-assoc))
                              ((and (terminal-sym? r) (cdr xs)) nil)
                              ((terminal-sym? r)
                               (setf (gethash (cons xs r) temporary-sym-assoc)
                                     (cond
                                      ((or symbol-mode
                                           (not (numberp r)))
                                       (equalv (car xs) r))
                                      (t 
                                       (=v (car xs) r)))))
                              ((and-sym? r) 
                               (print (format nil " WARNING obsolete AND routine triggered by symbol: ~A" r))
                               (setf (gethash (cons xs r) temporary-sym-assoc)
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
                                                 (car vs))))))))))
                              ((or-sym? r)
                               (if (position nil (cdr-assoc r or-and-sym-assoc)) (error (format nil "or-sym r: ~A ~A" r  (cdr-assoc r or-and-sym-assoc))))
                               (setf (gethash (cons xs r) temporary-sym-assoc)
                                     (cond 
                                      ((cdr xs)
                                       (apply
                                        #'orv
                                        (mapcar
                                         #'(lambda (x) (maprule (ordered-partitions-of xs (cdr-assoc (car x) rule-card-assoc)) x))
                                         (cdr-assoc r or-and-sym-assoc))))
                                      ((cdr-assoc r or-sym-domain-assoc) 
                                       (reduce-chunks #'orv (mapcar #'(lambda (r1) (maprule xs r1)) (cdr-assoc r or-sym-domain-assoc))))
                                      (t (error "bad call to OR clause with r: ~A xs; ~A" r xs)))))
                              (t nil))))
                        (values (cond (symbol-mode 
                                       (andv (all-memberv vars terminals)
                                             (maprule vars (name dmg))))
                                      (t
                                       (maprule vars (name dmg))))
                                vars                                                                                 
                                dmg)))))))))))))

(defstruct (dmg-cursor (:conc-name nil) (:print-function print-dmg-cursor)) label sym stack rules)

(defvar *mapprules-default-input-process-increment* 4)
(define-box mapprules (input
                       prules 
                       &key process-chunk-size
                            input-process-increment
                            continue
                            init
                            listdxx
                            max
                            min
                            ordered-partitions-nondeterministic-values-cap
                            superset
                            symbol-mode
                            params
                            print-graph-info)
  :outputs 3
  :indoc '("input template list, screamer variable list or number" 
           "production rules"
           "process-chunk-size"
           "input-process-increment (use process-chunk-size)"
           "continue"
           "init"
           "listdxx"
           "max"
           "min"
           "ordered-partitions-nondeterministic-values-cap"
           "superset"
           "symbol-mode"
           "params"
           "print-graph-info")
  :icon 324
  (assert (not (and symbol-mode listdxx)))
  (if (and (null process-chunk-size) input-process-increment)
      (setf process-chunk-size input-process-increment))
  (setf process-chunk-size
        (cond ((and (null process-chunk-size) input-process-increment) input-process-increment)
              (t process-chunk-size)))
  (setf process-chunk-size
        (cond ((null process-chunk-size) nil)
              ((and process-chunk-size 
                    (< (if (numberp input) input (length input)) process-chunk-size)) nil)               
              ((numberp process-chunk-size) process-chunk-size)
              (t *mapprules-default-input-process-increment*)))
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
       (underscore? (x) (or (symeq x "_") (symeq x "t2l::_")))
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
         (if process-chunk-size
             (* (ceiling (/ length process-chunk-size)) process-chunk-size)
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
             (list-vars-flat (if (and process-chunk-size
                                      (< (length list-vars-flat-subseq) (process-increment-adjusted-length (length list-vars-flat-subseq))))
                                 (append list-vars-flat-subseq 
                                         (mapcar #'atom->var (make-sequence 'list (- (process-increment-adjusted-length (length list-vars-flat-subseq)) (length list-vars-flat-subseq)) :initial-element '_)))
                               list-vars-flat-subseq))
             (map-fn-input (if listdxx (listdxv list-vars-flat) list-vars-flat)))
        (if (not symbol-mode)
            (progn 
              (if min (push (reduce-chunks #'andv (mapcar #'(lambda (x) (>=v x min)) list-vars-flat) :default t) c))
              (if max (push (reduce-chunks #'andv (mapcar #'(lambda (x) (<=v x max)) list-vars-flat) :default t) c))))
        (if superset (push (reduce-chunks #'andv (mapcar #'(lambda (x) (memberv x superset)) list-vars-flat) :default t) c))
        (cond
         (process-chunk-size
          (let ((map-fn-input-chunks (let ((nsucc (nsucc map-fn-input process-chunk-size :step (1- process-chunk-size))))
                                       (cond ((and (> (length nsucc) 1)
                                                   (= (length (car (reverse nsucc))) 1)) (butlast nsucc))
                                             (t nsucc)))))
            (if (>= *mess* 5) (print (format nil "map-fn-input-chunks: ~A" map-fn-input-chunks)))              
            (let ((var-input-dmg-list 
                   (maplist
                    #'(lambda (chunks)
                        (if (>= *mess* 5) (print (format nil "mapprules: chunk ~A / ~A" (1+ (- (length map-fn-input-chunks) (length chunks))) (length map-fn-input-chunks))))
                        (multiple-value-list
                         (mapprules-internal (car chunks)
                                             prules
                                             :continuation-mode t
                                             :symbol-mode symbol-mode
                                             :ordered-partitions-nondeterministic-values-cap ordered-partitions-nondeterministic-values-cap
                                             :params params
                                             :print-graph-info print-graph-info)))
                    map-fn-input-chunks)))
              (values (apply #'andv (mapcar #'car var-input-dmg-list))
                      (cadar var-input-dmg-list)
                      (caddar var-input-dmg-list)))))
         (t 
          (multiple-value-bind 
              (var list dmg)
              (mapprules-internal map-fn-input                                            
                                  prules
                                  :continuation-mode continue
                                  :symbol-mode symbol-mode
                                  :ordered-partitions-nondeterministic-values-cap ordered-partitions-nondeterministic-values-cap
                                  :params params
                                  :print-graph-info print-graph-info) 
            (values var list-vars dmg)))))))))