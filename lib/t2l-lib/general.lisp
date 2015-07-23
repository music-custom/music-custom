(in-package :t2l)

(defvar *infinity* 1d38)
(defvar *-infinity* -1d38)
(defvar *prime-numbers*)
(setf *prime-numbers*
#(1 2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71 73 79 83 89 97 101 103 107 109 113 127 131 137 139
  149 151 157 163 167 173 179 181 191 193 197 199 211 223 227 229 233 239 241 251 257 263 269 271 277 281 283
  293 307 311 313 317 331 337 347 349 353 359 367 373 379 383 389 397 401 409 419 421 431 433 439 443 449 457
  461 463 467 479 487 491 499 503 509 521 523 541 547 557 563 569 571 577 587 593 599 601 607 613 617 619 631
  641 643 647 653 659 661 673 677 683 691 701 709 719 727 733 739 743 751 757 761 769 773 787 797 809 811 821
  823 827 829 839 853 857 859 863 877 881 883 887 907 911 919 929 937 941 947 953 967 971 977 983 991 997 1009
  1013 1019 1021 1031 1033 1039 1049 1051 1061 1063 1069 1087 1091 1093 1097 1103 1109 1117 1123 1129 1151 1153
  1163 1171 1181 1187 1193 1201 1213 1217 1223 1229 1231 1237 1249 1259 1277 1279 1283 1289 1291 1297 1301 1303
  1307 1319 1321 1327 1361 1367 1373 1381 1399 1409 1423 1427 1429 1433 1439 1447 1451 1453 1459 1471 1481 1483
  1487 1489 1493 1499 1511 1523 1531 1543 1549 1553 1559 1567 1571 1579 1583 1597 1601 1607 1609 1613 1619 1621
  1627 1637 1657 1663 1667 1669 1693 1697 1699 1709 1721 1723 1733 1741 1747 1753 1759 1777 1783 1787 1789 1801
  1811 1823 1831 1847 1861 1867 1871 1873 1877 1879 1889 1901 1907 1913 1931 1933 1949 1951 1973 1979 1987 1993
  1997 1999 2003 2011 2017 2027 2029 2039 2053 2063 2069 2081 2083 2087 2089 2099 2111 2113 2129 2131 2137 2141
  2143 2153 2161 2179 2203 2207 2213 2221 2237 2239 2243 2251 2267 2269 2273 2281 2287 2293 2297 2309 2311 2333
  2339 2341 2347 2351 2357 2371 2377 2381 2383 2389 2393 2399 2411 2417 2423 2437 2441 2447 2459 2467 2473 2477
  2503 2521 2531 2539 2543 2549 2551 2557 2579 2591 2593 2609 2617 2621 2633 2647 2657 2659 2663 2671 2677 2683
  2687 2689 2693 2699 2707 2711 2713 2719 2729 2731 2741 2749 2753 2767 2777 2789 2791 2797 2801 2803 2819 2833
  2837 2843 2851 2857 2861 2879 2887 2897 2903 2909 2917 2927 2939 2953 2957 2963 2969 2971 2999 3001 3011 3019
  3023 3037 3041 3049 3061 3067 3079 3083 3089 3109 3119 3121 3137 3163 3167 3169 3181 3187 3191 3203 3209 3217
  3221 3229 3251 3253 3257 3259 3271 3299 3301 3307 3313 3319 3323 3329 3331 3343 3347 3359 3361 3371 3373 3389
  3391 3407 3413 3433 3449 3457 3461 3463 3467 3469 3491 3499 3511 3517 3527 3529 3533 3539 3541 3547 3557 3559
  3571 3581 3583 3593 3607 3613 3617 3623 3631 3637 3643 3659 3671 3673 3677 3691 3697 3701 3709 3719 3727 3733
  3739 3761 3767 3769 3779 3793 3797 3803 3821 3823 3833 3847 3851 3853 3863 3877 3881 3889 3907 3911 3917 3919
  3923 3929 3931 3943 3947 3967 3989 4001 4003 4007 4013 4019 4021 4027 4049 4051 4057 4073 4079 4091 4093 4099
  4111 4127 4129 4133 4139 4153 4157 4159 4177 4201 4211 4217 4219 4229 4231 4241 4243 4253 4259 4261 4271 4273
  4283 4289 4297 4327 4337 4339 4349 4357 4363 4373 4391 4397 4409 4421 4423 4441 4447 4451 4457 4463 4481 4483
  4493 4507 4513 4517 4519 4523 4547 4549 4561 4567 4583 4591 4597 4603 4621 4637 4639 4643 4649 4651 4657 4663
  4673 4679 4691 4703 4721 4723 4729 4733 4751 4759 4783 4787 4789 4793 4799 4801 4813 4817 4831 4861 4871 4877
  4889 4903 4909 4919 4931 4933 4937 4943 4951 4957 4967 4969 4973 4987 4993 4999 5003 5009 5011 5021 5023 5039
  5051 5059 5077 5081 5087 5099 5101 5107 5113 5119 5147 5153 5167 5171 5179 5189 5197 5209 5227 5231 5233 5237
  5261 5273 5279 5281 5297 5303 5309 5323 5333 5347 5351 5381 5387 5393 5399 5407 5413 5417 5419 5431 5437 5441
  5443 5449 5471 5477 5479 5483 5501 5503 5507 5519 5521 5527 5531 5557 5563 5569 5573 5581 5591 5623 5639 5641
  5647 5651 5653 5657 5659 5669 5683 5689 5693 5701 5711 5717 5737 5741 5743 5749 5779 5783 5791 5801 5807 5813
  5821 5827 5839 5843 5849 5851 5857 5861 5867 5869 5879 5881 5897 5903 5923 5927 5939 5953 5981 5987 6007 6011
  6029 6037 6043 6047 6053 6067 6073 6079 6089 6091 6101 6113 6121 6131 6133 6143 6151 6163 6173 6197 6199 6203
  6211 6217 6221 6229 6247 6257 6263 6269 6271 6277 6287 6299 6301 6311 6317 6323 6329 6337 6343 6353 6359 6361
  6367 6373 6379 6389 6397 6421 6427 6449 6451 6469 6473 6481 6491 6521 6529 6547 6551 6553 6563 6569 6571 6577
  6581 6599 6607 6619 6637 6653 6659 6661 6673 6679 6689 6691 6701 6703 6709 6719 6733 6737 6761 6763 6779 6781
  6791 6793 6803 6823 6827 6829 6833 6841 6857 6863 6869 6871 6883 6899 6907 6911 6917 6947 6949 6959 6961 6967
  6971 6977 6983 6991 6997 7001 7013 7019 7027 7039 7043 7057 7069 7079 7103 7109 7121 7127 7129 7151 7159 7177
  7187 7193 7207 7211 7213 7219 7229 7237 7243 7247 7253 7283 7297 7307 7309 7321 7331 7333 7349 7351 7369 7393
  7411 7417 7433 7451 7457 7459 7477 7481 7487 7489 7499 7507 7517 7523 7529 7537 7541 7547 7549 7559 7561 7573
  7577 7583 7589 7591 7603 7607 7621 7639 7643 7649 7669 7673 7681 7687 7691 7699 7703 7717 7723 7727 7741 7753
  7757 7759 7789 7793 7817 7823 7829 7841 7853 7867 7873 7877 7879 7883 7901 7907 7919 7927 7933 7937 7949 7951
  7963 7993 8009 8011 8017 8039 8053 8059 8069 8081 8087 8089 8093 8101 8111 8117 8123 8147 8161 8167 8171 8179
  8191 8209 8219 8221 8231 8233 8237 8243 8263 8269 8273 8287 8291 8293 8297 8311 8317 8329 8353 8363 8369 8377
  8387 8389 8419 8423 8429 8431 8443 8447 8461 8467 8501 8513 8521 8527 8537 8539 8543 8563 8573 8581 8597 8599
  8609 8623 8627 8629 8641 8647 8663 8669 8677 8681 8689 8693 8699 8707 8713 8719 8731 8737 8741 8747 8753 8761
  8779 8783 8803 8807 8819 8821 8831 8837 8839 8849 8861 8863 8867 8887 8893 8923 8929 8933 8941 8951 8963 8969
  8971 8999 9001 9007 9011 9013 9029 9041 9043 9049 9059 9067 9091 9103 9109 9127 9133 9137 9151 9157 9161 9173
  9181 9187 9199 9203 9209 9221 9227 9239 9241 9257 9277 9281 9283 9293 9311 9319 9323 9337 9341 9343 9349 9371
  9377 9391 9397 9403 9413 9419 9421 9431 9433 9437 9439 9461 9463 9467 9473 9479 9491 9497 9511 9521 9533 9539
  9547 9551 9587 9601 9613 9619 9623 9629 9631 9643 9649 9661 9677 9679 9689 9697 9719 9721 9733 9739 9743 9749
  9767 9769 9781 9787 9791 9803 9811 9817 9829 9833 9839 9851 9857 9859 9871 9883 9887 9901 9907 9923 9929 9931
  9941 9949 9967 9973))

(cl:defun funcall-rec (fn tree &key level-min level-max cons-mode)
  "recursively applies fn to atoms in tree"
  (cond
   (cons-mode
    (funcall-rec-internal2 fn tree 0 level-min level-max))
   (t
    (funcall-rec-internal fn tree 0 level-min level-max))))

(cl:defun funcall-rec-internal (fn tree level level-min level-max)
  (cond ((null tree) nil)
        ((consp (car tree))
         (cons (funcall-rec-internal fn (car tree) (1+ level) level-min level-max)
               (funcall-rec-internal fn (cdr tree) level level-min level-max)))
        (t (cons (if (and (or (null level-min)
                              (>= level level-min))
                          (or (null level-max)
                              (<= level level-max)))
                     (funcall fn (car tree))
                   (car tree))
                 (funcall-rec-internal fn (cdr tree) level level-min level-max)))))

(cl:defun funcall-rec-internal2 (fn tree level level-min level-max)
  (cond ((null tree) nil)
        ((consp (car tree))
         (cons (if (and (or (null level-min)
                            (>= level level-min))
                        (or (null level-max)
                            (<= level level-max)))
                   (funcall fn (car tree))
                 (funcall-rec-internal2 fn (car tree) (1+ level) level-min level-max))
               (funcall-rec-internal2 fn (cdr tree) level level-min level-max)))
        (t (cons (if (and (or (null level-min)
                              (>= level level-min))
                          (or (null level-max)
                              (<= level level-max)))
                     (funcall fn (car tree))
                   (car tree))
                 (funcall-rec-internal2 fn (cdr tree) level level-min level-max)))))

(defun funcall-nondeterministic-rec (fn tree &key level-min level-max cons-mode)
  "recursively applies fn to atoms in tree"
  (cond
   (cons-mode
    (funcall-nondeterministic-rec-internal2 fn tree 0 level-min level-max))
   (t
    (funcall-nondeterministic-rec-internal fn tree 0 level-min level-max))))

(defun funcall-nondeterministic-rec-internal (fn tree level level-min level-max)
  (cond ((null tree) nil)
        ((consp (car tree))
         (cons (funcall-nondeterministic-rec-internal fn (car tree) (1+ level) level-min level-max)
               (funcall-nondeterministic-rec-internal fn (cdr tree) level level-min level-max)))
        (t (cons (if (and (or (null level-min)
                              (>= level level-min))
                          (or (null level-max)
                              (<= level level-max)))
                     (funcall-nondeterministic fn (car tree))
                   (car tree))
                 (funcall-nondeterministic-rec-internal fn (cdr tree) level level-min level-max)))))

(defun funcall-nondeterministic-rec-internal2 (fn tree level level-min level-max)
  (cond ((null tree) nil)
        ((consp (car tree))
         (cons (if (and (or (null level-min)
                            (>= level level-min))
                        (or (null level-max)
                            (<= level level-max)))
                   (funcall fn (car tree))
                 (funcall-nondeterministic-rec-internal2 fn (car tree) (1+ level) level-min level-max))
               (funcall-nondeterministic-rec-internal2 fn (cdr tree) level level-min level-max)))
        (t (cons (if (and (or (null level-min)
                              (>= level level-min))
                          (or (null level-max)
                              (<= level level-max)))
                     (funcall fn (car tree))
                   (car tree))
                 (funcall-nondeterministic-rec-internal2 fn (cdr tree) level level-min level-max)))))

(cl:defun set-equal (list1 list2 &key (key #'cl:identity) (test #'cl:eq))
  (null (set-difference list1 list2 :key key :test test)))

(cl:defun permut-random (input)
  (labels
      ((takeout (i list)
        (cond ((= i 0) (subseq list 1 (length list)))
              ((= i (1- (length list))) (butlast list))
              (t (append
                  (subseq list 0 i)
                  (subseq list (1+ i) (length list)))))))
    (let ((list (copy-seq input))
          (r nil))
      (loop for i from 0 while (< i (length input)) do
            (unless (= 0 (length list))
              (let ((j (random (length list))))
                (push (elt list j) r)
                (setf list (takeout j list)))))
      r)))

(defun mapr (fn tree) ; ?
  (cond ((null tree) nil)
        ((listp tree)
         (append (list (cond ((some #'listp tree) (mapr fn (car tree)))
                             (t (funcall fn tree))))
                 (mapr fn (cdr tree))))
        (t tree)))
(cl:defun funcall-chain (input &rest fns)
  (cond ((null fns) input)
        (t (apply 
            #'funcall-chain 
            (append (list (funcall (car fns) input))
                    (cdr fns))))))

(cl:defun apply-chain (input &rest fns)
  (cond ((null fns) input)
        (t (apply #'apply-chain
                  (append (list (apply (car fns) input))
                          (cdr fns))))))

(cl:defun map-sublists (fn tree &key map-leafs)
  "applies fn to lists within tree that do not contain lists"
  (cond ((null tree) nil)
        ((and (listp tree)
              (notany #'listp tree))
         (funcall fn tree))
        ((listp tree)
         (append (list
                  (map-sublists fn (car tree)))
                 (map-sublists fn (cdr tree))))
        (t tree))) 

(cl:defun first-atom (l)
  (cond ((null l) nil)
        ((atom l) l)
        (t (first-atom (car l)))))

(cl:defun last-atom (l)
  (cond ((null l) nil)
        ((atom l) l)
        (t (last-atom (car (reverse l))))))

(cl:defun lprint (&rest something)
  (print something))
  
(cl:defun lastcar (list)
  (car (last list)))

(cl:defun list-elt (list idx)
  (mapcar #'(lambda (x) (elt list x)) idx))

(cl:defun om-multiple-value-list (form)
  (multiple-value-list (funcall form)))

(defmacro push-to-end (item place)
  `(setf ,place (nconc ,place (list ,item))))
;(cl:defun push-to-end (item place)
;  (reverse (push item (reverse place))))

(cl:defun plist-key-posn (list)
  (let (ki)
    (loop for i from 0 while (< i (length list))
          do (let ((e (elt list i)))
               (if (and (symbolp e)
                        (or (= i 0)
                            (not (position (1- i) ki))))
                   (push-to-end i ki))))
    ki))

(cl:defun plist-subseq (list)
  "collects unattached elements of a plist"
 (flat1 
  (loop for i in (plist-key-posn list)
        collect (list (elt list i)
                      (elt list (1+ i))))))

(cl:defun non-plist-subseq (list)
  "collects unattached elements of a plist"
  (let ((ki (plist-key-posn list))
        (c))
    (loop for i from 0 
          while (< i (length list))
          do (let ((e (elt list i)))
               (if (and (not (position i ki)) ; not a key
                        (not (position (1- i) ki))) ; not directly ahead of a key
                   (push-to-end e c))))
    c))

(cl:defun strip-plist-sections (input)
  (cond ((null input) nil)
        ((listp input) (mapcar #'strip-plist-sections (non-plist-subseq input)))
        (t input)))

(cl:defun om-random (low high)
  (let ((l (if low low 0))
        (h (if high high 10)))
    (+ (random (abs (- h l))) l)))

(cl:defun rotate-internal (list &optional (n 1))
  (cond ((null list) nil)
        ((> n 0) (rotate-internal 
                  (append (cdr list) (list (car list)))
                  (1- n)))
        (t list)))

(cl:defun rotate (list &optional n)
  (rotate-internal list (mod (if n n 1) (length list))))

(cl:defun split-list (list &key (test #'numberp))
  (let (a)
    (dotimes (i (length list))
      (let ((e (elt list i)))
        (cond ((funcall test e)
               (push i a))
              (t nil))))
    (setf a (reverse a))
    (setf a (cond ((null a) nil)
                  ((not (= 0 (car a))) (append (list 0) a))
                  (t a)))
    (cond (a (maplist #'(lambda (x) 
                          (cond ((cdr x) (subseq list (car x) (cadr x)))
                                (t (subseq list (car x) (length list))))) a))
          (t (list list)))))

;(defstruct (node (:conc-name nil) (:print-function print-node)) data name next-nodes parent  (visited? nil) (visits 0))

(defclass node () 
  ((name :accessor name
         :initarg :name
         :initform nil)
   (data :accessor data
         :initarg :data
         :initform nil)
   (next-nodes :accessor next-nodes
               :initarg :next-nodes
               :initform nil)))

(cl:defun print-node (node stream print-level)
  (declare (ignore print-level))
  (princ (concatenate 'string 
                      "["
                      (write-to-string (name node))
                      (cond ((parent node)
                             (concatenate 'string
                                          "("
                                          (cond ((name (parent node)) (write-to-string (name (parent node))))
                                                (t "null"))
                                          ")"))
                            (t ""))
                                    
                      ":"
                      (write-to-string (length (next-nodes node)))
                      "]")
         stream))

(cl:defun walk-nodes (fn node)
  (funcall fn node)
  (mapcan (lambda (n) (walk-nodes fn n)) (next-nodes node)))

(cl:defun leaf-nodes (l)
  (remove-if-not #'null l :key #'next-nodes))
(cl:defun root-nodes (l)
  (remove-if #'null l :key #'next-nodes))

(cl:defun sequences->paths (sequences &key (test #'list-eq))
  (let* ((root (make-instance 't2l::node))
         (n root))
    (dolist (s sequences)
      (dolist (e s)
        (setf n (let ((i (position e (next-nodes n) :key #'name :test test)))
                  (cond (i (elt (next-nodes n) i))
                        (t (car (push (make-instance 't2l::node :name e :data e :parent n) (next-nodes n))))))))
      (setf n root))
    (let (leaves)
      (walk-nodes #'(lambda (x)
                      (cond ((next-nodes x) x)
                            (t (push x leaves))))
                  root)
      ;(print (list 'leaves leaves))
      (dolist (o leaves)
        (setf (next-nodes o) (next-nodes root))))
    root))

(cl:defun symbols->path-list (symbols &key (test #'list-eq))
  (let* ((root)
         (s (reverse 
             (mapcar #'(lambda (x) (list x (make-instance 't2l::node :name (write-to-string x) :data x)))
                    (remove-duplicates 
                     (let (x)
                       (funcall-rec #'(lambda (y)
                                        ;(lprint 'y y) 
                                        (push y x)) symbols)
                       x)
                     :test test
                     :from-end t)))))
    (setf root (cadr-assoc (caar symbols) s))
    (if (>= *mess* 30) (lprint 'symbols->paths 'root root))
    (dolist (k (mapcar #'car s))
      (let ((left (cadr-assoc k s)))
        (dolist (n (remove-duplicates (cdr-assoc k symbols) :test test :from-end t))
          (let ((right (cadr-assoc n s)))
            (push right (next-nodes left))))))
    s))

(cl:defun symbols->paths (symbols &key (test #'list-eq))
  (cadar (symbols->path-list symbols :test test)))

(cl:defun a-simple-path (u v)
  (if (visited? u) (fail))
  (local (setf (visited? u) t))
  (either (progn (unless (eq u v) (fail)) (list u))
          (cons u (a-simple-path (a-member-of (next-nodes u)) v))))

(cl:defun k-simple-path (u v k rules)
  (let ((path (k-simple-path-rec u v k)))
    (unless (evaluate path rules) (fail))
    path))

(cl:defun k-simple-path-rec (u v k)
  (if (>= (visits u) k) (fail))
  ;; This can't be (LOCAL (INCF (VISITS U))) since Lucid screws up here.
  (local (setf (visits u) (1+ (visits u))))
  (either (progn (unless (eq u v) (fail)) (list u))
          (cons u (k-simple-path-rec (a-member-of (next-nodes u)) v k))))

(cl:defun arithm-ser (begin end step &optional (nummax *infinity*))
  (if (plusp step)
    (loop for i from begin to end by step
          for counter from 1 to nummax
          collect i)
    (loop for i from begin downto end by (abs step)
          for counter from 1 to nummax
          collect i)))

(cl:defun om-one-simple-path (u v &key assoc (i 0) (k 1) rules)
  (let ((u-node (if assoc
                    (cadr-assoc u assoc)
                  u))
        (v-node (if assoc
                    (cadr-assoc v assoc)
                  v)))
    (ith-value i (k-simple-path u-node
                                v-node
                                k
                                rules))))

(cl:defun path-domainsv (list root)
  (let ((vars (path-domainsv-rec list (collect-next-nodes root))))
    (if (>= *mess* 20) (lprint 'path-domainsv 'vars vars))
    (apply #'andv vars)))

(cl:defun path-domainsv-rec (list roots)
  (if (>= *mess* 30) (lprint 'path-domainsv-rec 'list list 'roots roots))
  (cond ((null list) (list t))
        ((every #'null roots) nil) ; 
        (t (append (list (memberv (car list) (remove-duplicates (mapcar #'name roots))))
                   (path-domainsv-rec (cdr list)                                      
                                      ;(remove-duplicates (flat (mapcar #'next-nodes roots))))))))
                                      (collect-next-nodes roots))))))

(cl:defun collect-next-nodes (input) ; -> list-template->vars labels
  (cond ((null input) nil)
        ((listp input) (remove nil (remove-duplicates (flat (mapcar #'(lambda (x) (search-nodes #'data x)) input)))))
        (t (collect-next-nodes (next-nodes input)))))

(cl:defun search-nodes (predicate root) ; -> list-template->vars labels
  (let ((visited-nodes)
        (c))
    (labels 
        ((process (predicate node)
           (cond ((not (find node visited-nodes)) 
                  (push node visited-nodes)
                  (cond ((funcall predicate node) 
                         (if (not (find node c))
                             (push-to-end node c)))
                        (t 
                         (mapcar #'(lambda (x)
                                     (process predicate x))
                                 (next-nodes node)))))
                 (t nil))))
      (mapcar #'(lambda (x)
                  (process predicate x))
              (next-nodes root))
      c)))
(cl:defun paths->vars (list root &optional stack)
  (assert! (path-domainsv list root))
  (paths->vars-rec list (collect-next-nodes root))
  list)
(cl:defun paths->vars-rec (list roots)
  (cond ((null list) nil)
        ((cdr list)
         (let (a)
           (mapcar #'(lambda (o) (push o (system:cdr-assoc (name o) a))) roots)
           (if (>= *mess* 20) (print (list 'assert! 'or (car list) '--)))
           (assert!
            (apply 
             #'orv
             (mapcar #'(lambda (k) 
                         (if (>= *mess* 20) (progn 
                                              (print (list "   " 'and '= (car list) k))
                                              (print (list "   " "   " 'member (cadr list) (remove-duplicates (mapcar #'name (apply #'append (mapcar #'next-nodes  (system:cdr-assoc k a)))))))))
                         (andv (=v (car list) k)
                               (memberv (cadr list) 
                                        (remove-duplicates (mapcar #'name (collect-next-nodes (system:cdr-assoc k a)))))))                      
                     (mapcar #'car a)))))
         (paths->vars-rec (cdr list)
                          ; (remove-duplicates (flat (mapcar #'next-nodes roots)))))
                          (collect-next-nodes roots)))
                          
        (t         
         ; (assert! (memberv (car list) (remove-duplicates (mapcar #'name roots))))
         nil)))


(cl:defun print-dmg-cursor (obj stream depth)
  (labels ((cursor-rule->string (obj level)
             (cond ((null obj) "nil")
                   ((listp obj) (write-to-string (mapcar #'(lambda (x) 
                                                             (cursor-rule->string x level))
                                                         obj)))
                   ((screamer::variable? obj) (write-to-string obj))
                   ((booleanp obj) (write-to-string obj))
                   ((eq (type-of obj) 'dmg-cursor)
                    (format nil 
                            "[~A:~A~A]"
                            (label obj)
                            (sym obj)
                            (cond ((null depth) "")
                                  ((>= level depth) "")
                                  ((null (rules obj)) "")
                                  (t (cursor-rule->string (rules obj) (1+ level))))))
                   (t (write-to-string obj)))))
    (format stream
            "[~A:~A stack: ~A rules: ~A]"
            (write-to-string (label obj))
            (write-to-string (sym obj))
            (write-to-string (stack obj))
            (cursor-rule->string (rules obj) 0))))
          
(cl:defun demo-underscore (a)
  (print (write-to-string a))
  (string= (write-to-string a) "_"))


(cl:defun list-template->vars (list templates &key init superset min max (template->dx t) (seqc->dx t) mode)
  (list-template->vars-internal list 
                                (sequences->paths (if template->dx 
                                                      (mapcar 
                                                       #'(lambda (x)
                                                           (listdxv (append (list 0) x)))
                                                       templates)
                                                    templates))
                                :init init
                                :superset superset
                                :min min
                                :max max
                                :seqc->dx seqc->dx))

(cl:defun list-template->vars-internal (seqc root &key init superset min max (seqc->dx t))
  (let* ((seqcv (funcall-rec #'(lambda (x) 
                                 (cond ((null x) nil)
                                       ((screamer::variable? x) x)
                                       ((numberp x) x)
                                       (t (an-integerv))))
                             seqc))         
         (sflat (flat seqcv))
         (sflatnn (remove nil sflat)))
    (if init (assert! (=v (car (remove nil sflat)) init)))
    (if min (mapcar #'(lambda (x) (assert! (>=v x min))) sflatnn))
    (if max (mapcar #'(lambda (x) (assert! (<=v x max))) sflatnn))
    (if superset (mapcar #'(lambda (x) (assert! (memberv x superset))) sflatnn))
    (let* ((list sflat)
           (vlst (mapcar #'(lambda (x) (an-integerv)) 
                         (make-sequence 'list (1- (length list)))))
           (vars (paths->vars vlst root)))
      (if seqc->dx
          (progn 
            (maplist #'(lambda (x y)
                         (cond ((and (cdr x)
                                     (screamer::variable? (cadr x)))
                                (assert! (=v (cadr x) (+v (car x) (car y)))))
                               (t nil)))
                     (remove nil list)
                     vars)      
            seqcv)
        vars))))


(cl:defun list-max (l)
  (cond ((null l) (* -1 *infinity*))
        ((consp l) (max (list-max (car l))
                        (list-max (cdr l))))
        (t l)))
(cl:defun tree-maxv (l)
  (cond ((null l) (* -1 *infinity*))
        ((consp l) (maxv (list-maxv (car l))
                         (list-maxv (cdr l))))
        (t l)))
(cl:defun list-min (l)
  (cond ((null l) *infinity*)
        ((consp l) (min (list-min (car l))
                        (list-min (cdr l))))
        (t l)))
(cl:defun tree-minv (l)
  (cond ((null l) *infinity*)
        ((consp l) (minv (tree-minv (car l))
                         (tree-minv (cdr l))))
        (t l)))
(cl:defun list-minv (l)
  (apply #'minv l))
(cl:defun list-maxv (l)
  (apply #'maxv l))
(cl:defun clast (l)
  (cond ((atom l) l)
        (t (let ((last (last l)))
             (cond ((atom last) last)
                   ((cdr last)
                    (cond ((atom (cdr last))
                           (cdr last))
                          (t (clast (cdr last)))))
                   ((consp last)
                    (clast (car last)))
                   (t (car last)))))))

;(cl:defun n! (n)
;  (cond ((= n 1) 1)
;        (t (* n (n! (1- n))))))

(cl:defun n! (n)
  (labels ((n!-rec (n acc)
             (if (< n 2)
                 acc
               (n!-rec (1- n ) (* acc n)))))
    (n!-rec n 1)))

(cl:defun list-replace (s1 s2 s3 &key (test 'list-eq) (depth-limit -1))
  (list-replace-rec s1 s2 s3 :test test :depth-limit depth-limit))

(cl:defun list-replace-rec (s1 s2 s3 &key (test 'list-eq) (depth-limit -1) (level 0))
  (cond ((null s1) nil)
        ((funcall test s1 s2) s3)
        ((listp s1) 
         (cond ((or (= -1 depth-limit)
                    (< level depth-limit))
                (append (list 
                         (list-replace-rec (car s1) s2 s3 :test test :depth-limit depth-limit :level (+ 1 level)))
                        (list-replace-rec (cdr s1) s2 s3 :test test :depth-limit depth-limit :level level)))
               (t s1)))
        (t s1)))

; p 64 
(cl:defun match (p s)
  (cond ((null p) (null s))
        ((atom (car p))
         (and s
              (equal (car p) (car s))
              (match (cdr p) (cdr s))))
        ((and s
              (eq (caar p) '?))
         (cond ((match (cdr p) (cdr s))
                (set (cadar p) (car s))
                t)
               (t nil)))
        ((eq (caar p) '*)
         (cond ((and s (match (cdr p) (cdr s)))
                (set (cadar p) (list (car s))))
               ((match (cdr p) s)
                (set (cadar p) nil) t)
               ((and s (match p (cdr s)))
                (set (cadar p) (cons (car s) (eval (cadar p)))) t)
               (t nil)))
        ((and s
              (apply (caar p) (list (car s)))
              (match (cdr p) (cdr s)))
         (set (cadar p) (car s)) t)
        (t nil)))

(cl:defun prime-facts (x) ; copied from om::prime-facts 
  (let ((ip 1) (r) (n 0))
    (loop while (and (> x 1) 
                     (<= (* (aref *prime-numbers* ip) 
                            (aref *prime-numbers* ip))
                         x)) do
      (when (= 0 (mod x (aref *prime-numbers* ip)))
        (setq n 1)
        (loop while (= 0 
                       (progn (setq x (/ x (aref *prime-numbers* ip))) 
                              (mod x (aref *prime-numbers* ip)))) do
              (incf n))
        (push  (list (aref *prime-numbers* ip) n) r))
      (incf ip))
    (when (/= x 1)   (push  (list x 1) r))
    (or (reverse r) (list (list 1 1)))))

(cl:defun gcm (&rest n)
  (cond ((listp (car n))
         (apply #'gcm (car n)))
        (t (apply #'*
                  (mapcar (lambda (x) 
                            (apply #'expt x))
                          (remove-duplicates
                           (sort (apply #'append (mapcar #'prime-facts n))
                                 (lambda (x y)
                                   (cond ((= (car x) (car y))
                                          (> (cadr x) (cadr y)))
                                         (t (< (car x) (car y))))))
                           :test #'= 
                           :key #'car
                           :from-end t))))))

(cl:defun gcd1 (&rest n)
  (cond ((listp (car n))
         (apply #'gcd1 (car n)))
        (t (let* ((factors (mapcar #'prime-facts n))
                  (shared-terms (reduce #'intersection 
                                        (mapcar (lambda (x)
                                                  (mapcar #'car x))
                                                factors)))
                  (shared-factors (mapcan (lambda (x)
                                            (if (position (car x) shared-terms)
                                                (list x)))
                                          (apply #'append factors))))  
             (apply #'*
                    (mapcar (lambda (x) 
                              (apply #'expt x))
                            (remove-duplicates
                             (sort shared-factors
                                   (lambda (x y)
                                     (cond ((= (car x) (car y))
                                            (< (cadr x) (cadr y)))
                                           (t (< (car x) (car y))))))
                             :test #'= 
                             :key #'car
                             :from-end t)))))))
(cl:defun lcd (&rest n)
  (gcd1 n))

(cl:defun flat1 (list)
  (cond ((null list) nil)
        ((and (listp list)
              (every #'listp list))
         (apply #'append list))
        ((listp list)
         (flat1 (mapcar #'(lambda (x) (cond ((listp x) x)
                                            (t (list x))))
                        list)))
        (t list)))

(cl:defun flat (list &optional levels)
  (cond ((and levels (> levels 0)) (flat (flat1 list) (1- levels)))
        ((and levels (= levels 0)) list)
        (t (flat-internal list))))

(cl:defun flat-internal (list)
  (let (c)
    (funcall-rec (lambda (x) (push x c)) list)
    (reverse c)))

; Function defn
(cl:defun om-class-of (obj)
  (class-of obj))

(cl:defun replace-all (string part replacement &key (test #'char=))
"Returns a new string in which all the occurences of the part 
is replaced with replacement."
    (with-output-to-string (out)
      (loop with part-length = (length part)
            for old-pos = 0 then (+ pos part-length)
            for pos = (search part string
                              :start2 old-pos
                              :test test)
            do (write-string string out
                             :start old-pos
                             :end (or pos (length string)))
            when pos do (write-string replacement out)
            while pos)))

(cl:defun fill-list (template size)
  (setq l (make-sequence 'list size :initial-element nil))
  (setq elem nil)
  (loop for i from 0 
        while (< i (length l))
        collect (cond ((>= i (length template)) elem)                      
                      ((not (null (elt template i)))
                       (progn
                         (setf elem (elt template i))
                         elem))
                      ((null (elt template i)) elem)                       
                      (t (elt template i)))))

(cl:defun find-nearest-n (l1 n)
  (cond ((numberp (car n))
         (append (list (find-nearest-n l1 (car n))) 
                 (find-nearest-n l1 (cdr n))))
        (t nil)))

(cl:defun find-nearest-n (l1 n)
  (labels ((tstcar (ee1 ll1 cmin1 cdif1)
             (if (car ll1)
                 (let ((tst1 (abs (- (car ll1) ee1))))
                   (if (< tst1 cdif1)
                       (tstcar ee1 (cdr ll1) (car ll1) tst1)
                     (tstcar ee1 (cdr ll1) cmin1 cdif1)))
               (list cmin1))))
    (car (tstcar n l1 (car l1) (abs (- n (car l1)))))))
  
(cl:defun find-sumof (l1 )
  (tree-sum l1))

(cl:defun find-countof (l1 )
  (tree-count l1))

(cl:defun tree-sum (l1)
  (cond ((null l1) 0)
	((null (car l1)) 0)
	((numberp (car l1)) (+ (car l1) (tree-sum (cdr l1))))
        ((consp (car l1)) (+ (tree-sum (car l1)) (tree-sum (cdr l1))))
	(t 0)))

(cl:defun tree-count (l1)
  (cond ((null l1) 0)
        ((and (consp l1) (null (car l1)))
         (+ 1 (tree-count (cdr l1))))
        ((consp l1) (+ (tree-count (car l1))
                       (tree-count (cdr l1))))
        (t 1)))

(cl:defun remove-consecutive-duplicates (list &key (test #'eql))
  (let (c)
    (loop for i from 0 while (< i (length list))
          do (unless (and c (funcall test (elt list i) (car c)))
               (push (elt list i) c)))
    (reverse c)))

(cl:defun nsucc (input n &key step list-padding pad-character)
  (cond
   ((null step) (nsucc input n :step (1- n) :list-padding list-padding :pad-character pad-character))
   (t
    (let* ((list (if list-padding 
                     (append input
                             (make-sequence 'list (* -1 (- (length input) 
                                                           (* n (ceiling (/ (length input) n))))) :initial-element pad-character))
                   input))
           (length (length list)))
      (loop for i from 0
            for j = (* i step)
            for k = (+ j n)
            while (< j (length list))
            collect (subseq list j (if (<= k length) k length)))))))


(cl:defun copy-all (tree)
 (if (atom tree) tree (cons (copy-all (car tree)) (copy-all (cdr tree)))))

(cl:defun list-countof (item l1 &key test)
  (let ((test (if test test 'list-eq)))
    (cond ((and (null item)
                (null l1)) 1)
          ((null l1) 0)
          ((funcall test item (car l1)) 
           (+ 1 (list-countof item (cdr l1) :test test)))
          (t (list-countof item (cdr l1) :test test)))))

(cl:defun tree-countof (item l1 &key test)
  (let ((test (if test test 'list-eq)))
    (cond ((and (null item)
                (null l1)) 1)
          ((null l1) 0)
          ((listp l1) 
           (+ (tree-countof item (car l1) :test test) 
              (tree-countof item (cdr l1) :test test)))
          ((funcall test item l1) 1)
          (t 0))))

(cl:defun sort-list-by-count (ll &optional desc)
  (let* ((akeys (remove-duplicates ll :test 'list-eq)) ; assoc list keys or ll as set
         (alist (mapcar 
                 (lambda (k)
                   (append (list k) 
                           (list (list-countof k ll)))) 
                 akeys))
         (comparator (if desc '> '<)))
    (sort akeys :test (lambda (x y) 
                              (funcall comparator 
                                       (cadr-assoc x alist)
                                       (cadr-assoc y alist))))))

(cl:defun listdxxv (list)
  (let ((vars (mapcar #'(lambda (x) (an-integerv)) (make-sequence 'list (1+ (length list))))))
    (maplist #'(lambda (x y)
                 (if (cdr x)
                     (assert! (=v (cadr x) (+v (car x) (car y))))))
             vars
             (append list (list nil)))
    vars))

(cl:defun listdxv (list)
  (remove nil (maplist #'(lambda (x) (if (cdr x) (-v (cadr x) (car x)))) list)))

(cl:defun listdx (ll list &optional bin-mode)
  (let ((ml (maplist #'(lambda (x) 
                      (cond ((null bin-mode) (cond ((null (car x)) nil)
                                                   ((null (cadr x)) nil)
                                                   (t (- (cadr x) (car x)))))
                            (t (cond ((null (car x)) nil)
                                     ((null (cadr x)) nil)
                                     ((> (car x) (cadr x)) -1)
                                     ((< (car x) (cadr x)) 1)
                                     (t 0))))) ll)))
    (cond ((> (length ml) 1) (subseq ml 0 (- (length ml) 1)))
          ((= (length ml) 1) (list 0))
          (t nil))))

;(cl:defun listdxv (ll &optional bin-mode)
;  (setq ml (maplist (lambda (x) 
;                      (cond ((null bin-mode) 
;                             (cond ((null (car x)) nil)
;                                   ((null (cadr x)) nil)
;                                   (t (-v (car x) (cadr x)))))
;                            (t (cond ((null (car x)) nil)
;                                     ((null (cadr x)) nil)
;                                     ((>v (car x) (cadr x)) (make-intv= -1))
;                                     ((<v (car x) (cadr x)) (make-intv= 1))
;                                     (t (make-intv= 0)))))) ll))
;  (cond ((and (listp ml)
;              (>v (lengthv ml) 1) (subseq ml 0 (- (length ml) 1))))
;        (t (list (make-intv= 0)))))

(cl:defun filter-by-min-absdiff (t1 n1)
  (labels ((lma-filt (u n2) 
             (if (cdr u) 
                 (if (>= (abs (- (car u) (car (cdr u)))) n2)
                     (append (list (car u)) (lma-filt (cdr u) n2))
                   (if (cdr (cdr u))
                       (lma-filt (append (list (car u)) (cdr (cdr u))) n2)
                     (u))) 
               (list (car u)))))
    (lma-filt t1 n1)))	
 
(cl:defun remove-all (items l &key from-end test test-not start end count key)
  (let ((test (if test test 'list-eq))
        (start (if start start 0)))
    (cond ((null items) l)
          ((null l) nil)
          (t (remove-all (cdr items) 
                         (remove (car items) 
                                 l 
                                 :from-end from-end
                                 :test test
                                 :test-not test-not
                                 :start start 
                                 :end end
                                 :count count 
                                 :key key)
                         :from-end from-end
                         :test test
                         :test-not test-not
                         :start start 
                         :end end
                         :count count 
                         :key key)))))


(cl:defun retain-all (items l &key from-end test test-not start end count key)
  (let ((test (if test test 'list-eq))
        (start (if start start 0)))
    (remove-all (remove-all items 
                            l
                            :from-end from-end
                            :test test
                            :test-not test-not
                            :start start 
                            :end end
                            :count count 
                            :key key)
                l
                :from-end from-end
                :test test
                :test-not test-not
                :start start 
                :end end
                :count count 
                :key key)))
	
(cl:defun ftor (n)
  (sort (remove-duplicates (flat (prime-facts n)) :test 'eq)))

(cl:defun partn-list-elem (e keys)	
  (cond ((null e) nil)
        (t (screamer:one-value 
            (an-expanded-list keys #'(lambda (x) (= (find-sumof x) e)))))))

(cl:defun partn-list (keys input) ; TODO rename
  (if (null input)
      nil
    (append
     (list
      (partn-list-elem (car input) keys))
     (partn-list keys (cdr input)))))

(cl:defun random-list-e (ll )
  (elt ll (random (length ll))))

;(cl:defun flat (l) 
;  (cond ((null l) nil) 
;        ((atom (car l)) (cons (car l) (flat (cdr l)))) 
;        (t (append (flat (car l)) (flat (cdr l))))))

(cl:defun scale-to-int (ll)
  (cond ((numberp ll)
         (cond ((< ll 0) -1)
               ((= ll 0) 0)
               (t 1)))
        (t
         (cond ((= (length ll) 0) (list (scale-to-int 0)))
               ((= (length ll) 1) (list (scale-to-int (car ll))))
               (t 
                (let* ((gcm (gcm (mapcar (lambda (x)
                                             (if (rationalp x)
                                                 (denominator x)
                                               1))
                                           (mapcar #'rationalize ll))))
                       (r (om* ll gcm))
                       (gcd1 (gcd1 r)))
                  (values (mapcar
                           #'(lambda (x y)
                               (cond ((floatp x) (float y))
                                     (t y)))
                           ll
                           (mapcar #'floor (om/ r gcd1)))
                          gcm
                          gcd1)))))))

	 

(cl:defun list2int (ll)
  (cond ((null ll) nil)	
	((listp ll) (cond ((cdr ll) (append (list2int (car ll)) (list2int (cdr ll))))
			  (t (list2int (car ll)))))
	(t (let ((flll (car (multiple-value-list (floor ll)))))
	     (cond ((listp flll) (list (car flll)))
		   (t (list flll)))))))

(cl:defun to-fractn (n)
  (reduce-fractn n 1))		

(cl:defun reduce-fractn (n d)
  (if (not (= (mod n 1) 0))
      (reduce-fractn (* n 10) (* d 10))
    (let ((fs (remove 1 (intersection (ftor n) (ftor d)))))
      (if fs
          (let ((gcf (list-max fs)))
            (reduce-fractn (/ n gcf) (/ d gcf)))
        (list n d)))))
		    
(cl:defun to-om-ms-den-list (ms)
  (to-ms-den-list ms))

(cl:defun to-ms-den-list (ms)
  (if (car ms) (append (list (car (cdr (car ms)))) (to-ms-den-list (cdr ms))) nil))

(cl:defun test-merge-ms-partns (partns groups)
  (merge-ms-partns partns groups))

(cl:defun merge-ms-partns (partns groups)
  (setq grouprz (if (> (tlength partns) (tlength groups))
		    (append groups (make-sequence 'list
                                                  (- (tlength partns) (tlength groups)) 
                                                  :initial-element (list -1)))
		  groups))
  (if (car partns)
      (append (list (merge-partn-group (car partns) (subseq grouprz 0 (length (car partns)))))
	      (merge-ms-partns (cdr partns) (nthcdr (length (car partns)) grouprz)))
    nil))

(cl:defun merge-partn-group (partn group) 
  (if (car partn)
      (append (list (list (car partn) (car group))) (merge-partn-group (cdr partn) (cdr group)))
    nil))

(cl:defun tlength (ll)
  (treelen ll))

(cl:defun treelen (ll)
  (cond ((null ll) 0)
	((null (car ll)) (+ 0 (treelen (cdr ll))))
	((listp (car ll)) (+ (treelen (car ll)) (treelen (cdr ll))))
	(t (+ 1 (treelen (cdr ll))))))

(cl:defun to-ms-numr-list (ms den)
		(to-ms-numr-list (print ms) (car den)))		

(cl:defun to-ms-numr-list (ms den)
  (if (car ms)
      (append (list (to-ms-numr (car ms) den)) (to-ms-numr-list (cdr ms) den))
    nil))

(cl:defun to-ms-numr (ms den)
  (to-ms-numr ms (car den)))

(cl:defun to-ms-numr (ms den)
  (* (car ms) (/ den (car (cdr ms)))))

(cl:defun list-fnappl (fn ll)
  (cond ((cdr ll) (funcall fn (car ll) (list-fnappl fn (cdr ll))))
	(t (car ll))))

(cl:defun contains-list (ll)
  (cond ((null ll) nil)
        ((listp ll) (or (listp (car ll))
                        (contains-list (cdr ll))))
        (t nil)))

(cl:defun contains-atom (ll)
  (cond ((null ll) nil)
        ((listp ll) (or (atom (car ll))
                        (contains-atom (cdr ll))))
        (t nil)))

(cl:defun om-list-eq (l1 l2)
  (list-eq l1 l2))

(cl:defun atom-eq (a1 a2)
  (cond ((and (numberp a1)
              (numberp a2))
         (= a1 a2))
        ((and (stringp a1)
              (stringp a2))
         (string= a1 a2))
        ((and (atom a1)
              (atom a2))
         (eq a1 a2))
        (t nil)))

; (cl:defun list-eq (l1 l2 &key (test #'atom-eq))
;    (cond ((and (null l1) (null l2)) t)
;          ((or (null l1) (null l2)) nil)
;          ((and (listp l1) (listp l2)) 
;           (and (list-eq (car l1) (car l2)) (list-eq (cdr l1) (cdr l2))))
;          ((or (listp l1) (listp l2)) nil)
;          (t (funcall test l1 l2))))

(cl:defun list-eq (l1 l2 &key test) (equalp l1 l2))

;(define-box list-structure-eq ((l1 list) (l2 list))
;  :initvals '((0 2) (0 2))
;  :indoc '("list1" "list2")
;  :icon 410
;  :doc ""
;  (cond ((and (null l1) (null l2)) t)
;	((or (null l1) (null l2)) nil)
;	((and (listp (car l1)) (listp (car l2))) 
;	 (and (list-structure-eq (car l1) (car l2)) (list-structure-eq (cdr l1) (cdr l2))))
;	((and (atom (car l1)) (atom (car l2))) 
;	 (and (and (atom (car l1)) (atom (car l2))) (list-structure-eq (cdr l1) (cdr l2))))
;	(t nil)))

(cl:defun list-structure-eq (&rest xs)
  (cond ((every #'null xs) t)
        ((some #'null xs) nil)
        ((every #'listp (mapcar #'car xs))
         (and (apply #'list-structure-eq (mapcar #'car xs))
              (apply #'list-structure-eq (mapcar #'cdr xs))))
        ((every #'atom (mapcar #'car xs))
         (apply #'list-structure-eq (mapcar #'cdr xs)))
	(t nil)))

(cl:defun pad-lists (&rest lists)
  ;(assert (every #'(lambda (x) (not (null x))) lists))
  (let ((lmax (list-max (mapcar #'length lists))))
    (mapcar #'(lambda (x) (append x (make-sequence 'list (- lmax (length x)) :initial-element (last-atom x)))) lists)))

(cl:defun x-operator (fn arg1 arg2)
  (cond ((and (numberp arg1) (numberp arg2)) (funcall fn arg1 arg2))
        ((and (numberp arg1) (listp arg2)) (mapcar #'(lambda (input) (x-operator fn arg1 input)) arg2))
        ((and (listp arg1) (numberp arg2)) (mapcar #'(lambda (input) (x-operator fn input arg2)) arg1))
        ((and (listp arg1) (listp arg2)) (mapcar #'(lambda (input1 input2) (x-operator fn input1 input2)) arg1 arg2))
        (t nil)))

(define-box om+ (arg1 arg2)
  :icon 193
  (x-operator #'+ arg1 arg2))
(define-box om- (arg1 arg2)
  :icon 194
  (x-operator #'- arg1 arg2))
(define-box om* (arg1 arg2)
  :icon 195
  (x-operator #'* arg1 arg2))
(define-box om/ (arg1 arg2)
  :icon 196
  (x-operator #'/ arg1 arg2))
(define-box om= (arg1 arg2)
  :icon 259
  (= arg1 arg2))
(define-box om/= (arg1 arg2)
  :icon 260
  (/= arg1 arg2))
(define-box om> (arg1 arg2)
  :icon 256
  (> arg1 arg2))
(define-box om< (arg1 arg2)
  :icon 255
  (< arg1 arg2))
(define-box om>= (arg1 arg2)
  :icon 258
  (>= arg1 arg2))
(define-box om<= (arg1 arg2)
  :icon 257
  (<= arg1 arg2))

(define-box list-om+ (list)
  :icon 193
  (reduce-chunks #'+ list :default 0))
(define-box list-om- (list)
  :icon 194
  (apply #'- list))
(define-box list-om* (list)
  :icon 195
  (reduce-chunks #'* list :default 0))
(define-box list-om/ (list)
  :icon 196
  (apply #'/ list))
(define-box list-om= (list)
  :icon 259
  (assert (listp list))
  (assert (> (length list) 0))
  (assert (every #'numberp list))
  (reduce-chunks #'= list))
(define-box list-om/= (list)
  :icon 260
  (assert (listp list))
  (assert (> (length list) 0))
  (assert (every #'numberp list))
  (reduce-chunks #'/= list))
(define-box list-om> (list)
  :icon 256
  (apply #'> list))
(define-box list-om< (list)
  :icon 255
  (apply #'< list))
(define-box list-om>= (list)
  :icon 258
  (apply #'>= list))
(define-box list-om<= (list)
  :icon 257
  (apply #'<= list))
(define-box list-omand (list)
  :icon 218
  (every #'identity list))
(define-box list-omor (list)
  :icon 219
  (some #'identity list))

(define-box stretch ((list '(a s d f)) (length 7) &key from-head)
  :icon 235
  (assert (listp list))
  (assert (numberp length))
  (assert (>= length 0))
  (cond
   ((= length (length list)) list)
   ((< length (length list)) 
    (cond
     (from-head
      (subseq list (- (length list) length) (length list)))
     (t
      (subseq list 0 length))))
   (t 
    (cond
     (from-head
      (append (make-sequence 'list (- length (length list)) :initial-element (car list))
              list))
     (t
      (append list
              (make-sequence 'list (- length (length list)) :initial-element (car (reverse list)))))))))

(cl:defun funcalltr (fn tr)
  (cond ((null tr) nil)
        ((listp tr) (append (list (funcalltr fn (car tr)))
                            (funcalltr fn (cdr tr))))
        (t (funcall fn tr))))

(cl:defun funcalln-rec (fn tr)
  (cond ((null tr) nil)
        ((listp tr) (append (list (funcalln-rec fn (car tr)))
                            (funcalln-rec fn (cdr tr))))
        (t (funcall-nondeterministic fn tr))))
  

(cl:defun index-of (e ll)
  (remove-if-not (lambda (x) (not (null x))) 
		   (loop for n from 0 to (- (length ll) 1) 
		      collect (cond ((eq e (car (nthcdr n ll))) n)
				    (t nil)))))

(cl:defun mat-trans (ll)
  (apply 'mapcar 'list ll))

(cl:defun mat-trans2 (ll &optional pad)
  (let ((length-max (list-max (mapcar #'length ll))))   
    (apply 'mapcar 'list (mapcar (lambda (x)
                                   (append x
                                           (make-sequence 'list
                                                          (- length-max (length x))
                                                          :initial-element pad)))
                                 ll))))

(cl:defun mat-trans-rec (fn m &optional pad)
  (mat-trans2 
   (funcall fn (mapcar (lambda (x)
                         (cond ((contains-atom x) x)
                               (t 
                                (mat-trans-rec fn x pad))))
                       (mat-trans2 m pad)))
   pad))

(cl:defun atoms2list (x) 
  (cond ((null (car x)) nil)
	((listp (car x)) (append (list (car x)) (atoms2list (cdr x))))
	((atom (car x)) (append (list (list (car x))) (atoms2list (cdr x))))
	(t nil)))
(cl:defun atoms2list-rec (x)
  (cond ((null (car x)) nil)
	((and (listp (car x))
	      (> (list-length (car x)) 1)) (append (list (atoms2list-rec (car x))) (atoms2list-rec (cdr x))))
	(t (append (list (list (car x))) (atoms2list-rec (cdr x))))))

(cl:defun find-largest-sublist-len (x) 
  (cond ((null (car x)) 0)
	((listp (car x)) (max (length (car x)) 
			      (find-largest-sublist-len (cdr x))))
	((atom (car x)) (max 1 
			     (find-largest-sublist-len (cdr x))))
	(t 0)))

(cl:defun cadr-assoc (item alist &key default)
  (let ((assoc-entry (assoc item (mapcar (lambda (x) (cond ((atom x) 
                                                            (append (list x) (list t)))
                                                           ((and (listp x)
                                                                 (= 1 (list-length x)))
                                                            (append x (list t)))
							   (t x))) alist))))
    (cond ((not (null assoc-entry)) (cadr assoc-entry))
	  ((not (null default)) default)
	  (t nil))))

(cl:defun assoc-keys (alist)
  (remove nil (mapcar (lambda (x) (cond ((null x) nil) 
                          ((listp x) (car x))
                          (t x))) alist)))

(cl:defun om-rplac (caritem consitem alist)
  (let ((init-alist (atoms2list alist)))
    (cond ((position caritem (assoc-keys init-alist))
           (rplacd consitem (assoc caritem init-alist)))
          (t (append init-alist
                     (list (append (list caritem) consitem)))))))



(cl:defun sort-ratio-list (ll)
  (sort ll #'(lambda (x y) (< (/ (car x) (cadr x))
                              (/ (car y) (cadr y))))))

(cl:defun cartx (&rest lists)
  (apply #'cartesian-product lists))

(cl:defun cp-atom-list (a l)
 (cond ((null l) nil)
       (t (cons (cons a (car l))
                (cp-atom-list a (cdr l))))))
 
(cl:defun cp-list-list (m n)
 (if (and m n)
     (append (cp-atom-list (car m) n)
             (cp-list-list (cdr m) n))
   nil))
 
(cl:defun cartesian-product (&rest list-of-sets)
 (reduce #'cp-list-list list-of-sets
 :from-end t
 :initial-value '(())))

(cl:defun all-subsets (list r)
  (nPr list r))

(cl:defun nPr (n r)
  (cond ((numberp n) (/ (n! n) (* (n! n) (n! (- n r)))))
        (t (all-values 
             (let ((set (a-subset-of n)))
               (unless (= (length set) r) (fail))
               set)))))
  

(cl:defun cartx-pair (l1 l2) (reduce 'append (mapcar #'(lambda(x) 
						      (mapcar #'(lambda(y)  
								  (list x y)) 
							      l2))  
						  l1)))

(cl:defun list-min-idx (ll &key test)
  (let ((init-test (if test test '<)))
    (position (car (sort ll :test init-test)) ll)))

(cl:defun contains1 (obj place &key test)
  (loop for n in place when (funcall (or test #'list-eq) obj n) return t))

(cl:defun push1 (obj place &key test)
  (let ((init-test (cond ((null test) 'list-eq)
                         (t test)))
        (init-place (cond ((listp place) place)
                          (t (list place)))))
    (cond ((not (contains1 obj init-place :test init-test))
           (push obj init-place))
          (t place))))

(cl:defun remove1 (obj place &key test)
  (let ((init-test (cond ((null test) 'list-eq)
                         (t test))))
    (remove obj place :test init-test)))

(cl:defun subseq-rec (ll p)
  (cond ((null ll) nil)
        ((null p) ll)
        ((> (car p) (list-length ll)) ll)
        (t (append (list (subseq ll 0 (car p)))
                   (subseq-rec (subseq ll (car p) (list-length ll))
                               (cdr p))))))

(cl:defun lfill (ll max) 
  (labels ((lfill-rec (ll-l ll-r max)
             (cond ((and (null ll-l) (null ll-r)) nil)
                   ((and ll-l (null ll-r)) ll-l)
                   ((null ll-l) (lfill-rec (list (car ll-r))
                                           (cdr ll-r)
                                           max))
                   ((> (tree-sum (append ll-l (list (car ll-r)))) max) ll-l)
                   (t (lfill-rec (append ll-l (list (car ll-r)))
                                 (cdr ll-r)
                                 max)))))
  (lfill-rec nil ll max)))

(cl:defun copy-assoc (l1 rplac-l1 )
  (copy-assoc-rec (atoms2list l1) rplac-l1))

(cl:defun copy-assoc-rec (l1 rplac-l1)
  (cond ((null l1) nil)
        ((and (not (listp (car l1)))
              (list-eq (car l1) (car rplac-l1)))
         (append (list rplac-l1) (copy-assoc-rec (cdr l1) rplac-l1)))
        ((not (listp (car l1)))
         (append (list (car l1)) (copy-assoc-rec (cdr l1) rplac-l1)))
        ((list-eq (caar l1) (car rplac-l1)) 
         (append (list rplac-l1) (copy-assoc-rec (cdr l1) rplac-l1)))
        (t 
         (append (list (car l1)) (copy-assoc-rec (cdr l1) rplac-l1)))))

(cl:defun list2string (ll)
  (cond ((null ll) nil)
        ((stringp ll) ll)
        ((numberp ll) (write-to-string ll))
        ((atom ll) (write-to-string ll))
        (t (concatenate 'string (list2string (car ll)) (list2string (cdr ll))))))

(cl:defun list< (l1 l2)
  (labels ((elem< (e1 e2) 
             (cond ((and (null e1)
                         (null e2)) nil)
                   ((null e1) t)
                   ((null e2) nil)
                   ((and (numberp e1)
                         (numberp e2)) (< e1 e2))
                   (t (string< (write-to-string e1)
                               (write-to-string e2))))))
    (loop for i from 0
          while (and (< i (length l1))
                     (< i (length l2)))
          when (or (not (list-eq (subseq l1 i (+ 1 i))
                                 (subseq l2 i (+ 1 i))))
                   (= i (- (length l1) 1))
                   (= i (- (length l2) 1)))
          return (or (elem< (subseq l1 i (+ 1 i))
                            (subseq l2 i (+ 1 i)))
                     (and (not (= (length l1) (length l2)))
                          (and (= i (- (length l1) 1))
                               (> (length l2) 
                                  (length l1))))))))

(cl:defun list= (l1 l2)
  (list-eq l1 l2))

(cl:defun list-comparev (fn list value)
  (apply #'andv (mapcar #'(lambda (x) (funcall fn x value)) (flat list))))



;; suffix arr support
(defstruct sfx-arr-elem index suffix lcp count)

(cl:defun sfx-arr-elem-list< (s0 s1)
  (list< (sfx-arr-elem-suffix s0) (sfx-arr-elem-suffix s1)))

(cl:defun sfx-arr-elem-listv< (s0 s1)
  (list< (sfx-arr-elem-suffix s0) (sfx-arr-elem-suffix s1)))

(cl:defun sfx-arr-elem-string< (s0 s1)
  (string< (sfx-arr-elem-suffix s0) (sfx-arr-elem-suffix s1)))

(cl:defun sfx-arr-elem-string= (s0 s1)
  (string= (sfx-arr-elem-suffix s0) (sfx-arr-elem-suffix s1)))

(cl:defun sfx-arr-elem-list-eq (s0 s1)
  (list-eq (sfx-arr-elem-suffix s0) (sfx-arr-elem-suffix s1)))

(cl:defun sfx-arr-elem-list-eqv (s0 s1)
  (equalv (sfx-arr-elem-suffix s0) (sfx-arr-elem-suffix s1)))

(cl:defun sfx-arr-elem-test= (s0 s1)
  (let ((mess (list s0 s1)))
    nil))

(cl:defun string2suffix-list (s &optional len)
  (let* ((arr (sort
               (flat (loop for i0 from 0
                               while (< i0 (length s))
                               collect (loop for i1 downfrom i0
                                             while (>= i1 0)
                                             collect (make-sfx-arr-elem :index i0 
                                                                        :suffix (write-to-string (subseq s i1 (+ 1 i0))))))
                         1)
               :test 'sfx-arr-elem-string<))
         (arr-occ (mapcar (lambda (x) (setf (sfx-arr-elem-count x) (count x arr :test (quote sfx-arr-elem-string=)))) arr)))
    (remove-duplicates arr :test 'sfx-arr-elem-string=)))

(cl:defun build-suffix-list (s &optional interval-mode)
  (let* ((si (if interval-mode 
                 (listdx s)
               s))
         (arr (sort
               (remove nil 
                       (flat (loop for i0 from 0
                                       while (< i0 (length si))
                                       collect (loop for i1 downfrom i0
                                                     while (>= i1 (max 0 (- i0 6)))
                                                     collect (make-sfx-arr-elem :index i0 
                                                                                :suffix (subseq si i1 (+ 1 i0)))))
                                 1))
               :test 'sfx-arr-elem-list<))
         (sset (remove-duplicates arr :test #'sfx-arr-elem-list-eq))
         (arr-occ (mapcar (lambda (x) (setf (sfx-arr-elem-count x) (count x arr :test #'sfx-arr-elem-list-eq))) sset)))
    sset))

(cl:defun build-suffix-listv (ss &optional interval-mode (prcs-max 8))
  (let* ((s (apply-substitution ss))
         (si (if interval-mode 
                 (listdxv s)
               s))
         (arr (remove nil
                       (flat (local 
                                   (let (r) 
                                     (dotimes (i0 (lengthv si))
                                       (dotimes (i1 (min i0 prcs-max))
                                         (let ((subseq (make-variable)))
                                           (prolog-subseq si (- i0 i1) (+ 1 i0) subseq)
                                           (push (make-sfx-arr-elem :index i0
                                                                    :suffix subseq) r))))
                                     r))
                                 1)))
         (sset (remove-duplicates arr :test #'sfx-arr-elem-list-eqv))
         (arr-occ (progn 
                    (dotimes (i (length sset)) 
                      (let ((sfx (elt sset i)))
                        (setf (sfx-arr-elem-count sfx) (count sfx arr :test #'sfx-arr-elem-list-eqv))))
                    sset)))
    arr-occ))

(cl:defun build-suffix-listv2 (s &optional interval-mode (prcs-max 8))
  (let* ((si (apply-substitution (if interval-mode 
                 (listdxv s)
               s)))
         (arr (remove nil
                       (flat (local 
                                   (let (r) 
                                     (dotimes (i0 (length si))
                                       (dotimes (i1 (min i0 prcs-max))
                                         (push (make-sfx-arr-elem :index i0
                                                                  :suffix (subseq si (- i0 i1) (+ 1 i0))) r)))
                                     r))
                                 1)))
         (sset (remove-duplicates arr :test #'sfx-arr-elem-list-eqv))
         (arr-occ (progn 
                    (dotimes (i (length sset)) 
                      (let ((sfx (elt sset i)))
                        (setf (sfx-arr-elem-count sfx) (count sfx arr :test #'sfx-arr-elem-list-eqv))))
                    sset)))
    arr-occ))

(cl:defun lwin (ll fn wlen &key index-mode)
  (cond ((> wlen (length ll)) nil)
        (t (remove-duplicates (loop for i from 0
                                 while (< i (- (length ll) wlen))
                                 collect (if index-mode
                                             (position (funcall fn (subseq ll i (+ i wlen 1))) ll :start i)
                                           (funcall fn (subseq ll i (+ i wlen 1)))))
                           :test 'eq))))

(cl:defun llevels (input &key key)
  (labels ((llevels-rec (l level key)              
             (cond ((null l) 0)
                   ((consp l) (max (llevels-rec (car l) (1+ level) key)
                                   (llevels-rec (cdr l) level key)))
                   (key (let ((d (funcall key l)))
                          (cond (d (llevels-rec d level key))
                                (t level))))
                   (t level))))
    (llevels-rec (funcall-rec #'(lambda (x) '_) input) 0 key)))

(cl:defun llevela (l)
  (labels ((llevela-rec (l level)
             (cond ((null l) *infinity*)
                   ((listp l) (min (llevela-rec (car l) (+ 1 level))
                                   (llevela-rec (cdr l) level)))
                   (t level))))
    (llevela-rec l 0)))
  

(cl:defun std (ll) 
  (let* ((m (/ (find-sumof ll) (length ll)))
         (adev2 (mapcar (lambda (x) (expt (- x m) 2)) ll))
         (v (/ (find-sumof adev2) (length ll))))
    (sqrt v)))

(cl:defun ivals-list (l1) 
  (labels ((ivals-list-rec (ll)
             (cond ((null ll) nil)
                   ((cadr ll) (append (list (- (cadr ll) (car ll)))
                                      (ivals-list-rec (cdr ll))))
                   (t nil))))
    (cond ((and l1
                (= 1 (length l1)))
           (list 0))
          (t (ivals-list-rec l1)))))

(cl:defun factorial (N)
  "Compute the factorial of N."
  (if (= N 1)
      1
    (* N (factorial (- N 1)))))

(cl:defun avg (n)
  (/ (apply #'+ n)
     (length n)))

(cl:defun variance (n &optional sample-stdv)
  (let ((avg (avg n)))
    (/ (find-sumof 
        (mapcar
         (lambda (x)
           (expt (- x avg) 2))
         n))
       (if (and sample-stdv 
                (> (length n) 0))
           (- (length n) 1)
         (length n)))))
(cl:defun variancev (n &optional sample-stdv)
  (let ((input (mapcar #'(lambda (x) (make-realv= x)) n)))
    (let ((avg (meanv input)))
      (/v (sumv (mapcar #'(lambda (x) (funcallv #'expt (-v x avg) 2)) input))
          (if (and sample-stdv
                   (> (length input) 0))
              (- (length input) 1)
            (length input))))))

(cl:defun stdv (n &optional sample-stdv)
  (sqrt (variance n sample-stdv)))
(cl:defun stdvv (n &optional sample-stdv)
  (funcallv #'sqrt (variancev n sample-stdv)))


(cl:defun histo (seqc &key (test #'=) (comparator #'<) (sort-by-count nil))
  (let* ((seqc-flat (flat seqc))
         (set (sort 
               (remove-duplicates seqc-flat
                                  :test test)
               :test test))
         (counts (mapcar 
                  (lambda (x)
                    (count x seqc-flat))
                  set)))
    (sort (mapcar #'cons
                           set
                           counts)
                   :key (if sort-by-count
                            #'cdr
                          #'car)
                   :test comparator)))

(cl:defun listbfn (l fn)
  (cond ((null l) t)
        ((listp l) 
         (and (listbfn (car l) fn)
              (listbfn (cdr l) fn)))
        (t (funcall fn l))))

(cl:defun list-stdv (seqc &key (test #'=) (comparator #'<) (stdv-sort nil) (do-sample-stdv nil))
  (let* ((counts (histo seqc :test test :comparator comparator :sort-by-count nil))
         (cavg (avg (mapcar #'cdr counts)))
         (cstdv (stdv (mapcar #'cdr counts) do-sample-stdv))
         (stdv-set (cond ((= 0 cstdv) (make-sequence 'list (length counts) :initial-element 0))
                         (t (mapcar 
                             (lambda (x)
                               (/ (- x cavg)
                                  cstdv))
                             (mapcar #'cdr counts))))))
    (sort (mapcar #'cons 
                           (mapcar #'car counts)
                           stdv-set)
                   :key (if stdv-sort
                            #'cdr
                          #'car)
                   :test #'<)))

(cl:defun histogram-stdv-seqc-processing (seqc fn-assoc &key print-warnings)
  (setq stdv-histo (list-stdv (flat seqc)))
  (mapcar (lambda (x) 
            (setf (system:cdr-assoc x stdv-histo)
                  (round (system:cdr-assoc x stdv-histo))))
          (mapcar #'car stdv-histo))
  (setq stdv-keys (remove-duplicates (mapcar #'cdr stdv-histo) :test #'=))
  (setq fn-keys (remove-duplicates (mapcar #'car fn-assoc) :test #'=))
  (setq h2fn-keys (mapcar (lambda (x)
                            (cons x (find-nearest-n fn-keys x)))
                          stdv-keys))
  (if print-warnings 
      (progn
        (print (list 'stdv-scores stdv-histo))
        (print (list 'stdv-keys stdv-keys))
        (print (list 'h2fn-keys h2fn-keys))
        nil))
  (funcall-rec (lambda (x)
                 (funcall
                  (system:cdr-assoc
                   (system:cdr-assoc
                    (system:cdr-assoc x
                                      stdv-histo) 
                    h2fn-keys)
                   fn-assoc)
                  x))
               seqc))

(defmethod fill-slots ((obj standard-object) alist &optional package)
  (let ((f nil))
    (dolist (k (mapcar #'car alist))
      (let ((ksym (intern (symbol-name k))))
        (cond
         ((slot-exists-p obj ksym)
          (setf (slot-value obj ksym) (cdr-assoc k alist))
          (push k f))
         (t nil))))
    f))

(cl:defun mergeassoc (alist1 alist2 &optional cdr-test)
  (assert (not (some #'atom alist1)))
  (assert (not (some #'atom alist2)))
  (let ((keys (remove-duplicates
               (append (mapcar #'car alist1)
                       (mapcar #'car alist2)))))
    (cond
     (cdr-test
      (let ((alist3 nil))
        (mapcar #'(lambda (k)
                    (setf (cdr-assoc k alist3)
                          (or (cdr-assoc k alist1)
                              (cdr-assoc k alist2))))
                keys)
        alist3))
     (t (mapcar #'(lambda (k)
                    (or (assoc k alist1)
                        (assoc k alist2)))
                keys)))))

(cl:defun list-structure (x) 
  (funcall-rec #'(lambda (y) (cond ((null y) nil)
                                   ((screamer::variable? y) 'v)
                                   ((integerp y) 'i)
                                   ((numberp y) 'r)
                                   (t '_)))
               x))

(defun set-difference-eq (x y) (and (not (or (null x) (null y))) (or (and (null x) (null y)) (null (set-difference x y)))))


(define-box read-textfile (filename)
  :indoc '("filename")
  :icon 908
  :doc ""
  (let ((in (open filename :if-does-not-exist nil)))
    (when in
      (let ((string (loop for line = (read-line in nil)
                          while line collect line)))
        (close in)
        string))))

  
(cl:defun space-split (string)
  (loop for start = 0 then (1+ finish)
        for finish = (position #\Space string :start start)
        collecting (subseq string start finish)
        until (null finish)))
 
(cl:defun write-with-periods (strings)
  (format t "~{~A~^.~}" strings))
;(let ((out (reduce
;                    #'concatenate
;                    (append (list 'string)
;                            (mapcar #'(lambda (s)
;                                        (format nil (concatenate 'string s "~%")))
;                                    (butlast string))
;                            (last string)))))
;          out)))))

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
(cl:defun kill-background-jobs ()
  (let ((pname (mp:process-name (mp:get-current-process))))
   (loop for p in (mp:list-all-processes)
         when (and (not (string= (mp:process-name p)
                                 pname))
                   (or 
                    (not
                     (null
                      (search "Background"
                              (mp:process-name p))))
                    (not
                     (null
                      (search "OM EVAL PROCESS" 
                              (mp:process-name p))))))
         do (mp:process-kill p))))

(cl:defun kbj ()
  (kill-background-jobs))