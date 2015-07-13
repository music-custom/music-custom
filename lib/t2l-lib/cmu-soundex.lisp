(in-package :t2l)

(define-box om-soundex (string &key abbrv-p)
  :initvals "twenty"
  :indoc "string"
  :icon 224
  (soundex string :abbrv-p abbrv-p))

(define-box om-convert-phrase-to-soundex (string)
  :initvals "twenty eight days later"
  :indoc "string"
  :icon 224
  (convert-phrase-to-soundex string))

(define-box om-convert-phrase-list-to-soundex (strings)
  :initvals '("twenty eight days later" "jurassic park")
  :indoc "ten things I hate about you -> ((\"ten\" \"things\" ... ) . (\"A333\" \"A333\"))"
  :icon 224
  (convert-phrase-list-to-soundex strings))

(defun rhymes? (entry1 entry2)
  :doc "word/soundex pairs"
  (let ((word1 (car entry1))
        (word2 (car entry2))
        (soundex1 (cadr entry1))
        (soundex2 (cadr entry2)))
    (and (not (string-equal word1 word2))
       (> (length soundex1) 3)
       (> (length soundex2) 3)
       (string-equal (subseq soundex1 (- (length soundex1) 1) (length soundex1))
                     (subseq soundex2 (- (length soundex2) 1) (length soundex2))))))

(defun phrase-rhymes? (entry1 entry2)
  (let ((xlat1 (mat-trans entry1))
        (xlat2 (mat-trans entry2)))
    (let ((word-entry1 (car (reverse xlat1)))
          (word-entry2 (car (reverse xlat2))))
      (rhymes? word-entry1 word-entry2))))

(defun rhymes-with (input-entry soundex-db) 
  (let ((match-entry (a-member-of soundex-db)))
    (assert! (phrase-rhymes? input-entry match-entry))
    match-entry))

(defun best-rhyme (input-entry soundex-db)
  (let ((match-score (an-integerv)))
    (best-value (solution (let ((match (rhymes-with input-entry soundex-db)))
                            (assert! (=v match-score (soundex-word-list-comparator (cadr input-entry) (cadr match))))
                            (print (format nil "~A: ~A" match match-score))
                            (list input-entry match))
                          (static-ordering #'linear-force))
                match-score)))

; A123 L43 -> 10 match points (10 x # of matches)  A123 L23 -> 20 + 15 consecutivity points
(cl:defun soundex-word-comparator (word1 word2)
  (let ((digits1 (reverse (subseq word1 1 (length word1))))
        (digits2 (reverse (subseq word2 1 (length word2)))))
    (let ((total 0))
      
      (loop for i from 0 to (1- (min (length digits1) (length digits2)))
            do (let ((c1 (subseq digits1 i (1+ i)))
                     (c2 (subseq digits2 i (1+ i)))
                     (l1 (if (> i 0)
                             (subseq digits1 (1- i) i)
                           nil))
                     (l2 (if (> i 0)
                             (subseq digits2 (1- i) i)
                           nil)))
                 ; (print (format nil "c1: ~A c2: ~A l1: ~A l2: ~A" c1 c2 l1 l2))
                 (if (string-equal c1 c2)
                     (setf total (+ total 1))
                   (return))
                 (if (and l1 l2 (string-equal c1 c2) (string-equal l1 l2))
                     (setf total (+ total 20)))
))
                 
     
      total)))

(cl:defun soundex-word-list-comparator (l1 l2)
  (apply #'+ (mapcar #'(lambda (x y) (soundex-word-comparator x y)) l1 l2)))

(cl:defun soundex-rhymes? (w1 w2) (> (soundex-word-comparator w1 w2) 0))

(cl:defun convert-phrase-to-soundex (string &key retain-initial-char)
  :doc "ten things I hate about you -> ((\"ten\" \"things\" ... ) . (\"A333\" \"A333\"))"
  (let ((words  (space-split string))) 
    (let ((sx (cons words (list (mapcar #'(lambda (x) (soundex x)) words)))))
      (let ((sx-assoc (mat-trans sx)))
        (let ((sx-assoc-filtered (remove-if #'(lambda (x) (string-equal (cadr x) *soundex-placeholder*))
                   sx-assoc)))
          (if (null sx-assoc-filtered)
              nil
            (mat-trans sx-assoc-filtered)))))))

(cl:defun convert-phrase-list-to-soundex (strings)
  (remove-if #'(lambda (x) (null x))
             (mapcar #'(lambda (x) (convert-phrase-to-soundex x)) strings )))


;; SOUNDEX
(defvar *soundex-alphabet* 
  '(
"A" "B" "C" "D" "E" "F"
"G" "H" "I" "J" "K" "L" 
"M" "N" "O" "P" "Q" "R" 
"S" "T" "U" "V" "W" "X" 
"Y" "Z" ))

(defvar *soundex-placeholder* "A000")

(cl:defun soundex (word &key abbrv-p)
  (cond
   ((null word) *soundex-placeholder*)
   ((soundex-string-has-numbers? word) *soundex-placeholder*)
   ((string-equal word "") *soundex-placeholder*)

   (t
    (let ((soundex (soundex-internal word :abbrv-p abbrv-p)))
      (cond ((null soundex)  *soundex-placeholder*)
            ((not (stringp soundex)) *soundex-placeholder*)
            ((string-equal soundex "") *soundex-placeholder*)
            ((> (length soundex) 4) (subseq soundex 0 4))
            (t ; (add-zeros-to-soundex soundex)
             soundex))))))
; (cl:defun add-zeros-to-soundex (soundex)
;   (loop for i from (max (1- (length soundex)) 1) while (< (length soundex) 4)
;        do (setf soundex (concatenate 'string soundex "0")))
;  soundex)
(cl:defun count-occurances (item string)
  (let ((count 0))
    (loop for i from 0 while (< i (length string))
          do (if (string-equal item (subseq string i (min (length string) (+ i (length item)))))
                 (setf count (1+ count))))
    count))
(cl:defun soundex-string-has-numbers? (word)
  (block fn
  (loop for i from 0 while (< i (length word))
        do (unless (find (string-upcase (subseq word i (1+ i))) *soundex-alphabet* :test #'string-equal)
             (return-from fn t)))
  nil))

;; Public domain CL implementation of the soundex algorithm


;Soundex (developed by the Remington Rand Corp.?)
; from
;     Huffman, Edna K. (1972) Medical Record Management.
;        Berwyn, Illonois: Physicians' Record Company.
;The algorithm is very simple:
;
;1:  Assign number values to all but the first letter of the
;    word, using this table
;   1 - B P F V
;   2 - C S K G J Q X Z
;   3 - D T
;   4 - L
;   5 - M N
;   6 - R
;   7 - A E I O U W H Y
;
;2: Apply the following rules to produce a code of one letter and
;   three numbers.
;   A: The first letter of the word becomes the initial character
;      in the code.
;   B: When two or more letters from the same group occur together
;      only the first is coded.
;   C: If two letters from the same group are seperated by an H or
;      a W, code only the first.
;   D: Group 7 letters are never coded (this does not include the
;      first letter in the word, which is always coded).
;
;Of course, this can be used without the numeric substitution to
;produce abbreviations also, but the numbers indicate the phonemic
;similarity (e.g. Bear = Bare = B6), or Rhymes (e.g. Glare = G46,
;Flair = F46).  This can also be useful for finding duplicate entries
;in a large database, where a name may be slightly mis-spelled (e.g.
;Smith = Simth = S53).
;
;Norman R. Stewart Jr.
;C.S. Grad - SUNYAB
;internet: stewart@cs.buffalo.edu
;bitnet:   stewart@sunybcs.bitnet

(defparameter soundex-character-values
  #(#\7 #\1 #\2 #\3 #\7 #\1 #\2 #\7 #\7 #\2 #\2 #\4 #\5
    #\5 #\7 #\1 #\2 #\6 #\2 #\3 #\7 #\1 #\7 #\2 #\7 #\2)
  "Vector of character values accessible by
   (aref ... (- (char-int (char-upcase x)) (char-int #\A)))
   Cheat Note: this is true of ASCII, but *not* EBCDIC!")

(cl:defun soundex-internal (word &key abbrv-p)
  (let* ((first-character (char-int #\A))
	 (last-letter (char-upcase (char word 0)))
	 (last-class  (aref soundex-character-values
			    (- (char-int last-letter) first-character)))
	 (l (length word))
	 (out (list last-letter))
	 this-letter
	 this-class)
    (do* ((i 1 (1+ i)))
	 ((>= i l))
      (setf this-letter (char-upcase (char word i)))
      (setf this-class (aref soundex-character-values
			     (- (char-int this-letter) first-character)))
      (when (char/= this-class last-class)
	(if (char/= this-class #\7)
	    (if abbrv-p
		(push this-letter out)
		(push this-class out))
	    ;; could be H or W separating like letters:
	    (when (and (or (char= this-letter #\H)
			   (char= this-letter #\W))
		       (< (1+ i) l)
		       (char= (aref soundex-character-values
				    (- (char-int (char-upcase (char word (1+ i))))
				       first-character))
			      last-class))
	      (incf i)
	      (setf this-letter (char-upcase (char word i)))
	      (setf this-class (aref soundex-character-values
				     (- (char-int this-letter) first-character))))))
      (setf last-letter this-letter
	    last-class  this-class))
    (coerce (nreverse out) 'string)))

