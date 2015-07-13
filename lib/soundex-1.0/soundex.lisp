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

(in-package :soundex)

(defparameter soundex-character-values
  #(#\7 #\1 #\2 #\3 #\7 #\1 #\2 #\7 #\7 #\2 #\2 #\4 #\5
    #\5 #\7 #\1 #\2 #\6 #\2 #\3 #\7 #\1 #\7 #\2 #\7 #\2)
  "Vector of character values accessible by
   (aref ... (- (char-int (char-upcase x)) (char-int #\A)))
   Cheat Note: this is true of ASCII, but *not* EBCDIC!")

(defun soundex (word &key abbrv-p)
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
