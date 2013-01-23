;;; -*-mode: Lisp; -*-
;;;
;;; Common Lisp N-ARY tree test package
;;;
;;; (C)opyright 2003, Greg Menke.  Contact: gdm-narytree@toadmail.com
;;; Released under the GNU GPL Version 2.
;;;
;;; 
;;; This program is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU General Public License
;;; as published by the Free Software Foundation; either version 2
;;; of the License, or (at your option) any later version.
;;; 
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;; 
;;; You should have received a copy of the GNU General Public License
;;; along with this program; if not, write to the Free Software
;;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
;;; 02111-1307, USA.
;;;
;;; GNU GPL: http://www.gnu.org/licenses/gpl.html
;;;

;;; This file loads several N-ARY tree test suites, and is not required
;;; for use of the N-ARY package.

(in-package :cl-user)


;;; Small test function.  Creates, exercises and returns a small n-ary
;;; tree.  Suitable for generally investigating the data structures and
;;; examining a simple case of rebalancing.
;;;
(defun testtree ()

  (let ((items  '(2 5 12 1 9 3 11 6 7 8 2 4 10 13 -5 60 99 12.3 30 -2 0 55 42 3.14))
        (tree    (n-ary-tree:make-ntree 5 
                                        :test #'< 
                                        :equal #'= 
                                        :test-skip 5 
                                        :threshold 0.2 
                                        :minhits-before-rebalance 1 )))

    (format t "Inserting items...~%")
    (loop for n in items
          do
          (n-ary-tree:ntree-insert-item tree n))

    (format t "Searching items...~%")
    (loop for i from 0 to 100
          do
          (unless (n-ary-tree:ntree-find-item tree 30)
            (error "item not found")))

    tree))





;;;
;;; For those running Lispworks, the following functions provide a
;;; GUI that displays the graph of a n-ary tree.  Invoke it as;
;;;
;;; (nary <tree>) where <tree> is a n-ary tree with some number of
;;; items inserted.  (nary (testtree)), for example.  It will work with
;;; a considerable number of items, though will become slow with very
;;; large sets.
;;;
#+LispWorks
(defun tree-items (node)
  (when (and node (n-ary-tree:ntree-node-p node))
    (list (cond ((n-ary-tree::ntree-node-leftnode node)) 
                (t (format nil "lnil")))

          (cond ((n-ary-tree::ntree-node-rightnode node)) 
                (t (format nil "rnil"))) )))


#+LispWorks
(defun node-print (node)
  (if (n-ary-tree:ntree-node-p node)
      (format nil "~A (~D ~D)" 
              (n-ary-tree::ntree-node-itemvector node)
              (n-ary-tree::ntree-node-lhits node)
              (n-ary-tree::ntree-node-rhits node) )
    node))

    
  
#+LispWorks
(let ((temptree    nil))

  (capi:define-interface nary-graph ()
    ()
    (:panes
     (graph-pane-1
      capi:graph-pane
      :children-function 'tree-items
      :print-function 'node-print
      :layout-function :top-down
      :roots (list (n-ary-tree::ntree-root-topnode temptree)) ))
    (:layouts
     (simple-pinboard-layout-1
      capi:simple-pinboard-layout
      '(graph-pane-1)))
    (:default-initargs
     :best-height 300
     :best-width 600
     :layout 'simple-pinboard-layout-1 ))



  (defun nary (tt &optional (title "N-ARY tree nodes"))
    (setf temptree tt)
    (capi:display (make-instance 'nary-graph :title title))) )






;;;
;;; A more demanding test suite that exercises and tests all N-ary tree
;;; features.  (More or less...).  It will take a while to run and will
;;; throw an error when a problem is found.
;;;
(defun testsuite ()

  (let ((rstate   (make-random-state))
        (items    nil)
        (unfound  0)
        (badfound 0))

    (let ((tree    (n-ary-tree:make-ntree 100
                                          :test #'< 
                                          :equal #'= 
                                          :test-skip 10
                                          :threshold 0.5 
                                          :minhits-before-rebalance 10 )))

      (format t "Making list of random items~%")
      (setf items (loop for i from 1 to 500000
                        collect
                        (random 90000000 rstate)))

      (format t "Inserting into rebalancing tree~%")
      (loop for i in items
            for counter from 1
            for msgcounter from 1
            with msginterval = 40000
            do 
            (n-ary-tree:ntree-insert-item tree i)
            (when (zerop (mod msgcounter msginterval)) 
              (setf msgcounter 0)
              (incf msginterval 5000)
              (format t "Tree size; ~D items, ~D nodes~%" 
                      (n-ary-tree::ntree-root-numitems tree)
                      (n-ary-tree::ntree-root-numnodes tree)))
            #+ignore (nary tree (format nil "N-ARY tree ~D" counter)) )
      
      (format t "Tree size; ~D items, ~D nodes~%" 
              (n-ary-tree::ntree-root-numitems tree) 
              (n-ary-tree::ntree-root-numnodes tree))
      (format t "Left rebalances  : ~A~%" (n-ary-tree::ntree-root-leftwards-balances tree))
      (format t "Right rebalances : ~A~%" (n-ary-tree::ntree-root-rightwards-balances tree))

      (format t "~%")
      (format t "Testing tree~%")
      (loop for i in items
            for counter from 1
            with res = nil
            do
            (unless (setf res (n-ary-tree:ntree-find-item tree i))
              (format t "Item number ~D, value = ~A not found~%" counter i)
              (incf unfound))

            (when res
              (unless (= res i)
                (format t "Wrong result; item num ~D, got ~A wanted ~A~%" counter res i)
                (incf badfound))) )

      (format t "~%")
      (format t "Unfound items: ~A~%" unfound)
      (format t "Found wrong  : ~A~%" badfound)
      (format t "Left rebalances  : ~A~%" (n-ary-tree::ntree-root-leftwards-balances tree))
      (format t "Right rebalances : ~A~%" (n-ary-tree::ntree-root-rightwards-balances tree)) 


      (when (or (not (zerop unfound)) (not (zerop badfound)))
        (error "Tree search failure"))


      (format t "~%")
      (format t "Deleting half the items from test-set~%")
      (loop for index from 0 below (/ (length items) 2)
            for i in items
            for counter from 1
            for msgcounter from 1
            with msginterval = 40000
            do
            (unless (n-ary-tree:ntree-delete-item tree i)
              (error "Failed to delete item number ~D, value = ~A" counter i))
            (when (zerop (mod msgcounter msginterval)) 
              (setf msgcounter 0)
              (incf msginterval 5000)
              (format t "~D items, ~D nodes remaining~%" 
                      (n-ary-tree::ntree-root-numitems tree)
                      (n-ary-tree::ntree-root-numnodes tree))) )


      (format t "~%")
      (format t "Tree size; ~D items, ~D nodes~%" 
              (n-ary-tree::ntree-root-numitems tree) 
              (n-ary-tree::ntree-root-numnodes tree))

      (format t "Left rebalances  : ~A~%" (n-ary-tree::ntree-root-leftwards-balances tree))
      (format t "Right rebalances : ~A~%" (n-ary-tree::ntree-root-rightwards-balances tree)) 


      (format t "~%")
      (format t "Confirming deletions~%")
      (loop for index from 0 below (/ (length items) 2)
            for i in items
            for counter from 1
            do
            (n-ary-tree:ntree-find-item tree i)
            ;; could be duplicates in the random set...
            #+ignore (when (n-ary-tree:ntree-find-item tree i)
                       (error "Item number ~D found!, value = ~A~%" counter i)))

      (format t "~%")
      (format t "Left rebalances  : ~A~%" (n-ary-tree::ntree-root-leftwards-balances tree))
      (format t "Right rebalances : ~A~%" (n-ary-tree::ntree-root-rightwards-balances tree)) 


      (format t "~%")
      (format t "Re-inserting the deleted items~%")
      (loop for i in items
            for index from 0 below (/ (length items) 2)
            for msgcounter from 1
            with msginterval = 40000
            do 
            (n-ary-tree:ntree-insert-item tree i)
            (when (zerop (mod msgcounter msginterval)) 
              (setf msgcounter 0)
              (incf msginterval 5000)
              (format t "Tree size; ~D items, ~D nodes~%" 
                      (n-ary-tree::ntree-root-numitems tree)
                      (n-ary-tree::ntree-root-numnodes tree))) )


      (format t "Tree size; ~D items, ~D nodes~%" 
              (n-ary-tree::ntree-root-numitems tree) 
              (n-ary-tree::ntree-root-numnodes tree))
      (format t "Left rebalances  : ~A~%" (n-ary-tree::ntree-root-leftwards-balances tree))
      (format t "Right rebalances : ~A~%" (n-ary-tree::ntree-root-rightwards-balances tree))


      (format t "~%")
      (format t "Testing tree~%")
      (loop for i in items
            for counter from 1
            with res = nil
            do
            (unless (setf res (n-ary-tree:ntree-find-item tree i))
              (error "Item number ~D, value = ~A not found~%" counter i))

            (when res
              (unless (= res i)
                (error "Wrong result; item num ~D, got ~A wanted ~A~%" counter res i) )))

      
      (format t "~%")
      (format t "Left rebalances  : ~A~%" (n-ary-tree::ntree-root-leftwards-balances tree))
      (format t "Right rebalances : ~A~%" (n-ary-tree::ntree-root-rightwards-balances tree)) 


      (format t "Repeatedly find the same key;~%")
      (time (loop for i from 0 below 350
            do
            (n-ary-tree:ntree-find-item tree (car items))) )
      (time (loop for i from 0 below 350
            do
            (n-ary-tree:ntree-find-item tree (car items))) )
      (time (loop for i from 0 below 350
            do
            (n-ary-tree:ntree-find-item tree (car items))) )

      (format t "~%")
      (format t "Left rebalances  : ~A~%" (n-ary-tree::ntree-root-leftwards-balances tree))
      (format t "Right rebalances : ~A~%" (n-ary-tree::ntree-root-rightwards-balances tree)) 


      (let ((ecount 0)
            (esum   0))
        (format t "~%")
        (format t "Mapping tree from min to max~%")
        (n-ary-tree:mapnary #'(lambda (e) (incf ecount) (incf esum e)) tree)
        (format t "~D items, sum is ~D~%" ecount esum) )


      (let ((min2max  nil)
            (max2min  nil))

        (format t "~%")
        (format t "Traversing tree from min to max~%")
        (loop for msgcounter from 0
              for counter from 0
              with msginterval = 40000
              with sv = (n-ary-tree:ntree-first-item tree)
              while sv
              do
              (push (n-ary-tree:ntree-item-value sv) min2max)
              (setf sv (n-ary-tree:ntree-next-item sv))
              (when (zerop (mod msgcounter msginterval)) 
                (setf msgcounter 0)
                (incf msginterval 5000)
                (format t "Got ~D items~%" counter))

              finally
              (format t "Got ~D items~%" (length min2max)))

        (format t "Traversing tree from max to min and reversing list~%")
        (loop for msgcounter from 0
              for counter from 0
              with msginterval = 40000
              with sv = (n-ary-tree:ntree-last-item tree)
              while sv
              do
              (push (n-ary-tree:ntree-item-value sv) max2min)
              (setf sv (n-ary-tree:ntree-prev-item sv))
              (when (zerop (mod msgcounter msginterval)) 
                (setf msgcounter 0)
                (incf msginterval 5000)
                (format t "Got ~D items~%" counter))
              finally
              (format t "Got ~D items~%" (length max2min)))
        (setf max2min (reverse max2min))

        (format t "Comparing lists~%")
        (unless (= (length min2max) (length max2min))
          (error "2 sets are of different length"))
        (format t "Sets are ~D items~%" (length min2max))

        (loop for i in min2max
              for j in max2min
              for counter from 0
              do
              (unless (= i j)
                (error "Sets differ at item ~D" counter))) )


      (format t "~%")
      )))






;;; A string based N-ARY tree instead of the random integer tree
;;; generated by testsuite.  This test is particularly useful when its
;;; result is passed to (nary).  Invoke testsuite2 with the pathname
;;; of a file.  Best results are obtained by taking a text file and
;;; processing it for one word per line, as the shell command seen
;;; below will do.
;;;
;;; cat docs/scarp10.txt | tr [:space:] \\n | grep ^. | sed -n 1,500p > testfile
;;;
;;; - take a copy of the Scarlet Pimpernel (downloaded from Project
;;; Gutenburg) in the docs subdirectory, turn spaces into newlines.
;;; Then take the first 500 non-empty lines and send them into the
;;; file 'testfile'.  Invoke the test function as; (testsuite2
;;; "testfile") or (nary (testsuite2 "testfile")) if you're running
;;; Lispworks.
;;;
(defun testsuite2 (txtfile)
  (let ((tree    (n-ary-tree:make-ntree 5
                                        :test #'string< 
                                        :equal #'string=
                                        :test-skip 2
                                        :threshold 0.5 
                                        :minhits-before-rebalance 2 )) )

    (with-open-file (fstr txtfile :direction :input :if-does-not-exist :error)

      (format t "~%Reading textlines from ~A~%" txtfile)

      (let ((testdata   nil))
        (loop for txtline = (read-line fstr nil nil)
              for msgcounter from 1
              with msginterval = 10000
              while txtline
              do
              (n-ary-tree:ntree-insert-item tree txtline)
              (push txtline testdata)
              (when (zerop (mod msgcounter msginterval))
                (setf msgcounter 0)
                (incf msginterval 5000)
                (format t "Tree size; ~D items, ~D nodes~%" 
                        (n-ary-tree::ntree-root-numitems tree)
                        (n-ary-tree::ntree-root-numnodes tree)))
              finally
              (format t "Tree size; ~D items, ~D nodes~%" 
                        (n-ary-tree::ntree-root-numitems tree)
                        (n-ary-tree::ntree-root-numnodes tree)))

        (format t "Testing tree~%")
        (loop for i in testdata
              do
              (unless (n-ary-tree::ntree-find-item tree i)
                (format t "warning: Item '~A' not found~%" i))) ))

    (format t "Left rebalances  : ~A~%" (n-ary-tree::ntree-root-leftwards-balances tree))
    (format t "Right rebalances : ~A~%" (n-ary-tree::ntree-root-rightwards-balances tree))

    (format t "~%")
    tree))

;;; eof
