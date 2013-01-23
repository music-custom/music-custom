;;; -*-mode: Lisp; -*-
;;;
;;; Common Lisp N-ARY tree package
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
;;;
;;; Contributions;
;;;
;;; 10/30/2003
;;;	Yin Wang, Tsinghua University, Beijing China
;;;     - contributed bug fixes for CMUCL, insert-before & after, and pop


;;;
;;;
;;; Summary;
;;;
;;; Allows a user to create a b-tree data structure with multiple
;;; elements at each node.  The advantage over a one-item-per-node
;;; binary tree is there is no additional per-item overhead.  This
;;; comes with a (possibly small) performance overhead because two
;;; searches must be done to find an item; first the node where the
;;; item could be is found, then that node must be itself searched for
;;; the item.  However, the lack of per-item overhead can be helpful
;;; when the tree must hold lots of data.  The N-ARY tree maintains
;;; its items in order, allowing definite order mapping and
;;; previous/next traversal.
;;;
;;;
;;; Details;
;;;
;;; At runtime, an N-ARY tree consists of a root and once items have
;;; been added, one or more nodes.  The root contains global
;;; information; item counts, top node, rebalancing parameters, etc.
;;; Each node contains some number > 0 of items, up to but not
;;; exceeding the given number of items per node.
;;;
;;; In a node, items are stored in a simple array, added via a
;;; conventional setf/svref operation and removed by overwriting the
;;; array reference.  The N-ARY tree implementation does not impose
;;; any kind of store/remove protocol on the items themselves.
;;;
;;; The N-ARY tree create function accepts a number of parameters
;;; listed below.  The optional parameters have naive defaults,
;;; suitable for experimentation, but certainly not for large
;;; datasets.
;;;
;;; items per node;
;;;
;;;   Required integer >= 5, indicates the size of the simple array to
;;;   be created for each node.  Each node's array is allocated when
;;;   the node is created, then filled as items are added to the node.
;;;   The internal node search function requires at least 3 items,
;;;   thus 5 is an arbitrarily larger value.
;;;
;;; key computation function;
;;;
;;;   Required function that takes an item and returns whatever
;;;   keyvalue is to be used to order the item.  The keyvalue is
;;;   tested only by the user-supplied ordering and equal functions.
;;;   By employing a key computation function, the N-ARY tree doesn't
;;;   need to retain each item's keyvalue, allowing the user to define
;;;   its characteristics.  The key computation function should be
;;;   state-free, nominally efficient and when applied to a given
;;;   item, should always produce the same keyvalue.  If an item's key
;;;   computation function produces a keyvalue that varies between
;;;   invocations, the item is not moved to its new sorted location
;;;   and the tree could become corrupt.  If an item so changes state
;;;   that its keyvalue will change and it must be moved to a new
;;;   location in the tree, the item should be deleted before the
;;;   state evolution, then re-inserted afterwards.  Default is the
;;;   identity function.
;;;
;;; ordering function;
;;;
;;;   Required two parameter function that returns t if the first
;;;   keyval is less than the second in a user-defined sense or else
;;;   nil.  This function is applied when the N-ARY tree
;;;   implementation needs to compare item keyvals and should be
;;;   efficient and state-free.  Since this function is frequently
;;;   used, it should be optimized as much as possible.  Default is <.
;;;
;;; equal function;
;;;
;;;   Optional function that takes two item keyvals, returning t if
;;;   they are equal in a user-defined sense else nil if not.  The
;;;   "user-defined sense" should be specific enough to define
;;;   'eq-ness for the type(s) added to the tree.  This function is
;;;   required if searching for an exact match is desired or if items
;;;   may be deleted from the tree.  If not supplied, a find operation
;;;   will return either an exact match or if the item is not present,
;;;   the item with greatest keyvalue less than the test keyvalue.
;;;   The function is called when the N-ARY tree implementation needs
;;;   to test if a given item has the same keyval as one submitted for
;;;   a search.  It should be state-free, but due to the relative
;;;   infrequency of its use, efficiency is not of prime concern.
;;;   Default is nil.
;;;
;;; test skip;
;;;
;;;   Causes rebalance testing to occur every mod n = 0 operations; n=5
;;;   means rebalance tests occur every 5th find/insert/delete
;;;   operation.  Default is 0, meaning no rebalance tests occur- which
;;;   is how the programmer can disable rebalancing.
;;;
;;; threshold;
;;;
;;;   When a rebalance test is applied, the difference between left and
;;;   right traversals must exceed this ratio before rebalancing is
;;;   performed.  Default is 50%.
;;;
;;; min hits before rebalance;
;;;
;;;   Specifies a minimum number of traversals through a node before
;;;   rebalance tests are applied, avoiding trivial rebalancing of
;;;   small, infrequently used sub-trees.  Default is 10.
;;;


;;;
;;; Key values vs items;
;;;
;;;   The N-ARY tree package does not define a keyvalue type.
;;;   Instead, the programmer must supply a function to compute a
;;;   value of some convienent type given an item, and another
;;;   function to compare the computed keyvalues of two items.  Each
;;;   item should produce a unique keyvalue as there is no other way
;;;   for items to be differentiated in the tree.  Multiple items that
;;;   produce the same keyvalue can be stored in the tree and they
;;;   will be adjacent in the sort order, however there are two
;;;   implications; First, their relative order is not defined and may
;;;   change.  Second, when the key is searched, one of the items will
;;;   be returned but it is not defined which.
;;;
;;;   Since the node item arrays are not typed, items of varying types
;;;   may be added to a given N-ARY tree.  The programmer must ensure
;;;   the key computation, comparision and equal functions are suited
;;;   to all datatypes stored in the tree.
;;;
;;; Search vectors;
;;;
;;;   Some N-ARY tree functions return search vectors.  A search
;;;   vector identifies the node and node array index of a particular
;;;   item.  It acts as a bookmark that allows the programmer to
;;;   retrieve the item itself or navigate from it to other items in
;;;   the tree.  Search vectors are not bound to the tree in any way,
;;;   so are not updated as the tree changes (or even when it is
;;;   garbage collected).  Once created, a search vector is guaranteed
;;;   to remain valid until an item is added or removed from the tree.
;;;   For this reason, the programmer should be cautious when
;;;   maintaining search vectors.  On the other hand, search vectors
;;;   are guaranteed to remain valid when the tree rebalances as
;;;   consequence of searches.
;;;
;;;   The term "search vector" is a misnomer, in fact a search vector
;;;   is an instance of a struct which contains the index and
;;;   reference to the node where an item is located.  This allows
;;;   more stringent type testing to be applied.  It is recommended
;;;   that the programmer not manipulate the contents of a search
;;;   vector.
;;;
;;;   Once created, search vectors are never modified.  If a search
;;;   vector is used to navigate a tree, each new vector is created
;;;   fresh.  For this reason, mapnary should be used for efficient
;;;   end-to-end traversal of a tree.
;;;
;;;   mapnary, ntree-next-item and ntree-prev-item will never cause
;;;   the tree to rebalance. 
;;;
;;;
;;; Multiprocessing;
;;;
;;;   N-ARY trees are not protected against re-entrancy.  If a N-ARY
;;;   tree has rebalancing disabled, multiple searches may be performed
;;;   in parallel.  However if rebalancing is enabled or inserts/deletes
;;;   will occur, the programmer must ensure access to the N-ARY tree is
;;;   serialized.
;;;
;;;
;;; Item Insertion;
;;;
;;;   As items are added to a node, the array will fill up at some
;;;   point.  When this occurs, the node is split, moving half the
;;;   items to a new node and keeping the rest.  By this means the
;;;   tree is grown.  If a new item that fills the node is to be added
;;;   to the first half of the node, then the new node is added to the
;;;   left of the split node, otherwise it it added to the right.
;;;
;;;   Since the item array is preallocated, it imposes its memory
;;;   overhead regardless of how full it is.  However, given a
;;;   non-pathological insert key distribution, as the tree grows, the
;;;   average node utilization will increase.
;;;
;;;
;;; Item Deletion;
;;;
;;;   An item is deleted by finding its node, then removing its
;;;   reference from the node's array, shifting the subsequent items
;;;   over the item's entry and nil filling in the end of the array.
;;;   When the last item in a node is deleted, the node is itself
;;;   removed.  If a deleted node has left and/or right branches, they
;;;   are re-linked to take its place.  Therefore, all nodes in the
;;;   tree will contain at least one item at all times.  The delete
;;;   operation requires that an equal function be supplied by the
;;;   user.
;;;
;;; Rebalancing;
;;;
;;;   The N-ARY implementation counts traversals from node to node,
;;;   down from the root node to leaf nodes, counting each branch
;;;   taken, either left or right.  Periodically during the traversal
;;;   of any node, the leftward branch counts are compared to the
;;;   rightwards branch counts.  If both numbers are more or less
;;;   equal, it means both of the node's branches are receiving more
;;;   or less the same frequency of use.  If one branch's count is
;;;   sufficiently larger than the other, then that branch is
;;;   considered to be more frequently traversed, so the more used
;;;   sub-tree is shifted one position closer to the node being
;;;   tested, and the less used sub-tree is shifted one position
;;;   further away.  By moving the more used sub-tree closer to the
;;;   root, the tree is biased in favor of the more used sub-tree.  By
;;;   keeping the frequency of left branching close enough to the
;;;   frequency of right branching, the tree tends to distribute
;;;   overhead across all queries.  Since rebalancing can occur at any
;;;   node, the tree as a whole reacts to the search key distribution.
;;;   The rebalance parameters affect how often the tests are
;;;   performed and how unbalanced the subtree must become before
;;;   rebalancing occurs.
;;;
;;;   Since the rebalancing parameters are supplied by the programmer,
;;;   they may or may not be appropriate in a given situation.  Poorly
;;;   chosen parameters are likely to lead to inefficient searches.
;;;
;;;   Regardless of the rebalance parameters chosen, with rebalancing
;;;   enabled there is nothing to stop the tree from thrashing given a
;;;   pathological search key distribution.  To minimize the impact of
;;;   such a situation, the programmer may choose to modify the
;;;   rebalance parameters from time to time to suit the application-
;;;   even to the extent of freezing the topology.
;;;
;;;   It is appealing to consider different rebalancing policies for
;;;   search, insert and delete, however on examination there is
;;;   little apparent need for such features.  This is because all
;;;   inserts and deletes must use a search to find the node to be
;;;   modified, so overall tree efficiency is best served by
;;;   rebalancing to serve searches.  As the tree is modified, the new
;;;   topology affects when and where rebalancing occurs, so given
;;;   suitable rebalance parameters and a reasonably non-pathological
;;;   sequence of key searches, overhead should hover around at least
;;;   a local minimum.  Infrequently traversed sub-trees may remain
;;;   degenerate for some time, however frequently traversed sub-trees
;;;   should tend towards maximum efficiency.  Since inserts/deletes
;;;   add and remove entire nodes, they will shift subtrees from one
;;;   part of larger tree to another leading to spikes in rebalancing
;;;   as the tree settles towards equilibrium.  Trees loaded once then
;;;   only searched will probably yield better long-term performance
;;;   than those which undergo frequent updates.
;;;
;;;   The following is an example of how rebalancing, insertion and
;;;   deletion can affect the topology of a tree.
;;;
;;;   Assume the following tree;
;;;
;;;       	    +--a--+
;;;		    |7,8  |
;;;		    +-----+
;;;       	    /      \ 
;;;            +--b--+      +--e----+
;;;	       |4    |	    |9,10,11|
;;;	       +-----+	    +-------+
;;;            /     \
;;;   	  +--c--+   +--d--+
;;;	  |1,2,3|   |5,6  |
;;;	  +-----+   +-----+
;;;			  
;;; 
;;;   After the last item in node b is deleted, the node itself is
;;;   removed and the nodes are shifted to take its place;
;;;
;;;       	    +--a--+
;;;		    |7,8  |
;;;		    +-----+
;;;       	    /      \ 
;;;           +--d--+      +--e----+
;;;	      |5,6  |      |9,10,11|
;;;	      +-----+      +-------+
;;;            /     
;;;   	  +--c--+   
;;;	  |1,2,3|   
;;;	  +-----+    
;;;
;;;
;;;   If more searches occur down the left branches than the right,
;;;   then the tree will rebalance- in this case with node d becoming
;;;   the new root;
;;;
;;;           +--d--+      
;;;	      |5,6  |      
;;;	      +-----+      
;;;            /    \ 
;;;   	  +--c--+   +--a--+
;;;	  |1,2,3|   |7,8  |
;;;	  +-----+   +-----+ 
;;;                    \ 	    
;;;		       +--e----+
;;;		       |9,10,11|
;;;		       +-------+
;;;
;;;
;;;   If item 2.5 is inserted, node c is split;
;;;
;;;           +--d--+      
;;;	      |5,6  |      
;;;	      +-----+      
;;;            /    \ 
;;;   	  +--c--+   +--a--+
;;;	  |1,2  |   |7,8  |
;;;	  +-----+   +-----+ 
;;;            \        \ 	    
;;;	     +--d--+    +--e----+
;;;	     |2.5,3|    |9,10,11|
;;;	     +-----+    +-------+
;;;
;;;
		   


;;;   Variables:
;;;
;;;   +NTREE-PACKAGE-VERSION+
;;;
;;;      Contains the release version of the N-ARY tree package.
;;;
;;;
;;;   Functions:
;;;
;;;   (ntree-root-p ntree)
;;;      
;;;      N-ARY tree type predicate, t if a N-ARY tree root else nil.
;;;      
;;;      
;;;   (ntree-node-p node)
;;;
;;;      N-ARY node type predicate, t if a N-ARY tree node else nil.
;;;
;;;
;;;   (ntree-sv-p sv)
;;;
;;;      N-ARY search vector type predicate, t if a search vector else
;;;      nil.
;;;
;;;
;;;   (make-ntree items-per-node &optional &key
;;;                                   ((:test testfunc) #'<) 
;;;                                   ((:equal eqfunc) nil) 
;;;                                   ((:key keyfunc) #'identity)
;;;                                   ((:test-skip testskip)                 *DEFAULT-TEST-SKIP* )
;;;                                   ((:threshold threshold)                *DEFAULT-THRESHOLD* )
;;;                                   ((:minhits-before-rebalance minhits)   *DEFAULT-MINHITS* ) )
;;;
;;;      Create an empty N-ARY tree, items-per-node is required and must
;;;      be >= 5.
;;;
;;;
;;;   (ntree-compute-item-keyval ntree item)
;;;
;;;      Given a N-ARY tree and an item, compute and return its keyval.
;;;      The item does not have to be present in the tree.
;;;
;;;
;;;   (ntree-find-item ntree keyval)
;;;
;;;      Find and return an item given a N-ARY tree and a keyval.  If
;;;      the tree defines an :equal function, the item will be returned
;;;      or nil if it is not present in the tree.  If :equal is not
;;;      defined, the item with greatest keyval < the test keyval is
;;;      returned, or nil is returned if the test keyval is < all
;;;      keyvals in the tree.
;;;
;;;
;;;   (ntree-search-item ntree keyval)
;;;
;;;      Find and return the location of an item given a N-ARY tree
;;;      and a keyval.  Similar to ntree-find-item except this
;;;      function returns nil or a search vector which identifies the
;;;      node and index where the item resides (or its next lowest
;;;      neighbor if :equal is not defined).  All search vectors
;;;      should be considered opaque, ntree-item-value is the
;;;      suggested function to yield an item given its search vector.
;;;
;;;
;;;   (ntree-insert-item ntree item)
;;;
;;;      Inserts the given item into its sorted location in the given
;;;      N-ARY tree.  Returns 3 values; respectively, the item's
;;;      computed keyval, a search vector representing the item, and a
;;;      reference to the item itself.
;;;
;;;
;;;   (ntree-insert-before-sv tree sv item)
;;;
;;;      Inserts the given item just before the item pointed to by sv.
;;;      Be careful, if item's computed keyvalue does not correspond
;;;      to item's position in the tree, the tree will become corrupt.
;;;
;;;
;;;   (ntree-insert-after-sv tree sv item)
;;;
;;;      Inserts the given item just after the item pointed to by sv.
;;;      Be careful, if item's computed keyvalue does not correspond
;;;      to item's position in the tree, the tree will become corrupt.
;;;
;;;
;;;   (ntree-delete-item ntree keyval)
;;;
;;;      Delete an item from the N-ARY tree given its keyval (or the 
;;;      item itself), returning the sv of the next item in the tree 
;;;      if the item was deleted or nil if the item was not found.  If
;;;      no items are after the deleted item, t is returned.
;;;
;;;
;;;   (ntree-pop tree)
;;;   
;;;      Removes the first item from the tree and returns it, or nil
;;;      if no items are present.  Makes a nary-tree act like a
;;;      priority queue.
;;;
;;;
;;;   (mapnary func tree &optional &key ((:from-end from-end) nil)
;;;                                     ((:cancel-p cancel-on-nil) nil)
;;;
;;;      Analagous to the various CL mapping functions, mapnary calls
;;;      func once for each item in the tree in sort order or reverse
;;;      sort order.  If :from-end is nil, mapping starts from the
;;;      first item in the sort order, if non-nil, the mapping starts
;;;      from the last item.  :from-end defaults to nil.  The mapped
;;;      function is called with one parameter, being the item itself.
;;;      If :cancel-p is non-nil, the mapping will terminate when the
;;;      function returns nil.  If :cancel-p is nil, mapping will
;;;      terminate only after processing all items in the tree.
;;;      :cancel-p defaults to nil.  mapnary returns nil and has no
;;;      intrinsic side-effects.  Results are undefined if the mapping
;;;      function changes the N-ARY tree being mapped.
;;;
;;;
;;;   (ntree-equal-sv-p sv1 sv2)
;;;
;;;      Given 2 search vectors, return t if they both refer to the
;;;      same item in the same tree, else nil.  This is an efficient
;;;      test that compares only the node and index, so does not
;;;      require that the tree define an equal function.
;;;
;;;
;;;   (ntree-item-value sv)
;;;
;;;      Given a search vector, return the item it represents.  If
;;;      items have been inserted or deleted since the search vector
;;;      was created, the results of using this function are
;;;      undefined.
;;;
;;;
;;;   (ntree-first-item tree)
;;;
;;;      Return a search vector representing the first item in the
;;;      tree's sort order.
;;;
;;;
;;;   (ntree-last-item tree)
;;;
;;;      Return a search vector representing the last item in the
;;;      tree's sort order.
;;;
;;;
;;;   (ntree-next-item sv)
;;;
;;;      Given a search vector, return a vector for the next item in
;;;      the tree's sort order or nil if sv is the last item.  If
;;;      items have been inserted or deleted since the search vector
;;;      was created, the results of using this function are
;;;      undefined.
;;;
;;;
;;;   (ntree-prev-item sv)
;;;
;;;      Given a search vector, return a vector for the previous item
;;;      in the tree's sort order or nil if sv is the first item.  If
;;;      items have been inserted or deleted since the search vector
;;;      was created, the results of using this function are
;;;      undefined.
;;;

(proclaim '(optimize (speed 3)))

(in-package :cl-user)


(defpackage "N-ARY-TREE"
  (:use "CL" )
  (:export "+NTREE-PACKAGE-VERSION+"
	   "NTREE-SV-P"
           "NTREE-ROOT-P"
           "NTREE-NODE-P"
           "NTREE-EQUAL-SV-P"
           "MAKE-NTREE"
	   "NTREE-COMPUTE-ITEM-KEYVAL"
           "NTREE-FIND-ITEM"
           "NTREE-SEARCH-ITEM"
           "NTREE-INSERT-ITEM"
           "NTREE-INSERT-BEFORE-SV"
           "NTREE-INSERT-AFTER-SV"
	   "NTREE-DELETE-ITEM"
           "MAPNARY"
           "NTREE-POP"
           "NTREE-ITEM-VALUE"
           "NTREE-FIRST-ITEM"
           "NTREE-LAST-ITEM"
           "NTREE-NEXT-ITEM"
           "NTREE-PREV-ITEM"))

(pushnew :n-ary-tree *features*)

(in-package :n-ary-tree)


(defvar +NTREE-PACKAGE-VERSION+         0.4)


(defparameter *DEFAULT-TEST-SKIP*       0)
(defparameter *DEFAULT-THRESHOLD*       1/2)
(defparameter *DEFAULT-MINHITS*         10)




(intern "DIR-LEFT")
(intern "DIR-RIGHT")


(defstruct ntree-sv
  node
  iindex)


(defstruct ntree-root 
  numitems
  numnodes
  maxitems
  eqfunc
  testfunc
  keyfunc
  op-counter
  leftwards-balances
  rightwards-balances
  test-skip
  threshold
  minhits-before-rebalance
  topnode )


(defstruct ntree-node
  itemvector
  itemcount
  lhits
  rhits
  parentnode
  leftnode
  rightnode )




(defmethod print-object ((sv ntree-sv) stream)
  (print-unreadable-object (sv stream :type t)
    (format stream "node ~A iindex ~D"
          (ntree-sv-node sv)
          (ntree-sv-iindex sv))))



(defmethod print-object ((n ntree-root) stream)
  (print-unreadable-object (n stream :type t)
    (format stream "numitems ~D numnodes ~D maxitems ~D ops ~D rleft ~D rright ~D"
          (ntree-root-numitems n)
          (ntree-root-numnodes n)
          (ntree-root-maxitems n)
          (ntree-root-op-counter n)
	  (ntree-root-leftwards-balances n)
	  (ntree-root-rightwards-balances n))))


(defmethod print-object ((n ntree-node) stream)
  (print-unreadable-object (n stream :type t :identity nil)
    (format stream "itemvector ~A itemcount ~D leftnode ~A rightnode ~A" 
          (ntree-node-itemvector n)
          (ntree-node-itemcount n)
          (ntree-node-leftnode n)
          (ntree-node-rightnode n))))



(deftype chk-ntree-sv   (&optional e) '(satisfies ntree-sv-p))
(deftype chk-ntree-root (&optional e) `(satisfies ntree-root-p))
(deftype chk-ntree-node (&optional e) `(satisfies ntree-node-p))




(defun recursive-node-count (node slotname &optional (levels 0))
  "Recursively traverse the given right/left slot down to the leaf
node, return a vector containing the left node and the number of nodes
traversed to get there."
  (if (slot-value node slotname)
      (recursive-node-count (slot-value node slotname)
                  slotname
                  (1+ levels))
    (vector node levels)))






(defun make-ntree (items-per-node &optional &key 
                                  ((:test testfunc) #'<) 
                                  ((:equal eqfunc) nil) 
                                  ((:key keyfunc) #'identity)
                                  ((:test-skip testskip)                 *DEFAULT-TEST-SKIP* )
                                  ((:threshold threshold)                *DEFAULT-THRESHOLD* )
                                  ((:minhits-before-rebalance minhits)   *DEFAULT-MINHITS* ) )
  "Create an empty root node, providing means for supplying the
items-per-node count, an ordering function, an equal function and a
key calculation function."

  (unless (and (integerp items-per-node) (>= items-per-node 5))
    (error "make-ntree; items-per-node must be integer >= 5, was ~A" items-per-node))

  (make-ntree-root :numitems 0
                   :numnodes 0
                   :maxitems items-per-node
                   :eqfunc   eqfunc
                   :testfunc testfunc
                   :keyfunc  keyfunc
                   :topnode  nil
                   :op-counter 0
                   :leftwards-balances 0
                   :rightwards-balances 0
                   :test-skip  testskip
                   :threshold  threshold
                   :minhits-before-rebalance minhits ))







(defun make-new-node (root parentnode) 
  "Internal function used to create new node structures as data is
added to the tree"
  (incf (ntree-root-numnodes root))
  (make-ntree-node :itemcount 0
                   :lhits 0
                   :rhits 0
                   :parentnode parentnode
                   :itemvector (make-array (ntree-root-maxitems root)
                                           :initial-element nil) ))








(defun ntree-compute-item-keyval (root item) 
  "Wrapper function that computes a key value given an item and the
key calculation expression."
  (check-type root (chk-ntree-root))
  (funcall (ntree-root-keyfunc root) item))



(defun item-lt-node-item (root node keyval iindex) 
  "Wrapper function that tests a supplied key value against an
arbitrary item already contained in a given node."
  (funcall (ntree-root-testfunc root)
           keyval
           (ntree-compute-item-keyval root (svref (ntree-node-itemvector node) iindex)) ))


(defun item-lt-node-min (root node keyval) 
  "Wrapper function that tests a given keyval against the minimum
keyval in a given node."
  (item-lt-node-item root node keyval 0))









(defun binary-search-node-vector (root node keyval fromrec torec)
  "Within a node, binary search for the maximum keyval that is < a
test keyval, returning the index of the item.  If the test item is <
the min array item, return -1 to represent the array head."

  (cond ((= fromrec torec)
         ;; only 1 item, discover the result
         (cond ((item-lt-node-item root node keyval fromrec)
                ;; keyval < 1st item, so index before fromrec
                (1- fromrec))
               (t
                ;; keyval >= 1st item, index fromrec
                fromrec)))
         

        ((= (- torec fromrec) 1)
         ;; down to 2 adjacent items, discover the result
         (cond ((item-lt-node-item root node keyval fromrec)
                ;; keyval < 1st item, so index before fromrec
                (1- fromrec))
               
               ((item-lt-node-item root node keyval torec)
                ;; keyval < 2nd item, so index fromrec (which is torec-1)
                fromrec)
               
               (t
                ;; keyval is >= 2nd item, index torec
                torec)))


        (t 
         (let ((midrec   (floor (/ (+ fromrec torec) 2))))
           (cond ((item-lt-node-item root node keyval midrec)
                  ;; new item is in 1st half
                  (binary-search-node-vector root node keyval fromrec midrec))
              
                 ((item-lt-node-item root node keyval torec)
                  ;; new item is in second half
                  (binary-search-node-vector root node keyval midrec torec) )

                 (t
                  ;; item is >= torec, so index torec
                  torec) )))))











(defun insert-item-into-node (root node keyval item &optional (index nil))
  "Given a keyval and node into which it will be added, find its
position and add it.  If the node is full, split off the first half of
the items if the new item is to be contained in that set, or split it
off to the right if the new item is to be contained there."

  (let ((numitems   (ntree-node-itemcount node)))

    (flet ((insert-item (iindex item)
             ;; insert item into node, returning the target index
             (when (< iindex numitems)
               ;; if inserting before last item, shift vector contents up by 1
               (let ((itemvector    (ntree-node-itemvector node)))
                 (loop for si from (1- numitems) downto iindex
                       for di from numitems by -1
                       do
                       (setf (svref itemvector di) (svref itemvector si)))) )

             (setf (svref (ntree-node-itemvector node) iindex) item)
             (incf (ntree-node-itemcount node))
             (incf (ntree-root-numitems root))
             (make-ntree-sv :node node :iindex iindex)) )



      (cond ((zerop numitems)
             ;; store first item into a new node
             (insert-item 0 item))


            ((< numitems (ntree-root-maxitems root))
             ;; enough space for this item, find the right place for
             ;; the new item & shift the subsequent items up to make
             ;; space
             (cond (index (insert-item index item)) ;;fixed index insertion

                   ((< numitems 3)
                    ;; 1 or 2 items in node, do a quick sequential
                    ;; test instead of a binary search
                    (cond ((item-lt-node-item root node keyval 0)
                           ;; index 0 is > item, insert there
                           (insert-item 0 item))

                          ((and (> numitems 1)
                                (item-lt-node-item root node keyval 1))
                           ;; index 1 is > item, insert there
                           (insert-item 1 item))

                          (t
                           ;; add onto end
                           (insert-item numitems item))) )

                   (t
                    ;; >= 3 items, do a binary search for the next
                    ;; lowest item
                    (insert-item (1+ (binary-search-node-vector root node keyval 0 (1- numitems)))
                                 item)) ))


            (t
             ;; node is full, split it
             (let* ((newnode       (make-new-node root node))
                    (fromvector    (ntree-node-itemvector node))
                    (tovector      (ntree-node-itemvector newnode))
                    (newindex      (1+ (binary-search-node-vector root node keyval 0 (1- numitems))))
                    (halfindex     (floor (/ numitems 2))) )
               

               ;; split the node to left or right
               (cond ((< newindex halfindex)
                      ;;(format t "splitting node left ~A(~D)~%" (ntree-node-itemvector node) (ntree-node-itemcount node))
                      ;; item is in first half of items, split the node out to the left
                      (setf (ntree-node-leftnode newnode) (ntree-node-leftnode node))
                      (when (ntree-node-leftnode newnode)
                        (setf (ntree-node-parentnode (ntree-node-leftnode newnode)) newnode))
                      
                      (setf (ntree-node-leftnode node) newnode)

                      ;; copy first half of items to new node
                      (loop for si from 0 below halfindex
                            for di from 0
                            do 
                            (setf (svref tovector di) (svref fromvector si)))

                      ;; shift 2nd half of items down to head of
                      ;; vector, clear rest of vector to nil
                      (loop for si from halfindex
                            for di from 0 below numitems
                            do
                            (setf (svref fromvector di)
                                  (when (< si numitems) (svref fromvector si))) )

                     (setf (ntree-node-itemcount node) (- numitems halfindex))
                     (setf (ntree-node-itemcount newnode) halfindex) )


                     (t
                      ;;(format t "splitting node right ~A(~D)~%" (ntree-node-itemvector node) (ntree-node-itemcount node))
                      ;; item in the 2nd half, split the node out to the right
                      (setf (ntree-node-rightnode newnode) (ntree-node-rightnode node))
                      (when (ntree-node-rightnode newnode)
                        (setf (ntree-node-parentnode (ntree-node-rightnode newnode)) newnode))
                      
                      (setf (ntree-node-rightnode node) newnode)
 
                      ;; copy second half of items to new node
                      (loop for si from halfindex below numitems
                            for di from 0
                            do 
                            (setf (svref tovector di) (svref fromvector si)))
                      ;; clear rest of items from source vector to nil
                      (loop for di from halfindex below numitems
                            do
                            (setf (svref fromvector di) nil))
                      
                      (setf (ntree-node-itemcount newnode) (- numitems halfindex))
                      (setf (ntree-node-itemcount node) halfindex) ) )

               ;;(format t "oldnode ~A(~D), newnode ~A(~D)~%~%" (ntree-node-itemvector node) (ntree-node-itemcount node) (ntree-node-itemvector newnode) (ntree-node-itemcount newnode))
               
               ;; then add it
               (insert-item-into-node root newnode keyval item) )) ))))
                












(defun replace-parent-link (root parent node newnode)
  "Replace the link from node's parent to newnode, updating newnode's
parent.  Handle the topnode as well."

  (when newnode
    (setf (ntree-node-parentnode newnode) parent))

  (unless parent
    (setf (ntree-root-topnode root) newnode))

  (when parent
    (cond ((eql node (ntree-node-leftnode parent))
           ;; we were left node
           (setf (ntree-node-leftnode parent) newnode))
          
          ((eql node (ntree-node-rightnode parent))
           ;; we were right node
           (setf (ntree-node-rightnode parent) newnode)))) )










(defmacro shift-nodes (root parent node 
                                   hitsmore 
                                   hitsless 
                                   rebalance-threshold 
                                   root-balancecount-slot 
                                   replacement-node
                                   node-link-into-slot
                                   node-link-out-slot )
"Macro that generates code to shift the tree one node rightwards or
leftwards" 
  `(when (>= (/ (- ,hitsmore ,hitsless) (+ ,hitsmore ,hitsless)) ,rebalance-threshold )
    ;; relative hits exceed threshold, shift node to rebalance

     ;;(format t "rebalance ~A lhits ~D, rhits ~D~%" ,root-balancecount-slot (ntree-node-lhits ,node) (ntree-node-rhits ,node))

     ;; clear hits
     (setf (ntree-node-lhits ,node) 0)
     (setf (ntree-node-rhits ,node) 0)

     ;; increment the leftwards/rightwards reblanace counter
     (incf (slot-value ,root ,root-balancecount-slot))

     ;; replace our parent's link to us with a link to our replacement node
     (replace-parent-link ,root ,parent ,node ,replacement-node)

     ;; save the replacement node's link that we're going to take over
     (let ((oldlink    (slot-value ,replacement-node ,node-link-into-slot)))
       ;; link replacement node to us
       (setf (slot-value ,replacement-node ,node-link-into-slot) ,node)
       ;; and switch our parent to replacement node
       (setf (ntree-node-parentnode ,node) ,replacement-node)

       ;; link to the replacement node's children from us
       (setf (slot-value ,node ,node-link-out-slot)  oldlink)
       (when oldlink
         (setf (ntree-node-parentnode oldlink) ,node)) ) ))








(defun rebalance-subtree (root node dir)
"Test the relative number of left vs right traversals and shift the
subtree starting from node accordingly"
  (let ((lefthits   (ntree-node-lhits node))
        (righthits  (ntree-node-rhits node))
        (parent     (ntree-node-parentnode node))
        (lnode      (ntree-node-leftnode node))
        (rnode      (ntree-node-rightnode node))

        (threshold  (ntree-root-threshold root)) )

    (cond ((> righthits lefthits)
           (shift-nodes root parent node 
                        righthits 
                        lefthits
                        threshold 
                        'n-ary-tree::leftwards-balances
                        rnode
                        'n-ary-tree::leftnode
                        'n-ary-tree::rightnode ))

          ((> lefthits righthits)
           (shift-nodes root parent node 
                        lefthits
                        righthits 
                        threshold 
                        'n-ary-tree::rightwards-balances
                        lnode
                        'n-ary-tree::rightnode
                        'n-ary-tree::leftnode )) )))






             
                   
             



(defmacro do-postfind (dirsymbol)
"Macro that tests if rebalance tests are to be applied."
  `(when (and postfindop

	      (not (zerop (ntree-root-test-skip root)))

              (zerop (mod (ntree-root-op-counter root)
                          (ntree-root-test-skip root)))

              (>= (+ (ntree-node-lhits node) (ntree-node-rhits node))
                  (ntree-root-minhits-before-rebalance root)) )

     (funcall postfindop root node ,dirsymbol)) )







(defun find-node (root node keyval postfindop) 
  "Find the node that a test keyval would be in if its present in the
tree.  Return the node."

  (let ((leftnode   (ntree-node-leftnode  node))
        (rightnode  (ntree-node-rightnode node)) )

    (flet ((search-leftnode   ()
             (progn
               (let ((rv (find-node root leftnode keyval postfindop)))
                 (incf (ntree-node-lhits node))
                 (do-postfind 'n-ary-tree::dir-left)
                 rv)) )

           (search-rightnode  ()
             (progn
               (let ((rv (find-node root rightnode keyval postfindop)))
                 (incf (ntree-node-rhits node))
                 (do-postfind 'n-ary-tree::dir-right)
                 rv))) )


      ;; see if new item is < the min item in this node
      (cond ((item-lt-node-min root node keyval)
             ;; new item is < the min node item
             (cond (leftnode
                    ;; move to next left node
                    (search-leftnode))
               
                   (t 
                    ;; no leftnode, return this node
                    node) ))


            (rightnode
             ;; we're >= min item of rightnode, keep searching
             (let ((rn  (search-rightnode)))
               (cond ((item-lt-node-min root rn keyval)
                      ;; new item is < min item of the minimum node of
                      ;; the right hand subtree so it must exist in
                      ;; node, so return
                      node)

                     (t 
                      ;; new item is not < min item of the minimum
                      ;; node of the right hand subtree, so the new
                      ;; item must exist there
                      rn))))


            (t
             ;; no rightnode so key must exist in this node, return it
             node)))))














(defun ntree-search-item (root keyval &optional (ignore-eqfunc nil))
  "Given a keyval, search the tree for an item it corresponds to-
returning either nil for not found or a vector containing the node and
index where the item exists.  The location spec is guaranteed valid
until items are added or removed from the tree."

  (check-type root (chk-ntree-root))

  (when (ntree-root-topnode root)
    (incf (ntree-root-op-counter root))

    (let* ((node          (find-node root 
                                     (ntree-root-topnode root)
                                     keyval 
                                     #'rebalance-subtree))
           
           (iindex        (binary-search-node-vector root 
                                                     node 
                                                     keyval 
                                                     0 
                                                     (1- (ntree-node-itemcount node)))) )

      ;;(format t "found ~A, index ~D, search ~A~%" (ntree-node-itemvector node) iindex keyval)

      (cond ((= iindex -1)
             ;; test keyval < all keyvals in tree
             nil)


            ((and (ntree-root-eqfunc root) (not ignore-eqfunc))
	     ;; apply eq function if user gave us one supplied
	     (cond ((funcall (ntree-root-eqfunc root) 
                             keyval
                             (ntree-compute-item-keyval root (svref (ntree-node-itemvector node) iindex)))
		    ;; equal test passed, we matched
		    (make-ntree-sv :node node :iindex iindex))

		   (t 
		    ;; we didn't match
		    nil)))

	    (t 
	     ;; no eqfunc defined, assume we matched the item
	     (make-ntree-sv :node node :iindex iindex)) )) ))








(defun ntree-find-item (root keyval &optional (ignore-eqfunc nil))
  "Given a keyval, search the tree for the item corresponding to it,
returning the item if found or nil if not."

  (check-type root (chk-ntree-root))

  (when (ntree-root-topnode root)

    (let ((pos     (ntree-search-item root keyval ignore-eqfunc)))
      (cond (pos
	     ;; found the item, return it
	     (svref (ntree-node-itemvector (ntree-sv-node pos))
		    (ntree-sv-iindex pos )))

	    (t
	     ;; not definite.  this can only happen when
	     ;; eqfunc is specified and the items didn't match
	     nil))) ))













(defun ntree-insert-item (root item) 
  "Insert a new item into the tree.  Return its keyval, the new item's
location spec and the item itself."

  (check-type root (chk-ntree-root))

  (let ((keyval       (ntree-compute-item-keyval root item))
        (targetpos    nil))

    (incf (ntree-root-op-counter root))
    
    (cond ((not (ntree-root-topnode root))
           ;; handle adding very first item
           (let ((tmpnode       (make-new-node root nil)))
             (setf (ntree-root-topnode root) tmpnode)
             (setf targetpos (insert-item-into-node root tmpnode keyval item)) ))

          (t 
           (let ((tmpnode      (find-node root 
                                          (ntree-root-topnode root) 
                                          keyval 
                                          #'rebalance-subtree)))
             (unless tmpnode
               (error "find-node returned nil"))

             (setf targetpos   (insert-item-into-node root tmpnode keyval item))) ))

    (values keyval targetpos item) ))










(defun ntree-delete-item (root keyval)
  "Given a keyval or item, search the tree for the item.  If found, delete the 
item, removing the node if its empty, ultimately returning an sv for the next item, 
else do nothing and return nil.  If there is no following item, t is returned."

  (check-type root (chk-ntree-root))

  (unless (ntree-root-eqfunc root)
    (error "equal function not specified for the tree"))

  (when (ntree-root-topnode root)
    (incf (ntree-root-op-counter root))

    (let* ((pos	    (cond ((ntree-sv-p keyval) keyval)
                          (t (ntree-search-item root keyval))))
           (posnext (ntree-next-item pos)))

      (cond ((and pos (> (ntree-sv-iindex pos) -1))
	     ;; found the item, delete it
	     (let ((node	(ntree-sv-node pos))
		   (iindex	(ntree-sv-iindex pos)))

               ;;;(format t "deleting item from node: vector before ~A" (ntree-node-itemvector node))

	       ;; if item is not last in the vector
	       (when (< iindex (1- (ntree-node-itemcount node)))
		 (loop for si from (1+ iindex) below (ntree-node-itemcount node)
		       do
		       (setf (svref (ntree-node-itemvector node) (1- si))
			     (svref (ntree-node-itemvector node) si)))
                 ;; if we're shifting items, the next one is shifted onto pos, so
                 ;; we use pos's sv instead.
                 (setf posnext t))

               ;; if pos was the last item in the tree...
               (unless posnext
                 (setf posnext t))
	     
	       ;; ensure we clear the old tail of the vector
	       (setf (svref (ntree-node-itemvector node) (1- (ntree-node-itemcount node))) 
                     nil)

	       (decf (ntree-root-numitems root))
	       (decf (ntree-node-itemcount node))

               ;;(format t " after ~A~%" (ntree-node-itemvector node))

	       (when (zerop (ntree-node-itemcount node))
		 (decf (ntree-root-numnodes root))

                 ;;(format t "deleting node ~%" node)

		 (let ((parent	    (ntree-node-parentnode node))
                       (leftnode    (ntree-node-leftnode node))
                       (rightnode   (ntree-node-rightnode node)) )

                   (when parent 
                     (setf (ntree-node-lhits parent) 0)
                     (setf (ntree-node-rhits parent) 0))
		   
                   (cond ((and (not leftnode)
                               (not rightnode))
                          ;; no child nodes, just collapse upwards
                          (replace-parent-link root parent node nil))


                         ((and (not leftnode)
                               rightnode)
                          ;; no leftnode but have rightnodes
                          (replace-parent-link root parent node rightnode))
				

                         ((and leftnode
                               (not rightnode))
                          ;; have leftnodes but no rightnode
                          (replace-parent-link root parent node leftnode))
			  
                         (t
                          ;; have both leftnodes and rightnodes
                          (let* ((leftpoint   (recursive-node-count leftnode  'rightnode))
                                 (rightpoint  (recursive-node-count rightnode 'leftnode)))
                            
                            (cond ((< (aref leftpoint 1)
                                      (aref rightpoint 1))
                                   ;; leftwards subtree has fewer
                                   ;; levels than rightwards subtree
                                   ;; so replace the deleted node with
                                   ;; the rightwards subtree, add the
                                   ;; leftwards subtree to the
                                   ;; rightward subtree's minimum
                                   ;; node's left link
                                     
                                   ;; point rightnode to parent
                                   (replace-parent-link root parent node rightnode)
                                   ;; point min node of right subtree to leftnode
                                   (setf (ntree-node-leftnode (aref rightpoint 0))
                                         leftnode)
                                   ;; make leftnode's parent the min
                                   ;; node of right subtree
                                   (setf (ntree-node-parentnode leftnode)
                                         (aref rightpoint 0))
                                   ;; clear hits in right subtree's min node
                                   (setf (ntree-node-lhits (aref rightpoint 0)) 0)
                                   (setf (ntree-node-rhits (aref rightpoint 0)) 0) )
                                  

                                  (t
                                   ;; rightwards subtree has <= levels
                                   ;; than leftwards subtree so
                                   ;; replace the deleted node with
                                   ;; the leftwards subtree, add the
                                   ;; rightwards subtree to the
                                   ;; leftward subtree's minimum
                                   ;; node's right link
                                   
                                   ;; point leftnode to parent
                                   (replace-parent-link root parent node leftnode)
                                   ;; point min node of left subtree
                                   ;; to rightnode
                                   (setf (ntree-node-rightnode (aref leftpoint 0))
                                         rightnode)
                                   ;; make leftnode's parent the min
                                   ;; node of right subtree
                                   (setf (ntree-node-parentnode rightnode)
                                         (aref leftpoint 0))
                                   ;; clear hits in right subtree's min node
                                   (setf (ntree-node-lhits (aref leftpoint 0)) 0)
                                   (setf (ntree-node-rhits (aref leftpoint 0)) 0) ) )))))) )
             posnext )
      
            (t nil)) )))
	       
	       





(defun ntree-pop (root)
  "Remove the first item in the tree and return it."

  (check-type root (chk-ntree-root))

  (unless (zerop (ntree-numitems root))

    (let* ((first-sv   (ntree-first-item root))
	   (first-item (ntree-item-value first-sv)) )
      ;;(format t "first sv: ~A, first item ~A~%" first-sv first-item)
      (ntree-delete-item root first-sv)
      first-item )))






(defun ntree-equal-sv-p (sv1 sv2)
"Return t if two search vectors point to the same item"
  (when (and sv1 sv2)
    (check-type sv1 (chk-ntree-sv))
    (check-type sv2 (chk-ntree-sv))
    (and (= (ntree-sv-iindex sv1) (ntree-sv-iindex sv2))
         (eql (ntree-sv-node sv1) (ntree-sv-node sv2)))))







(defun ntree-item-value (sv)
"Return the item from a N-ARY search vector."
  (when sv
    (check-type sv    (chk-ntree-sv))
    (svref (ntree-node-itemvector (ntree-sv-node sv)) (ntree-sv-iindex sv))))







(defun mapnary (func tree &optional &key ((:from-end from-end) nil)
                                         ((:cancel-p cancel-on-nil) nil))
"N-ARY tree mapping function.  :from-end t maps from last to first,
:cancel-p t stops mapping if the user function returns nil."
  (check-type tree (chk-ntree-root))

  (when (ntree-root-topnode tree)
    (catch 'cancel

      (flet ((process-item (node iindex)
               "Applies the user lambda to an item."
               (when (and (not (funcall func (svref (ntree-node-itemvector node) iindex)))
                          cancel-on-nil)
                 (throw 'cancel nil) )))


        (let ((mfunc     #'(lambda (node mfunc)
                             "Depth-first recurses from one end of the
tree items to the other, direction depends on the user parameter."
                             (let ((leftnode   (ntree-node-leftnode  node))
                                   (rightnode  (ntree-node-rightnode node)) )

                               (cond (from-end
                                      (when rightnode (funcall mfunc rightnode mfunc))
                                      (loop for iindex from (1- (ntree-node-itemcount node)) downto 0
                                            do (process-item node iindex))
                                      (when leftnode  (funcall mfunc leftnode mfunc)))

                                     (t 
                                      (when leftnode  (funcall mfunc leftnode mfunc))
                                      (loop for iindex from 0 below (ntree-node-itemcount node)
                                            do (process-item node iindex))
                                      (when rightnode (funcall mfunc rightnode mfunc))) )))))

          
          (funcall mfunc (ntree-root-topnode tree) mfunc) ))))
  nil)







(defun fitem-recurse-left (node)
"Support function that recurses down a subtree to find the node with
the first item in the sort order."
  (let ((leftnode   (ntree-node-leftnode  node)))
    (if leftnode  
        (fitem-recurse-left leftnode)
      ;; else
        (make-ntree-sv :node node :iindex 0))))


(defun fitem-recurse-right (node)
"Support function that recurses down a subtree to find the node with
the last item in the sort order."
  (let ((rightnode   (ntree-node-rightnode  node)))
    (if rightnode  
        (fitem-recurse-right rightnode)
      ;; else
        (make-ntree-sv :node node :iindex (1- (ntree-node-itemcount node))))))




  

(defun ntree-first-item (tree)
"Return the search vector for the first item in the N-ARY tree sort
order."
  (check-type tree (chk-ntree-root))
  (when (ntree-root-topnode tree)
    (fitem-recurse-left (ntree-root-topnode tree))))



(defun ntree-last-item (tree)
"Return the search vector for the last item in the N-ARY tree sort
order."

  (check-type tree (chk-ntree-root))
  (when (ntree-root-topnode tree)
    (fitem-recurse-right (ntree-root-topnode tree))))







(defun ntree-insert-before-sv (root sv item)
"<DANGEROUS>; Insert an item before the item pointed to by sv.  Using
this function can lead to corruption of the tree if the computed
keyval doesn't match the item's position, please be careful."

  (check-type root (chk-ntree-root))

  (when sv 
    (check-type sv    (chk-ntree-sv))

    (let ((node      (ntree-sv-node sv))
          (iindex    (ntree-sv-iindex sv)))
      (insert-item-into-node root node 0 item iindex))))



          
(defun ntree-insert-after-sv (root sv item)
"<DANGEROUS>; Insert an item after the item pointed to by sv.  Using
this function can lead to corruption of the tree if the computed
keyval doesn't match the item's position, please be careful."

  (check-type root (chk-ntree-root))

  (when sv 
    (check-type sv    (chk-ntree-sv))

    (let ((node      (ntree-sv-node sv))
          (iindex    (ntree-sv-iindex sv)))
      (insert-item-into-node root node 0 item (1+ iindex)))))





(defun ntree-next-item (sv)
"Given a search vector, return the sv for thenext item in the sort order or nil
if the search vector was the last in the tree."

  (when sv 
    (check-type sv    (chk-ntree-sv))

    (let ((node      (ntree-sv-node sv))
          (iindex    (ntree-sv-iindex sv)))

      (cond ((= iindex (1- (ntree-node-itemcount node)))
             ;; last item in node
             (cond ((ntree-node-rightnode node)
                    ;; find min item of right subtree
                    (fitem-recurse-left (ntree-node-rightnode node)))

                   ((ntree-node-parentnode node)
                    ;; see if we're the left or right node of our
                    ;; parent
                    (cond ((eql node (ntree-node-leftnode (ntree-node-parentnode node)))
                           ;; we're the leftnode
                           (make-ntree-sv :node (ntree-node-parentnode node) :iindex 0))

                          ((eql node (ntree-node-rightnode (ntree-node-parentnode node)))
                           ;; we're the rightnode which means we've
                           ;; traversed a subtree, so run up the tree
                           ;; and find the first parent that is
                           ;; reached via a leftnode, we will continue
                           ;; from its first item.  If we search all
                           ;; the way back to the root and are not a
                           ;; leftnode, then we've finished searching,
                           ;; so return nil.
                           (loop with tnode = node
                                 with pnode = (ntree-node-parentnode node)
                                 do
                                 (when (eql tnode (ntree-node-leftnode pnode))
                                   (return (make-ntree-sv :node pnode :iindex 0)))
                                 (setf tnode pnode)
                                 (setf pnode (ntree-node-parentnode tnode))
                                 (unless pnode
                                   (return nil))) )

                          (t (error "nary-next-item logic is insane!"))))

                   (t nil)))

            (t
             (make-ntree-sv :node node :iindex (1+ iindex))) ))))




(defun ntree-prev-item (sv)
"Given a search vector, return the sv for the previous item in the sort order or
nil if the search vector was the first in the tree."

  (when sv
    (check-type sv    (chk-ntree-sv))

    (let ((node      (ntree-sv-node sv))
          (iindex    (ntree-sv-iindex sv)))

      (cond ((zerop iindex)
             ;; first item in node
             (cond ((ntree-node-leftnode node)
                    ;; find max item of left subtree
                    (fitem-recurse-right (ntree-node-leftnode node)))

                   ((ntree-node-parentnode node)
                    ;; see if we're the left or right node of our
                    ;; parent
                    (cond ((eql node (ntree-node-rightnode (ntree-node-parentnode node)))
                           ;; we're the rightnode
                           (make-ntree-sv :node (ntree-node-parentnode node) :iindex (1- (ntree-node-itemcount 
                                                                                          (ntree-node-parentnode node)))))
                          
                          ((eql node (ntree-node-leftnode (ntree-node-parentnode node)))
                           ;; we're the leftnode which means we've
                           ;; traversed a subtree, so run up the tree
                           ;; and find the first parent that is
                           ;; reached via a rightnode, we will
                           ;; continue from its last item.  If we
                           ;; search all the way back to the root and
                           ;; are not a rightnode, then we've finished
                           ;; searching, so return nil.
                           (loop with tnode = node
                                 with pnode = (ntree-node-parentnode node)
                                 do
                                 (when (eql tnode (ntree-node-rightnode pnode))
                                   (return (make-ntree-sv :node pnode :iindex (1- (ntree-node-itemcount pnode)))))
                                 (setf tnode pnode)
                                 (setf pnode (ntree-node-parentnode tnode))
                                 (unless pnode
                                   (return nil))) )

                          (t (error "nary-prev-item logic is insane!"))))

                   (t nil)))

            (t
             (make-ntree-sv :node node :iindex (1- iindex))) ))))





;;; eof
