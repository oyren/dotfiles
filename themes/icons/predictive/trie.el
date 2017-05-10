;;; trie.el --- Trie data structure  -*- lexical-binding: t; -*-

;; Copyright (C) 2008-2010, 2012  Free Software Foundation, Inc

;; Author: Toby Cubitt <toby-predictive@dr-qubit.org>
;; Version: 0.2.6
;; Keywords: extensions, matching, data structures
;;           trie, ternary search tree, tree, completion, regexp
;; Package-Requires: ((tNFA "0.1.1") (heap "0.3"))
;; URL: http://www.dr-qubit.org/emacs.php
;; Repository: http://www.dr-qubit.org/git/predictive.git

;; This file is part of Emacs.
;;
;; GNU Emacs is free software: you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free
;; Software Foundation, either version 3 of the License, or (at your option)
;; any later version.
;;
;; GNU Emacs is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
;; more details.
;;
;; You should have received a copy of the GNU General Public License along
;; with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.


;;; Commentary:
;;
;; Quick Overview
;; --------------
;; A trie is a data structure used to store keys that are ordered sequences of
;; elements (vectors, lists or strings in Elisp; strings are by far the most
;; common), in such a way that both storage and retrieval are space- and
;; time-efficient. But, more importantly, a variety of more advanced queries
;; can also be performed efficiently: for example, returning all strings with
;; a given prefix, searching for keys matching a given wildcard pattern or
;; regular expression, or searching for all keys that match any of the above
;; to within a given Lewenstein distance (though this last is not yet
;; implemented in this package - code contributions welcome!).
;;
;; You create a trie using `make-trie', create an association using
;; `trie-insert', retrieve an association using `trie-lookup', and map over a
;; trie using `trie-map', `trie-mapc', `trie-mapcar', or `trie-mapf'. You can
;; find completions of a prefix sequence using `trie-complete', or search for
;; keys matching a regular expression using `trie-regexp-search'. Using
;; `trie-stack', you can create an object that allows the contents of the trie
;; to be used like a stack, useful for building other algorithms on top of
;; tries; `trie-stack-pop' pops elements off the stack one-by-one, in
;; "lexical" order, whilst `trie-stack-push' pushes things onto the
;; stack. Similarly, `trie-complete-stack', and `trie-regexp-stack' create
;; "lexically-ordered" stacks of query results.
;;
;; Note that there are two uses for a trie: as a lookup table, in which case
;; only the presence or absence of a key in the trie is significant, or as an
;; associative array, in which case each key carries some associated
;; data. Libraries for other data structure often only implement lookup
;; tables, leaving it up to you to implement an associative array on top of
;; this (by storing key+data pairs in the data structure's keys, then defining
;; a comparison function that only compares the key part). For a trie,
;; however, the underlying data structures naturally support associative
;; arrays at no extra cost, so this package does the opposite: it implements
;; associative arrays, and leaves it up to you to use them as lookup tables if
;; you so desire.
;;
;;
;; Different Types of Trie
;; -----------------------
;; There are numerous ways to implement trie data structures internally, each
;; with its own time- and space-efficiency trade-offs. By viewing a trie as a
;; tree whose nodes are themselves lookup tables for key elements, this
;; package is able to support all types of trie in a uniform manner. This
;; relies on there existing (or you writing!) an Elisp implementation of the
;; corresponding type of lookup table. The best type of trie to use will
;; depend on what trade-offs are appropriate for your particular
;; application. The following gives an overview of the advantages and
;; disadvantages of various types of trie. (Not all of the underlying lookup
;; tables have been implemented in Elisp yet, so using some of the trie types
;; described below would require writing the missing Elisp package!)
;;
;;
;; One of the most effective all-round implementations of a trie is a ternary
;; search tree, which can be viewed as a tree of binary trees. If basic binary
;; search trees are used for the nodes of the trie, we get a standard ternary
;; search tree. If self-balancing binary trees are used (e.g. AVL or red-black
;; trees), we get a self-balancing ternary search tree. If splay trees are
;; used, we get yet another self-organising variant of a ternary search
;; tree. All ternary search trees have, in common, good space-efficiency. The
;; time-efficiency of the various trie operations is also good, assuming the
;; underlying binary trees are balanced. Under that assumption, all variants
;; of ternary search trees described below have the same asymptotic
;; time-complexity for all trie operations.
;;
;; Self-balancing trees ensure the underlying binary trees are always close to
;; perfectly balanced, with the usual trade-offs between the different the
;; types of self-balancing binary tree: AVL trees are slightly more efficient
;; for lookup operations than red-black trees, at a cost of slightly less
;; efficienct insertion operations, and less efficient deletion
;; operations. Splay trees give good average-case complexity and are simpler
;; to implement than AVL or red-black trees (which can mean they're faster in
;; practice!), at the expense of poor worst-case complexity.
;;
;; If your tries are going to be static (i.e. created once and rarely
;; modified), then using perfectly balanced binary search trees might be
;; appropriate. Perfectly balancing the binary trees is very inefficient, but
;; it only has to be when the trie is first created or modified. Lookup
;; operations will then be as efficient as possible for ternary search trees,
;; and the implementation will also be simpler (so probably faster) than a
;; self-balancing tree, without the space and time overhead required to keep
;; track of rebalancing.
;;
;; On the other hand, adding data to a binary search tree in a random order
;; usually results in a reasonably balanced tree. If this is the likely
;; scenario, using a basic binary tree without bothering to balance it at all
;; might be quite efficient, and, being even simpler to implement, could be
;; quite fast overall.
;;
;;
;; A digital trie is a different implementation of a trie, which can be viewed
;; as a tree of arrays, and has different space- and time-complexities than a
;; ternary search tree. Roughly speaking, a digital trie has worse
;; space-complexity, but better time-complexity. Using hash tables instead of
;; arrays for the nodes gives something similar to a digital trie, potentially
;; with better space-complexity and the same amortised time-complexity, but at
;; the expense of occasional significant inefficiency when inserting and
;; deleting (whenever a hash table has to be resized). Indeed, an array can be
;; viewed as a perfect hash table, but as such it requires the number of
;; possible values to be known in advance.
;;
;; Finally, if you really need optimal efficiency from your trie, you could
;; even write a custom type of underlying lookup table, optimised for your
;; specific needs.
;;
;; This package uses the AVL tree package avl-tree.el, the tagged NFA package
;; tNFA.el, and the heap package heap.el.


;;; Code:

(eval-when-compile (require 'cl))
(require 'avl-tree)
(require 'heap)
(require 'tNFA)



;;; ================================================================
;;;                   Pre-defined trie types

(defconst trie--types '(avl))


;; --- avl-tree ---
(put 'avl :trie-createfun
     (lambda (cmpfun _seq) (avl-tree-create cmpfun)))
(put 'avl :trie-insertfun 'avl-tree-enter)
(put 'avl :trie-deletefun 'avl-tree-delete)
(put 'avl :trie-lookupfun 'avl-tree-member)
(put 'avl :trie-mapfun 'avl-tree-mapc)
(put 'avl :trie-emptyfun 'avl-tree-empty)
(put 'avl :trie-stack-createfun 'avl-tree-stack)
(put 'avl :trie-stack-popfun 'avl-tree-stack-pop)
(put 'avl :trie-stack-emptyfun 'avl-tree-stack-empty-p)
(put 'avl :trie-transform-for-print 'trie--avl-transform-for-print)
(put 'avl :trie-transform-from-read 'trie--avl-transform-from-read)



;;; ================================================================
;;;           Internal utility functions and macros

;;; ----------------------------------------------------------------
;;;           Functions and macros for handling a trie.

;; symbol used to denote a trie leaf node
(defconst trie--terminator '--trie--terminator)

(defstruct
  (trie-
   :named
   (:constructor nil)
   (:constructor trie--create
		 (comparison-function &optional (type 'avl)
		  &aux
		  (_dummy
		   (or (memq type trie--types)
		       (error "trie--create: unknown trie TYPE, %s" type)))
		  (createfun (get type :trie-createfun))
		  (insertfun (get type :trie-insertfun))
		  (deletefun (get type :trie-deletefun))
		  (lookupfun (get type :trie-lookupfun))
		  (mapfun (get type :trie-mapfun))
		  (emptyfun (get type :trie-emptyfun))
		  (stack-createfun (get type :trie-stack-createfun))
		  (stack-popfun (get type :trie-stack-popfun))
		  (stack-emptyfun (get type :trie-stack-emptyfun))
		  (transform-for-print (get type :trie-transform-for-print))
		  (transform-from-read (get type :trie-transform-from-read))
		  (cmpfun (trie--wrap-cmpfun comparison-function))
		  (root (trie--node-create-root createfun cmpfun))
		  ))
   (:constructor trie--create-custom
		 (comparison-function
		  &key
		  (createfun 'avl-tree-create-bare)
		  (insertfun 'avl-tree-enter)
		  (deletefun 'avl-tree-delete)
		  (lookupfun 'avl-tree-member)
		  (mapfun 'avl-tree-mapc)
		  (emptyfun 'avl-tree-empty)
		  (stack-createfun 'avl-tree-stack)
		  (stack-popfun 'avl-tree-stack-pop)
		  (stack-emptyfun 'avl-tree-stack-empty-p)
		  (transform-for-print 'trie--avl-transform-for-print)
		  (transform-from-read 'trie--avl-transform-from-read)
		  &aux
		  (cmpfun (trie--wrap-cmpfun comparison-function))
		  (root (trie--node-create-root createfun cmpfun))
		  ))
   (:copier nil))
  root comparison-function cmpfun
  createfun insertfun deletefun lookupfun mapfun emptyfun
  stack-createfun stack-popfun stack-emptyfun
  transform-for-print transform-from-read print-form)


(defmacro trie-lexical-binding-p ()
  "Return non-nil if lexical binding is in effect, nil otherwise."
  (let ((tempvar (make-symbol "x")))
    `(let ((,tempvar nil)
           (f (let ((,tempvar t)) (lambda () ,tempvar))))
       (funcall f))))


;; wrap CMPFUN for use in a subtree
(if (trie-lexical-binding-p)
    (defun trie--wrap-cmpfun (cmpfun)
      (lambda (a b)
	(setq a (trie--node-split a)
	      b (trie--node-split b))
	(cond ((eq a trie--terminator)
	       (if (eq b trie--terminator) nil t))
	      ((eq b trie--terminator) nil)
	      (t (funcall cmpfun a b)))))
  (defun trie--wrap-cmpfun (cmpfun)
    `(lambda (a b)
       (setq a (trie--node-split a)
	     b (trie--node-split b))
       (cond ((eq a trie--terminator)
	      (if (eq b trie--terminator) nil t))
	     ((eq b trie--terminator) nil)
	     (t (,cmpfun a b))))))


;; create equality function from trie comparison function
(if (trie-lexical-binding-p)
    (defun trie--construct-equality-function (comparison-function)
      (lambda (a b)
	 (and (not (funcall comparison-function a b))
	      (not (funcall comparison-function b a)))))
  (defun trie--construct-equality-function (comparison-function)
    `(lambda (a b)
       (and (not (,comparison-function a b))
	    (not (,comparison-function b a))))))



;;; ----------------------------------------------------------------
;;;          Functions and macros for handling a trie node.

(defstruct
  (trie--node
   (:type vector)
   (:constructor nil)
   (:constructor trie--node-create
		 (split seq trie
		  &aux (subtree (funcall (trie--createfun trie)
					 (trie--cmpfun trie) seq))))
   (:constructor trie--node-create-data
		 (data &aux (split trie--terminator) (subtree data)))
   (:constructor trie--node-create-dummy
		 (split &aux (subtree nil)))
   (:constructor trie--node-create-root
		 (createfun cmpfun
		  &aux
		  (split nil)
		  (subtree (funcall createfun cmpfun []))))
   (:copier nil))
   split subtree)

;; data is stored in the subtree cell of a terminal node
(defalias 'trie--node-data 'trie--node-subtree)

(defsetf trie--node-data (node) (data)
  `(setf (trie--node-subtree ,node) ,data))

(defmacro trie--node-data-p (node)
  ;; Return t if NODE is a data node, nil otherwise.
  `(eq (trie--node-split ,node) trie--terminator))

(defmacro trie--node-p (node)
  ;; Return t if NODE is a TRIE trie--node, nil otherwise.  Have to
  ;; define this ourselves, because we created a defstruct without any
  ;; identifying tags (i.e. (:type vector)) for efficiency, but this
  ;; means we can only perform a rudimentary and very unreliable test.
  `(and (vectorp ,node) (= (length ,node) 2)))


(defun trie--node-find (node seq lookupfun)
  ;; Returns the node below NODE corresponding to SEQ, or nil if none
  ;; found.
  (let ((len (length seq))
	(i -1))
    ;; descend trie until we find SEQ or run out of trie
    (while (and node (< (incf i) len))
      (setq node
	    (funcall lookupfun
		     (trie--node-subtree node)
		     (trie--node-create-dummy (elt seq i))
		     nil)))
    node))


(defmacro trie--find-data-node (node lookupfun)
  ;; Return data node from NODE's subtree, or nil if NODE has no data
  ;; node in its subtree.
  `(funcall ,lookupfun
	    (trie--node-subtree ,node)
	    (trie--node-create-dummy trie--terminator)
	    nil))


(defmacro trie--find-data (node lookupfun)
  ;; Return data associated with sequence corresponding to NODE, or nil
  ;; if sequence has no associated data.
  `(let ((node (trie--find-data-node ,node ,lookupfun)))
     (when node (trie--node-data node))))



;;; ----------------------------------------------------------------
;;;              print/read transformation functions

(defun trie-transform-for-print (trie)
  "Transform TRIE to print form."
  (when (trie--transform-for-print trie)
    (if (trie--print-form trie)
	(warn "Trie has already been transformed to print-form")
      (funcall (trie--transform-for-print trie) trie)
      (setf (trie--print-form trie) t))))


(defun trie-transform-from-read (trie)
  "Transform TRIE from print form."
  (when (trie--transform-from-read trie)
    (if (not (trie--print-form trie))
	(warn "Trie is not in print-form")
      (funcall (trie--transform-from-read trie) trie)
      (setf (trie--print-form trie) nil))))


(defmacro trie-transform-from-read-warn (trie)
  "Transform TRIE from print form, with warning."
  `(when (trie--print-form ,trie)
     (warn (concat "Attempt to operate on trie in print-form;\
 converting to normal form"))
     (trie-transform-from-read ,trie)))


(defun trie--avl-transform-for-print (trie)
  ;; transform avl-tree based TRIE to print form.
  (trie-mapc-internal
   (lambda (avl _seq) (setf (avl-tree--cmpfun avl) nil))
   trie))


(defun trie--avl-transform-from-read (trie)
  ;; transform avl-tree based TRIE from print form."
  (let ((--trie-avl-transform--cmpfun (trie--cmpfun trie)))
    (trie-mapc-internal
     (lambda (avl _seq)
       (setf (avl-tree--cmpfun avl) --trie-avl-transform--cmpfun))
     trie)))



;;; ----------------------------------------------------------------
;;;                Replacements for CL functions

;; copied from cl-extra.el
(defun trie--subseq (seq start &optional end)
  "Return the subsequence of SEQ from START to END.
If END is omitted, it defaults to the length of the sequence.
If START or END is negative, it counts from the end."
  (if (stringp seq) (substring seq start end)
    (let (len)
      (and end (< end 0) (setq end (+ end (setq len (length seq)))))
      (when (< start 0)
	(setq start (+ start (or len (setq len (length seq))))))
      (cond ((listp seq)
	     (if (> start 0) (setq seq (nthcdr start seq)))
	     (if end
		 (let ((res nil))
		   (while (>= (setq end (1- end)) start)
		     (push (pop seq) res))
		   (nreverse res))
	       (copy-sequence seq)))
	    (t
	     (or end (setq end (or len (length seq))))
	     (let ((res (make-vector (max (- end start) 0) nil))
		   (i 0))
	       (while (< start end)
		 (aset res i (aref seq start))
		 (setq i (1+ i) start (1+ start)))
	       res))))))


(defun trie--position (item list)
  "Find the first occurrence of ITEM in LIST.
Return the index of the matching item, or nil of not found.
Comparison is done with 'equal."
  (let ((i 0))
    (catch 'found
      (while (progn
	       (when (equal item (car list)) (throw 'found i))
	       (setq i (1+ i))
	       (setq list (cdr list))))
      nil)))


(defsubst trie--seq-append (seq el)
  "Append EL to the end of sequence SEQ."
  (cond
   ((stringp seq) (concat seq (string el)))
   ((vectorp seq) (vconcat seq (vector el)))
   ((listp seq)	  (append seq (list el)))))


(defsubst trie--seq-concat (seq &rest sequences)
  "Concatenate SEQ and SEQUENCES, and make the result the same
type of sequence as SEQ."
  (cond
   ((stringp seq) (apply 'concat  seq sequences))
   ((vectorp seq) (apply 'vconcat seq sequences))
   ((listp seq)	  (apply 'append  seq sequences))))




;;; ================================================================
;;;                     Basic trie operations

;;;###autoload
(defalias 'make-trie 'trie--create
  "Return a new trie that uses comparison function COMPARISON-FUNCTION.

A trie stores sequences (strings, vectors or lists) along with
associated data. COMPARISON-FUNCTEION should accept two
arguments, each being an element of such a sequence, and return t
if the first is strictly smaller than the second.

The optional argument TYPE specifies the type of trie to
create. However, the only one that is currently implemented is
the default, so this argument is useless for now.

\(See also `make-trie-custom'.\)")


;;;###autoload
(defalias 'trie-create 'make-trie)


;;;###autoload
(defalias 'make-trie-custom 'trie--create-custom
  "Return a new trie that uses comparison function COMPARISON-FUNCTION.

A trie stores sequences (strings, vectors or lists) along with
associated data. COMPARISON-FUNCTION should accept two arguments,
each being an element of such a sequence, and return t if the
first is strictly smaller than the second.

The remaining keyword arguments: :CREATEFUN, :INSERTFUN, :DELETEFUN,
:LOOKUPFUN, :MAPFUN, :EMPTYFUN, :STACK-CREATEFUN, :STACK-POPFUN,
:STACK-EMPTYFUN, :TRANSFORM-FOR-PRINT and :TRANSFORM-FROM-READ
determine the type of trie that is created.

CREATEFUN is called as follows:

  (CREATEFUN COMPARISON-FUNCTION SEQ)

and should return a data structure (\"ARRAY\") that can be used
as an associative array, where two elements A and B are equal if
the following is non-nil:

  (and (COMPARISON-FUNCTION b a)
       (COMPARISON-FUNCTION b a))

The SEQ argument is a vector containing the sequence that will
correspond to the newly created array in the trie. For most types
of trie, this value is ignored. It is passed to CREATEFUN only in
order to allow the creation of \"hybrid\" trie structures, in
which different types of associative array are used in different
parts of the trie. For example, the type of associative array
could be chosen based on the depth in the trie, given by \(length
SEQ\). (Note that all the other functions described below must be
able to correctly handle *any* of the types of associate array
that might be created by CREATEFUN.)

INSERTFUN, DELETEFUN, LOOKUPFUN, MAPFUN and EMPTYFUN should
insert, delete, lookup, map over, and check-if-there-exist-any
elements in an associative array. They are called as follows:

  (INSERTFUN array element &optional updatefun)
  (DELETEFUN array element &optional predicate nilflag)
  (LOOKUPFUN array element &optional nilflag)
  (MAPFUN function array &optional reverse)
  (EMPTYFUN array)

INSERTFUN should insert ELEMENT into ARRAY and return the new
element, which will be ELEMENT itself unless UPDATEFUN is
specified. In that case, if and only if an element matching
ELEMENT already exists in the associative array, INSERTFUN should
instead pass ELEMENT and the matching element as arguments to
UPDATEFUN, replace the matching element with the return value,
and return that return value.

DELETEFUN should delete the element in the associative array that
matches ELEMENT, and return the deleted element. However, if
PREDICATE is specified and a matching element exists in ARRAY,
DELETEFUN should first pass the matching element as an argument
to PREDICATE before deleting, and should only delete the element
if PREDICATE returns non-nil. DELETEFUN should return NILFLAG if
no element was deleted (either becuase no matching element was
found, or because TESTFUN returned nil).

LOOKUPFUN should return the element from the associative array
that matches ELEMENT, or NILFLAG if no matching element exists.

MAPFUN should map FUNCTION over all elements in the order defined by
COMPARISON-FUNCTION, or in reverse order if REVERSE is non-nil.


STACK-CREATEFUN, STACK-POPFUN and STACK-EMPTYFUN should allow the
associative array to be used as a stack. STACK-CREATEFUN is
called as follows:

  (STACK-CREATEFUN array)

and should return a data structure (\"STACK\") that behaves like
a sorted stack of all elements in the associative array. I.e.
successive calls to

  (STACK-POPFUN stack)

should return elements from the associative array in the order
defined by COMPARISON-FUNCTION, and

  (STACK-EMPTYFUN stack)

should return non-nil if the stack is empty, nil otherwise.

The stack functions are optional, in that all trie operations
other than the stack-related ones will work correctly. However,
any code that makes use of trie-stacks will complain if supplied
with this type of trie.


The :TRANSFORM-FOR-PRINT and :TRANSFORM-FROM-READ arguments are
optional. If supplied, they can be used to transform the trie
into a format suitable for passing to Elisp's `print'
functions (typically used to persistently store the trie by
writing it to file), and transform from that format back to the
original usable form.


Warning: to avoid nasty dynamic scoping bugs, the supplied
functions must *never* bind any variables with names commencing
\"--\".")


;;;###autoload
(defalias 'trie-create-custom 'make-trie-custom)



(defalias 'trie-comparison-function 'trie--comparison-function
  "Return the comparison function for TRIE.")


(defalias 'trie-p 'trie--p
  "Return t if argument is a trie, nil otherwise.")


(defun trie-empty (trie)
  "Return t if the TRIE is empty, nil otherwise."
  (trie-transform-from-read-warn trie)
  (funcall (trie--emptyfun trie)
	   (trie--node-subtree (trie--root trie))))


(defun trie-construct-sortfun (cmpfun &optional reverse)
  "Construct function to compare key sequences, based on a CMPFUN
that compares individual elements of the sequence. Order is
reversed if REVERSE is non-nil."
  (if reverse
      `(lambda (a b)
	 (let (cmp)
	   (catch 'compared
	     (dotimes (i (min (length a) (length b)))
	       (cond ((,cmpfun (elt b i) (elt a i))
		      (throw 'compared t))
		     ((,cmpfun (elt a i) (elt b i))
		      (throw 'compared nil))))
	     (< (length a) (length b)))))
    `(lambda (a b)
       (let (cmp)
	 (catch 'compared
	   (dotimes (i (min (length a) (length b)))
	     (cond ((,cmpfun (elt a i) (elt b i))
		    (throw 'compared t))
		   ((,cmpfun (elt b i) (elt a i))
		    (throw 'compared nil))))
	   (< (length a) (length b)))))))



;; ----------------------------------------------------------------
;;                        Inserting data

(defun trie-insert (trie key &optional data updatefun)
  "Associate DATA with KEY in TRIE.

If KEY already exists in TRIE, then DATA replaces the existing
association, unless UPDATEFUN is supplied. Note that if DATA is
*not* supplied, this means that the existing association of KEY
will be replaced by nil.

If UPDATEFUN is supplied and KEY already exists in TRIE,
UPDATEFUN is called with two arguments: DATA and the existing
association of KEY. Its return value becomes the new association
for KEY.

Returns the new association of KEY.

Note: to avoid nasty dynamic scoping bugs, UPDATEFUN must *not*
bind any variables with names commencing \"--\"."

  ;; convert trie from print-form if necessary
  (trie-transform-from-read-warn trie)

  ;; absurd variable names are an attempt to avoid dynamic scoping bugs
  (let ((--trie-insert--updatefun updatefun)
	--trie-insert--old-node-flag
	(node (trie--root trie))
	(len (length key))
	(i -1))
    ;; Descend trie, adding nodes for non-existent elements of KEY. The
    ;; update function passed to `trie--insertfun' ensures that existing
    ;; nodes are left intact.
    (while (< (incf i) len)
      (setq --trie-insert--old-node-flag nil)
      (setq node (funcall (trie--insertfun trie)
			  (trie--node-subtree node)
			  (trie--node-create (elt key i) key trie)
			  (lambda (_a b)
			    (setq --trie-insert--old-node-flag t) b))))
    ;; Create or update data node.
    (setq node (funcall (trie--insertfun trie)
			(trie--node-subtree node)
			(trie--node-create-data data)
			;; if using existing data node, wrap UPDATEFUN
			;; if any was supplied
			(when (and --trie-insert--old-node-flag
				   --trie-insert--updatefun)
			  (lambda (new old)
			    (setf (trie--node-data old)
				  (funcall --trie-insert--updatefun
					   (trie--node-data new)
					   (trie--node-data old)))
			    old))))
    (trie--node-data node)))  ; return new data



;; ----------------------------------------------------------------
;;                        Deleting data

(defun trie-delete (trie key &optional test)
  "Delete KEY and its associated data from TRIE.

If KEY was deleted, a cons cell containing KEY and its
association is returned. Returns nil if KEY does not exist in
TRIE.

If TEST is supplied, it should be a function that accepts two
arguments: the key being deleted, and its associated data. The
key will then only be deleted if TEST returns non-nil.

Note: to avoid nasty dynamic scoping bugs, TEST must *not* bind
any variables with names commencing \"--\"."
  ;; convert trie from print-form if necessary
  (trie-transform-from-read-warn trie)
  ;; set up deletion (real work is done by `trie--do-delete'
  (let (--trie-deleted--node
	(--trie-delete--key key))
    (declare (special --trie-deleted--node)
	     (special --trie-delete--key))
    (trie--do-delete (trie--root trie) key test
		     (trie--deletefun trie)
		     (trie--emptyfun trie)
		     (trie--cmpfun trie))
    (when --trie-deleted--node
      (cons key (trie--node-data --trie-deleted--node)))))


(defun trie--do-delete (node --trie--do-delete--seq
			     --trie--do-delete--test
			     --trie--do-delete--deletefun
			     --trie--do-delete--emptyfun
			     --trie--do-delete--cmpfun)
  ;; Delete --TRIE--DO-DELETE--SEQ starting from trie node NODE, and
  ;; return non-nil if we deleted a node. If --TRIE--DO-DELETE--TEST is
  ;; supplied, it is called with two arguments, the key being deleted
  ;; and the associated data, and the deletion is only carried out if it
  ;; returns non-nil.

  ;; The absurd argument names are to lessen the likelihood of dynamical
  ;; scoping bugs caused by a supplied function binding a variable with
  ;; the same name as one of the arguments, which would cause a nasty
  ;; bug when the lambda's (below) are called.
  (declare (special --trie-deleted--node)
	   (special --trie-delete--key))
  ;; if --TRIE--DO-DELETE--SEQ is empty, try to delete data node and
  ;; return non-nil if we did (return value of
  ;; --TRIE--DO-DELETE--DELETEFUN is the deleted data, which is always
  ;; non-nil for a trie)
  (if (= (length --trie--do-delete--seq) 0)
      (setq --trie-deleted--node
	    (funcall --trie--do-delete--deletefun
		     (trie--node-subtree node)
		     (trie--node-create-dummy trie--terminator)
		     (when --trie--do-delete--test
		       (lambda (n)
			 (funcall --trie--do-delete--test
				  --trie-delete--key (trie--node-data n))))
		     nil))
    ;; otherwise, delete on down (return value of
    ;; --TRIE--DO-DELETE--DELETEFUN is the deleted data, which is always
    ;; non-nil for a trie)
    (funcall --trie--do-delete--deletefun
	     (trie--node-subtree node)
	     (trie--node-create-dummy (elt --trie--do-delete--seq 0))
	     (lambda (n)
	       (and (trie--do-delete
		     n (trie--subseq --trie--do-delete--seq 1)
		     --trie--do-delete--test
		     --trie--do-delete--deletefun
		     --trie--do-delete--emptyfun
		     --trie--do-delete--cmpfun)
		    (funcall --trie--do-delete--emptyfun
			     (trie--node-subtree n))))
	     nil)))



;; ----------------------------------------------------------------
;;                       Retrieving data

(defun trie-lookup (trie key &optional nilflag)
  "Return the data associated with KEY in the TRIE,
or nil if KEY does not exist in TRIE.

Optional argument NILFLAG specifies a value to return instead of
nil if KEY does not exist in TRIE. This allows a non-existent KEY
to be distinguished from an element with a null association. (See
also `trie-member-p', which does this for you.)"
  ;; convert trie from print-form if necessary
  (trie-transform-from-read-warn trie)
  ;; find node corresponding to key, then find data node, then return
  ;; data
  (let (node)
    (or (and (setq node (trie--node-find (trie--root trie) key
					 (trie--lookupfun trie)))
	     (trie--find-data node (trie--lookupfun trie)))
	nilflag)))

(defalias 'trie-member 'trie-lookup)


(defun trie-member-p (trie key)
  "Return t if KEY exists in TRIE, nil otherwise."
  ;; convert trie from print-form if necessary
  (trie-transform-from-read-warn trie)
  (let ((flag '(nil)))
    (not (eq flag (trie-member trie key flag)))))




;;; ================================================================
;;;                      Mapping over tries

(defun trie--mapc (--trie--mapc--function --trie--mapc--mapfun
		   --trie--mapc--root --trie--mapc--seq
		   &optional --trie--mapc--reverse)
  ;; Apply TRIE--MAPC--FUNCTION to all elements in a trie beneath
  ;; TRIE--MAPC--ROOT, which should correspond to the sequence
  ;; TRIE--MAPC--SEQ. TRIE--MAPC--FUNCTION is passed two arguments: the
  ;; trie node itself and the sequence it corresponds to. It is applied
  ;; in ascending order, or descending order if TRIE--MAPC--REVERSE is
  ;; non-nil.

  ;; The absurd argument names are to lessen the likelihood of dynamical
  ;; scoping bugs caused by a supplied function binding a variable with
  ;; the same name as one of the arguments.
  (funcall
   --trie--mapc--mapfun
   (lambda (--trie--mapc--node)
     ;; data node: apply function
     (if (trie--node-data-p --trie--mapc--node)
	 (funcall --trie--mapc--function
		  --trie--mapc--node
		  --trie--mapc--seq)
       ;; internal node: append split value to seq and keep descending
       (trie--mapc --trie--mapc--function
		   --trie--mapc--mapfun
		   --trie--mapc--node
		   (trie--seq-append
		    (copy-sequence --trie--mapc--seq)
		    (trie--node-split --trie--mapc--node))
		   --trie--mapc--reverse)))
   ;; --TRIE--MAPC--MAPFUN target
   (trie--node-subtree --trie--mapc--root)
   --trie--mapc--reverse))


(defun trie-mapc-internal (function trie &optional type)
  "Apply FUNCTION to all internal associative arrays within TRIE.
FUNCTION is passed two arguments: an associative array, and the
sequence it corresponds to.

Optional argument TYPE (one of the symbols vector, lisp or
string) sets the type of sequence passed to FUNCTION. Defaults to
vector."
  (trie--mapc-internal function (trie--mapfun trie) (trie--root trie)
		       (cond ((eq type 'string) "")
			     ((eq type 'lisp) ())
			     (t []))))


(defun trie--mapc-internal (--trie--mapc-internal--function
			     --trie--mapc-internal--mapfun
			     --trie--mapc-internal--root
			     --trie--mapc-internal--seq)
  (funcall
   --trie--mapc-internal--mapfun
   (lambda (--trie--mapc-internal--node)
     ;; data node
     (unless (trie--node-data-p --trie--mapc-internal--node)
       (funcall --trie--mapc-internal--function
		(trie--node-subtree --trie--mapc-internal--node)
		--trie--mapc-internal--seq)
       (trie--mapc-internal
	--trie--mapc-internal--function
	--trie--mapc-internal--mapfun
	--trie--mapc-internal--node
	(trie--seq-append
	 (copy-sequence --trie--mapc-internal--seq)
	 (trie--node-split --trie--mapc-internal--node)))))
   (trie--node-subtree --trie--mapc-internal--root)))


(defun trie-map (function trie &optional type reverse)
  "Modify all elements in TRIE by applying FUNCTION to them.

FUNCTION should take two arguments: a sequence stored in the trie
and its associated data. Its return value replaces the existing
data.

Optional argument TYPE (one of the symbols vector, lisp or
string) sets the type of sequence passed to FUNCTION. Defaults to
vector.

FUNCTION is applied in ascending order, or descending order if
REVERSE is non-nil.

Note: to avoid nasty dynamic scoping bugs, FUNCTION must *not*
bind any variables with names commencing \"--\"."
  ;; convert from print-form if necessary
  (trie-transform-from-read-warn trie)
  ;; map FUNCTION over TRIE
  (let ((--trie-map--function function)) ; avoid dynamic scoping bugs
    (trie--mapc
     (lambda (node seq)
       (setf (trie--node-data node)
	     (funcall --trie-map--function seq (trie--node-data node))))
     (trie--mapfun trie)
     (trie--root trie)
     (cond ((eq type 'string) "") ((eq type 'lisp) ()) (t []))
     reverse)))


(defun trie-mapc (function trie &optional type reverse)
  "Apply FUNCTION to all elements in TRIE for side effect only.

FUNCTION should take two arguments: a sequence stored in the trie
and its associated data.

Optional argument TYPE (one of the symbols vector, lisp or
string) sets the type of sequence passed to FUNCTION. Defaults to
vector.

FUNCTION is applied in ascending order, or descending order if
REVERSE is non-nil.

Note: to avoid nasty dynamic scoping bugs, FUNCTION must *not*
bind any variables with names commencing \"--\"."
  ;; convert from print-form if necessary
  (trie-transform-from-read-warn trie)
  ;; map FUNCTION over TRIE
  (let ((--trie-mapc--function function)) ; avoid dynamic scoping bugs
    (trie--mapc
     (lambda (node seq)
       (funcall --trie-mapc--function seq (trie--node-data node)))
     (trie--mapfun trie)
     (trie--root trie)
     (cond ((eq type 'string) "") ((eq type 'lisp) ()) (t []))
     reverse)))


(defun trie-mapf (function combinator trie &optional type reverse)
  "Apply FUNCTION to all elements in TRIE, and combine the results
using COMBINATOR.

FUNCTION should take two arguments: a sequence stored in the
trie, and its associated data.

Optional argument TYPE (one of the symbols vector, lisp or
string; defaults to vector) sets the type of sequence passed to
FUNCTION. If TYPE is 'string, it must be possible to apply the
function `string' to the individual elements of key sequences
stored in TRIE.

The FUNCTION is applied and the results combined in ascending
order, or descending order if REVERSE is non-nil.

Note: to avoid nasty dynamic scoping bugs, FUNCTION and
COMBINATOR must *not* bind any variables with names
commencing \"--\"."
  ;; convert from print-form if necessary
  (trie-transform-from-read-warn trie)
  ;; map FUNCTION over TRIE, combining results with COMBINATOR
  (let ((--trie-mapf--function function) ; avoid dynamic scoping bugs
	--trie-mapf--accumulate)
    (trie--mapc
     (lambda (node seq)
       (setq --trie-mapf--accumulate
	     (funcall combinator
		      (funcall --trie-mapf--function
			       seq (trie--node-data node))
		      --trie-mapf--accumulate)))
     (trie--mapfun trie)
     (trie--root trie)
     (cond ((eq type 'string) "") ((eq type 'lisp) ()) (t []))
     reverse)
    --trie-mapf--accumulate))


(defun trie-mapcar (function trie &optional type reverse)
  "Apply FUNCTION to all elements in TRIE,
and make a list of the results.

FUNCTION should take two arguments: a sequence stored in the trie
and its associated data.

Optional argument TYPE (one of the symbols vector, lisp or
string) sets the type of sequence passed to FUNCTION. Defaults to
vector.

The FUNCTION is applied and the list constructed in ascending
order, or descending order if REVERSE is non-nil.

Note that if you don't care about the order in which FUNCTION is
applied, just that the resulting list is in the correct order,
then

  (trie-mapf function 'cons trie type (not reverse))

is more efficient.

Note: to avoid nasty dynamic scoping bugs, FUNCTION must *not*
bind any variables with names commencing \"--\"."
  ;; convert from print-form if necessary
  (trie-transform-from-read-warn trie)
  ;; map FUNCTION over TRIE and accumulate in a list
  (nreverse (trie-mapf function 'cons trie type reverse)))




;;; ================================================================
;;;                    Using tries as stacks

(defstruct (trie--stack
	    (:constructor nil)
	    (:constructor
	     trie--stack-create
	     (trie
	      &optional
	      (type 'vector)
	      reverse
	      &aux
	      (comparison-function (trie--comparison-function trie))
	      (lookupfun (trie--lookupfun trie))
	      (stack-createfun (trie--stack-createfun trie))
	      (stack-popfun (trie--stack-popfun trie))
	      (stack-emptyfun (trie--stack-emptyfun trie))
	      (repopulatefun 'trie--stack-repopulate)
	      (store
	       (if (trie-empty trie)
		   nil
		 (trie--stack-repopulate
		  (list (cons
			 (cond ((eq type 'list) ())
			       ((eq type 'string) "")
			       (t []))
			 (funcall
			  stack-createfun
			  (trie--node-subtree (trie--root trie))
			  reverse)))
		  reverse
		  comparison-function lookupfun
		  stack-createfun stack-popfun stack-emptyfun)))
	      (pushed '())
	      ))
	    (:constructor
	     trie--completion-stack-create
	     (trie prefix
	      &optional
	      reverse
	      &aux
	      (comparison-function (trie--comparison-function trie))
	      (lookupfun (trie--lookupfun trie))
	      (stack-createfun (trie--stack-createfun trie))
	      (stack-popfun (trie--stack-popfun trie))
	      (stack-emptyfun (trie--stack-emptyfun trie))
	      (repopulatefun 'trie--stack-repopulate)
	      (store (trie--completion-stack-construct-store
		      trie prefix reverse))
	      (pushed '())
	      ))
	    (:constructor
	     trie--regexp-stack-create
	     (trie regexp
	      &optional
	      reverse
	      &aux
	      (comparison-function (trie--comparison-function trie))
	      (lookupfun (trie--lookupfun trie))
	      (stack-createfun (trie--stack-createfun trie))
	      (stack-popfun (trie--stack-popfun trie))
	      (stack-emptyfun (trie--stack-emptyfun trie))
	      (repopulatefun 'trie--regexp-stack-repopulate)
	      (store (trie--regexp-stack-construct-store
		      trie regexp reverse))
	      (pushed '())
	      ))
	    (:copier nil))
  reverse comparison-function lookupfun
  stack-createfun stack-popfun stack-emptyfun
  repopulatefun store pushed)


(defun trie-stack (trie &optional type reverse)
  "Return an object that allows TRIE to be accessed as a stack.

The stack is sorted in \"lexical\" order, i.e. the order defined
by the trie's comparison function, or in reverse order if REVERSE
is non-nil. Calling `trie-stack-pop' pops the top element (a key
and its associated data) from the stack.

Optional argument TYPE (one of the symbols vector, lisp or
string) sets the type of sequence used for the keys.

Note that any modification to TRIE *immediately* invalidates all
trie-stacks created before the modification (in particular,
calling `trie-stack-pop' will give unpredictable results).

Operations on trie-stacks are significantly more efficient than
constructing a real stack from the trie and using standard stack
functions. As such, they can be useful in implementing efficient
algorithms on tries. However, in cases where mapping functions
`trie-mapc', `trie-mapcar' or `trie-mapf' would be sufficient, it
is better to use one of those instead."
  ;; convert trie from print-form if necessary
  (trie-transform-from-read-warn trie)
  ;; if stack functions aren't defined for trie type, throw error
  (if (not (functionp (trie--stack-createfun trie)))
      (error "Trie type does not support stack operations")
    ;; otherwise, create and initialise a stack
    (trie--stack-create trie type reverse)))


(defun trie-stack-pop (trie-stack &optional nilflag)
  "Pop the first element from TRIE-STACK.

Returns nil if the stack is empty, or NILFLAG if specified. (The
latter allows an empty stack to be distinguished from a null
element stored in the trie.)"
  ;; return nilflag if stack is empty
  (if (trie-stack-empty-p trie-stack)
      nilflag
    ;; if elements have been pushed onto the stack, pop those first
    (if (trie--stack-pushed trie-stack)
	(pop (trie--stack-pushed trie-stack))
      ;; otherwise, pop first element from trie-stack and repopulate it
      (prog1
	  (pop (trie--stack-store trie-stack))
	(setf (trie--stack-store trie-stack)
	      (funcall (trie--stack-repopulatefun trie-stack)
		       (trie--stack-store trie-stack)
		       (trie--stack-reverse trie-stack)
		       (trie--stack-comparison-function trie-stack)
		       (trie--stack-lookupfun trie-stack)
		       (trie--stack-stack-createfun trie-stack)
		       (trie--stack-stack-popfun trie-stack)
		       (trie--stack-stack-emptyfun trie-stack)))))))


(defun trie-stack-push (element trie-stack)
  "Push ELEMENT onto TRIE-STACK."
  (push element (trie--stack-pushed trie-stack)))


(defun trie-stack-first (trie-stack &optional nilflag)
  "Return the first element from TRIE-STACK, without removing it
from the stack.

Returns nil if the stack is empty, or NILFLAG if specified. (The
latter allows an empty stack to be distinguished from a null
element stored in the trie.)"
  ;; return nilflag if stack is empty
  (if (trie-stack-empty-p trie-stack)
      nilflag
    ;; if elements have been pushed onto the stack, return first of
    ;; those
    (if (trie--stack-pushed trie-stack)
	(car (trie--stack-pushed trie-stack))
      ;; otherwise, return first element from trie-stack
      (car (trie--stack-store trie-stack)))))


(defalias 'trie-stack-p 'trie--stack-p
  "Return t if argument is a trie-stack, nil otherwise.")


(defun trie-stack-empty-p (trie-stack)
  "Return t if TRIE-STACK is empty, nil otherwise."
  (and (null (trie--stack-store trie-stack))
       (null (trie--stack-pushed trie-stack))))


(defun trie--stack-repopulate
  (store reverse _comparison-function _lookupfun
	 stack-createfun stack-popfun stack-emptyfun)
  ;; Recursively push children of the node at the head of STORE onto the
  ;; front of STORE, until a data node is reached.

  ;; nothing to do if stack is empty
  (when store
    (let ((node (funcall stack-popfun (cdar store)))
	  (seq (caar store)))
      (when (funcall stack-emptyfun (cdar store))
	;; (pop store) here produces irritating compiler warnings
	(setq store (cdr store)))

      (while (not (trie--node-data-p node))
	(push
	 (cons (trie--seq-append seq (trie--node-split node))
	       (funcall stack-createfun
			(trie--node-subtree node) reverse))
	 store)
	(setq node (funcall stack-popfun (cdar store))
	      seq (caar store))
	(when (funcall stack-emptyfun (cdar store))
	  ;; (pop store) here produces irritating compiler warnings
	  (setq store (cdr store))))

      (push (cons seq (trie--node-data node)) store))))




;; ================================================================
;;                   Query-building utility macros

;; Implementation Note
;; -------------------
;; For queries ranked in anything other than lexical order, we use a
;; partial heap-sort to find the k=MAXNUM highest ranked matches among
;; the n possibile matches. This has worst-case time complexity
;; O(n log k), and is both simple and elegant. An optimal algorithm
;; (e.g. partial quick-sort discarding the irrelevant partition at each
;; step) would have complexity O(n + k log k), but is probably not worth
;; the extra coding effort, and would have worse space complexity unless
;; coded to work "in-place", which would be highly non-trivial. (I
;; haven't done any benchmarking, though, so feel free to do so and let
;; me know the results!)

(defmacro trie--construct-accumulator (maxnum filter resultfun)
  ;; Does what it says on the tin! | sed -e 's/tin/macro name/'
  `(cond
    ;; filter, maxnum, resultfun
    ((and ,filter ,maxnum ,resultfun)
     (lambda (seq data)
       (when (funcall ,filter seq data)
	 (aset trie--accumulate 0
	       (cons (funcall ,resultfun seq data)
		     (aref trie--accumulate 0)))
	 (and (>= (length (aref trie--accumulate 0)) ,maxnum)
	      (throw 'trie-accumulate--done nil)))))
    ;; filter, maxnum, !resultfun
    ((and ,filter ,maxnum (not ,resultfun))
     (lambda (seq data)
       (when (funcall ,filter seq data)
	 (aset trie--accumulate 0
	       (cons (cons seq data)
		     (aref trie--accumulate 0)))
	 (and (>= (length (aref trie--accumulate 0)) ,maxnum)
	      (throw 'trie-accumulate--done nil)))))
    ;; filter, !maxnum, resultfun
    ((and ,filter (not ,maxnum) ,resultfun)
     (lambda (seq data)
       (when (funcall ,filter seq data)
	 (aset trie--accumulate 0
	       (cons (funcall ,resultfun seq data)
		     (aref trie--accumulate 0))))))
    ;; filter, !maxnum, !resultfun
    ((and ,filter (not ,maxnum) (not ,resultfun))
     (lambda (seq data)
       (when (funcall ,filter seq data)
	 (aset trie--accumulate 0
	       (cons (cons seq data)
		     (aref trie--accumulate 0))))))
    ;; !filter, maxnum, resultfun
    ((and (not ,filter) ,maxnum ,resultfun)
     (lambda (seq data)
       (aset trie--accumulate 0
	     (cons (funcall ,resultfun seq data)
		   (aref trie--accumulate 0)))
       (and (>= (length (aref trie--accumulate 0)) ,maxnum)
	    (throw 'trie-accumulate--done nil))))
    ;; !filter, maxnum, !resultfun
    ((and (not ,filter) ,maxnum (not ,resultfun))
     (lambda (seq data)
       (aset trie--accumulate 0
	     (cons (cons seq data)
		   (aref trie--accumulate 0)))
       (and (>= (length (aref trie--accumulate 0)) ,maxnum)
	    (throw 'trie-accumulate--done nil))))
    ;; !filter, !maxnum, resultfun
    ((and (not ,filter) (not ,maxnum) ,resultfun)
     (lambda (seq data)
       (aset trie--accumulate 0
	     (cons (funcall ,resultfun seq data)
		   (aref trie--accumulate 0)))))
    ;; !filter, !maxnum, !resultfun
    ((and (not ,filter) (not ,maxnum) (not ,resultfun))
     (lambda (seq data)
       (aset trie--accumulate 0
	     (cons (cons seq data)
		   (aref trie--accumulate 0)))))
    ))



(defmacro trie--construct-ranked-accumulator (maxnum filter)
  ;; Does what it says on the tin! | sed -e 's/tin/macro name/'
  `(cond
    ;; filter, maxnum
    ((and ,filter ,maxnum)
     (lambda (seq data)
       (when (funcall ,filter seq data)
	 (heap-add trie--accumulate (cons seq data))
	 (and (> (heap-size trie--accumulate) ,maxnum)
	      (heap-delete-root trie--accumulate)))))
    ;; filter, !maxnum
    ((and ,filter (not ,maxnum))
     (lambda (seq data)
       (when (funcall ,filter seq data)
	 (heap-add trie--accumulate (cons seq data)))))
    ;; !filter, maxnum
    ((and (not ,filter) ,maxnum)
     (lambda (seq data)
       (heap-add trie--accumulate (cons seq data))
       (and (> (heap-size trie--accumulate) ,maxnum)
	    (heap-delete-root trie--accumulate))))
    ;; !filter, !maxnum
    ((and (not ,filter) (not ,maxnum))
     (lambda (seq data)
       (heap-add trie--accumulate (cons seq data))))))



(defmacro trie--accumulate-results
  (rankfun maxnum reverse filter resultfun accfun duplicates &rest body)
  ;; Accumulate results of running BODY code, and return them in
  ;; appropriate order. BODY should call ACCFUN to accumulate a result,
  ;; passing it two arguments: a trie data node, and the corresponding
  ;; sequence. BODY can throw 'trie-accumulate--done to terminate the
  ;; accumulation and return the results. A non-null DUPLICATES flag
  ;; signals that the accumulated results might contain duplicates,
  ;; which should be deleted. Note that DUPLICATES is ignored if RANKFUN
  ;; is null. The other arguments should be passed straight through from
  ;; the query function.

  ;; rename functions to help avoid dynamic-scoping bugs
  `(let* ((--trie-accumulate--rankfun ,rankfun)
	  (--trie-accumulate--filter ,filter)
	  (--trie-accumulate--resultfun ,resultfun)
	  ;; construct structure in which to accumulate results
	  (trie--accumulate
	   (if ,rankfun
	       (heap-create  ; heap order is inverse of rank order
		(if ,reverse
		    (lambda (a b)
		      (funcall --trie-accumulate--rankfun a b))
		  (lambda (a b)
		    (not (funcall --trie-accumulate--rankfun a b))))
		(when ,maxnum (1+ ,maxnum)))
	     (make-vector 1 nil)))
	  ;; construct function to accumulate completions
	  (,accfun
	   (if ,rankfun
	       (trie--construct-ranked-accumulator
		,maxnum --trie-accumulate--filter)
	     (trie--construct-accumulator
	      ,maxnum --trie-accumulate--filter
	      --trie-accumulate--resultfun))))

     ;; accumulate results
     (catch 'trie-accumulate--done ,@body)

     ;; return list of completions
     (cond
      ;; for a ranked query, extract completions from heap
      (,rankfun
       (let (completions)
	 ;; check for and delete duplicates if flag is set
	 (if ,duplicates
	     (while (not (heap-empty trie--accumulate))
	       (if (equal (car (heap-root trie--accumulate))
			  (caar completions))
		   (heap-delete-root trie--accumulate)
		 (push (heap-delete-root trie--accumulate)
		       completions)))
	   ;; skip duplicate checking if flag is not set
	   (while (not (heap-empty trie--accumulate))
	     (if ,resultfun
		 (let ((res (heap-delete-root trie--accumulate)))
		   (push (funcall ,resultfun (car res) (cdr res))
			 completions))
	       (push (heap-delete-root trie--accumulate)
		     completions))))
	 completions))

      ;; for lexical query, reverse result list if MAXNUM supplied
      (,maxnum (nreverse (aref trie--accumulate 0)))
      ;; otherwise, just return list
      (t (aref trie--accumulate 0)))))




;; ================================================================
;;                          Completing

(defun trie-complete
  (trie prefix &optional rankfun maxnum reverse filter resultfun)
  "Return an alist containing all completions of PREFIX in TRIE
along with their associated data, in the order defined by
RANKFUN, defaulting to \"lexical\" order (i.e. the order defined
by the trie's comparison function). If REVERSE is non-nil, the
completions are sorted in the reverse order. Returns nil if no
completions are found.

PREFIX must be a sequence (vector, list or string) containing
elements of the type used to reference data in the trie. (If
PREFIX is a string, it must be possible to apply `string' to
individual elements of the sequences stored in the trie.) The
completions returned in the alist will be sequences of the same
type as KEY. If PREFIX is a list of sequences, completions of all
sequences in the list are included in the returned alist. All
sequences in the list must be of the same type.

The optional integer argument MAXNUM limits the results to the
first MAXNUM completions. Otherwise, all completions are
returned.

If specified, RANKFUN must accept two arguments, both cons
cells. The car contains a sequence from the trie (of the same
type as PREFIX), the cdr contains its associated data. It should
return non-nil if first argument is ranked strictly higher than
the second, nil otherwise.

The FILTER argument sets a filter function for the
completions. If supplied, it is called for each possible
completion with two arguments: the completion, and its associated
data. If the filter function returns nil, the completion is not
included in the results, and does not count towards MAXNUM.

RESULTFUN defines a function used to process results before
adding them to the final result list. If specified, it should
accept two arguments: a key and its associated data. It's return
value is what gets added to the final result list, instead of the
default key-data cons cell."

  ;; convert trie from print-form if necessary
  (trie-transform-from-read-warn trie)
  ;; wrap prefix in a list if necessary
  ;; FIXME: the test for a list of prefixes, below, will fail if the
  ;;        PREFIX sequence is a list, and the elements of PREFIX are
  ;;        themselves lists (there might be no easy way to fully fix
  ;;        this...)
  (if (or (atom prefix)
	  (and (listp prefix) (not (sequencep (car prefix)))))
      (setq prefix (list prefix))
    ;; sort list of prefixes if sorting completions lexically
    (when (null rankfun)
      (setq prefix
	    (sort prefix (trie-construct-sortfun
			  (trie--comparison-function trie))))))

  ;; accumulate completions
  (let (node)
    (declare (special accumulator))
    (trie--accumulate-results
     rankfun maxnum reverse filter resultfun accumulator nil
     (mapc (lambda (pfx)
	     (setq node (trie--node-find (trie--root trie) pfx
					 (trie--lookupfun trie)))
	     (when node
	       (trie--mapc
		(lambda (node seq)
		  (funcall accumulator seq (trie--node-data node)))
		(trie--mapfun trie) node pfx
		(if maxnum reverse (not reverse)))))
	   prefix))
    ))



(defun trie-complete-stack (trie prefix &optional reverse)
  "Return an object that allows completions of PREFIX to be accessed
as if they were a stack.

The stack is sorted in \"lexical\" order, i.e. the order defined
by TRIE's comparison function, or in reverse order if REVERSE is
non-nil. Calling `trie-stack-pop' pops the top element (a key and
its associated data) from the stack.

PREFIX must be a sequence (vector, list or string) that forms the
initial part of a TRIE key, or a list of such sequences. (If
PREFIX is a string, it must be possible to apply `string' to
individual elements of TRIE keys.)  The completions returned in
the alist will be sequences of the same type as KEY. If PREFIX is
a list of sequences, completions of all sequences in the list are
included in the stack. All sequences in the list must be of the
same type.

Note that any modification to TRIE *immediately* invalidates all
trie-stacks created before the modification (in particular,
calling `trie-stack-pop' will give unpredictable results).

Operations on trie-stacks are significantly more efficient than
constructing a real stack from completions of PREFIX in TRIE and
using standard stack functions. As such, they can be useful in
implementing efficient algorithms on tries. However, in cases
where `trie-complete' or `trie-complete-ordered' is sufficient,
it is better to use one of those instead."
  ;; convert trie from print-form if necessary
  (trie-transform-from-read-warn trie)
  ;; if stack functions aren't defined for trie type, throw error
  (if (not (functionp (trie--stack-createfun trie)))
      (error "Trie type does not support stack operations")
    ;; otherwise, create and initialise a stack
    (trie--completion-stack-create trie prefix reverse)))


(defun trie--completion-stack-construct-store (trie prefix reverse)
  ;; Construct store for completion stack based on TRIE.
  (let (store node)
    (if (or (atom prefix)
	    (and (listp prefix)
		 (not (sequencep (car prefix)))))
	(setq prefix (list prefix))
      (setq prefix
	    (sort prefix
		  (trie-construct-sortfun
		   (trie--comparison-function trie)
		   (not reverse)))))
    (dolist (pfx prefix)
      (when (setq node (trie--node-find (trie--root trie) pfx
					(trie--lookupfun trie)))
	(push (cons pfx (funcall (trie--stack-createfun trie)
				 (trie--node-subtree node)
				 reverse))
	      store)))
    (trie--stack-repopulate
     store reverse
     (trie--comparison-function trie)
     (trie--lookupfun trie)
     (trie--stack-createfun trie)
     (trie--stack-popfun trie)
     (trie--stack-emptyfun trie))))




;; ================================================================
;;                        Regexp search

(defun trie-regexp-search
  (trie regexp &optional rankfun maxnum reverse filter resultfun)
  "Return an alist containing all matches for REGEXP in TRIE
along with their associated data, in the order defined by
RANKFUN, defauling to \"lexical\" order (i.e. the order defined
by the trie's comparison function).  If REVERSE is non-nil, the
completions are sorted in the reverse order. Returns nil if no
completions are found.

REGEXP is a regular expression, but it need not necessarily be a
string. It must be a sequence (vector, list, or string) whose
elements are either elements of the same type as elements of the
trie keys (which behave as literals in the regexp), or a regexp
special character or backslash construct. If REGEXP is a string,
it must be possible to apply `string' to individual elements of
the keys stored in the trie. The matches returned in the alist
will be sequences of the same type as REGEXP.

Only a subset of the full Emacs regular expression syntax is
supported. There is no support for regexp constructs that are
only meaningful for strings (character ranges and character
classes inside character alternatives, and syntax-related
backslash constructs). Back-references and non-greedy postfix
operators are not supported, so `?' after a postfix operator
loses its special meaning. Also, matches are always anchored, so
`$' and `^' lose their special meanings (use `.*' at the
beginning and end of the regexp to get an unanchored match).

If the regexp contains any non-shy grouping constructs, subgroup
match data is included in the results. In this case, the car of
each match is no longer just a key. Instead, it is a list whose
first element is the matching key, and whose remaining elements
are cons cells whose cars and cdrs give the start and end indices
of the elements that matched the corresponding groups, in order.

The optional integer argument MAXNUM limits the results to the
first MAXNUM matches. Otherwise, all matches are returned.

If specified, RANKFUN must accept two arguments, both cons
cells. The car contains a sequence from the trie (of the same
type as REGEXP), the cdr contains its associated data. It should
return non-nil if first argument is ranked strictly higher than
the second, nil otherwise.

The FILTER argument sets a filter function for the matches. If
supplied, it is called for each possible match with two
arguments: the matching key, and its associated data. If the
filter function returns nil, the match is not included in the
results, and does not count towards MAXNUM.

RESULTFUN defines a function used to process results before
adding them to the final result list. If specified, it should
accept two arguments: a key and its associated data. It's return
value is what gets added to the final result list, instead of the
default key-data cons cell."

  ;; convert trie from print-form if necessary
  (trie-transform-from-read-warn trie)

  ;; massage rankfun to cope with grouping data
  ;; FIXME: could skip this if REGEXP contains no grouping constructs
  (when rankfun
    (setq rankfun
	  `(lambda (a b)
	     ;; if car of argument contains a key+group list rather than
	     ;; a straight key, remove group list
	     ;; FIXME: the test for straight key, below, will fail if
	     ;;        the key is a list, and the first element of the
	     ;;        key is itself a list (there might be no easy way
	     ;;        to fully fix this...)
	     (unless (or (atom (car a))
			 (and (listp (car a))
			      (not (sequencep (caar a)))))
	       (setq a (cons (caar a) (cdr a))))
	     (unless (or (atom (car b))
			 (and (listp (car b))
			      (not (sequencep (caar b)))))
	       (setq b (cons (caar b) (cdr b))))
	     ;; call rankfun on massaged arguments
	     (,rankfun a b))))

  ;; accumulate completions
  (declare (special accumulator))
  (trie--accumulate-results
   rankfun maxnum reverse filter resultfun accumulator nil
   (trie--do-regexp-search
    (trie--root trie)
    (tNFA-from-regexp regexp :test (trie--construct-equality-function
				    (trie--comparison-function trie)))
    (cond ((stringp regexp) "") ((listp regexp) ()) (t []))  0
    (or (and maxnum reverse) (and (not maxnum) (not reverse)))
    (trie--comparison-function trie)
    (trie--lookupfun trie)
    (trie--mapfun trie))))



(defun trie--do-regexp-search
  (--trie--regexp-search--node tNFA seq pos reverse
			       comparison-function lookupfun mapfun)
  ;; Search everything below the node --TRIE--REGEXP-SEARCH-NODE for
  ;; matches to the regexp encoded in tNFA. SEQ is the sequence
  ;; corresponding to NODE, POS is it's length. REVERSE is the usual
  ;; query argument, and the remaining arguments are the corresponding
  ;; trie functions.
  (declare (special accumulator))

  ;; if NFA has matched and we're accumulating in normal order, check if
  ;; trie contains current string
  (when (and (not reverse) (tNFA-match-p tNFA))
    (let (node groups)
      (when (setq node (trie--find-data-node
			--trie--regexp-search--node lookupfun))
	(setq groups (tNFA-group-data tNFA))
	(funcall accumulator
		 (if groups (cons seq groups) seq)
		 (trie--node-data node)))))

  (cond
   ;; ;; data node
   ;; ((trie--node-data-p --trie--regexp-search--node)
   ;;  (when (tNFA-match-p tNFA)
   ;;    (let ((groups (tNFA-group-data tNFA)))
   ;; 	(funcall accumulator
   ;; 		 (if groups (cons seq groups) seq)
   ;; 		 (trie--node-data --trie--regexp-search--node)))))

   ;; wildcard transition: map over all nodes in subtree
   ((tNFA-wildcard-p tNFA)
    (let (state)
      (funcall mapfun
	       (lambda (node)
		 (unless (trie--node-data-p node)
		     ;; (when (tNFA-match-p tNFA)
		     ;;   (setq groups (tNFA-group-data tNFA))
		     ;;   (funcall accumulator
		     ;; 		(if groups (cons seq groups) seq)
		     ;; 		(trie--node-data node)))
		   (when (setq state (tNFA-next-state
				      tNFA (trie--node-split node) pos))
		     (trie--do-regexp-search
		      node state
		      (trie--seq-append seq (trie--node-split node))
		      (1+ pos) reverse comparison-function
		      lookupfun mapfun))))
	       (trie--node-subtree --trie--regexp-search--node)
	       reverse)))

   (t ;; no wildcard transition: loop over all transitions
    (let (node state)
      (dolist (chr (sort (tNFA-transitions tNFA)
			 (if reverse
			     `(lambda (a b) (,comparison-function b a))
			   comparison-function)))
	(when (and (setq node (trie--node-find
			       --trie--regexp-search--node
			       (vector chr) lookupfun))
		   (setq state (tNFA-next-state tNFA chr pos)))
	  (trie--do-regexp-search
	   node state (trie--seq-append seq chr) (1+ pos)
	   reverse comparison-function lookupfun mapfun))))))

  ;; if NFA has matched and we're accumulating in reverse order, check if
  ;; trie contains current string
  (when (and reverse (tNFA-match-p tNFA))
    (let (node groups)
      (when (setq node (trie--find-data-node
			--trie--regexp-search--node lookupfun))
	(setq groups (tNFA-group-data tNFA))
	(funcall accumulator
		 (if groups (cons seq groups) seq)
		 (trie--node-data node))))))



(defun trie-regexp-stack  (trie regexp &optional reverse)
  "Return an object that allows matches to REGEXP to be accessed
as if they were a stack.

The stack is sorted in \"lexical\" order, i.e. the order defined
by TRIE's comparison function, or in reverse order if REVERSE is
non-nil. Calling `trie-stack-pop' pops the top element (a cons
cell containing a key and its associated data) from the stack.

REGEXP is a regular expression, but it need not necessarily be a
string. It must be a sequence (vector, list of string) whose
elements are either elements of the same type as elements of the
trie keys (which behave as literals in the regexp), or any of the
usual regexp special characters and backslash constructs. If
REGEXP is a string, it must be possible to apply `string' to
individual elements of the keys stored in the trie. The matches
returned in the alist will be sequences of the same type as KEY.

Back-references and non-greedy postfix operators are *not*
supported, and the matches are always anchored, so `$' and `^'
lose their special meanings.

If the regexp contains any non-shy grouping constructs, subgroup
match data is included in the results. In this case, the car of
each match (as returned by a call to `trie-stack-pop' is no
longer just a key. Instead, it is a list whose first element is
the matching key, and whose remaining elements are cons cells
whose cars and cdrs give the start and end indices of the
elements that matched the corresponding groups, in order."

  ;; convert trie from print-form if necessary
  (trie-transform-from-read-warn trie)
  ;; if stack functions aren't defined for trie type, throw error
  (if (not (functionp (trie--stack-createfun trie)))
      (error "Trie type does not support stack operations")
    ;; otherwise, create and initialise a regexp stack
    (trie--regexp-stack-create trie regexp reverse)))


(defun trie--regexp-stack-construct-store
  (trie regexp &optional reverse)
  ;; Construct store for regexp stack based on TRIE.
  (let ((seq (cond ((stringp regexp) "") ((listp regexp) ()) (t [])))
	store)
    (push (list seq (trie--root trie)
		(tNFA-from-regexp
		 regexp :test (trie--construct-equality-function
			       (trie--comparison-function trie)))
		0)
	  store)
    (trie--regexp-stack-repopulate
     store reverse
     (trie--comparison-function trie)
     (trie--lookupfun trie)
     (trie--stack-createfun trie)
     (trie--stack-popfun trie)
     (trie--stack-emptyfun trie))))


(defun trie--regexp-stack-repopulate
  (store reverse comparison-function lookupfun
	 stack-createfun stack-popfun stack-emptyfun)
  ;; Recursively push matching children of the node at the head of STORE
  ;; onto STORE, until a data node is reached. REVERSE is the usual
  ;; query argument, and the remaining arguments are the corresponding
  ;; trie functions.
  (let (state seq node pos groups n s)
    (while
	(progn
	  (setq pos (pop store)
		seq (nth 0 pos)
		node (nth 1 pos)
		state (nth 2 pos)
		pos (nth 3 pos))
	  (cond
	   ;; if stack is empty, we're done
	   ((null node) nil)

	   ;; if stack element is a trie node...
	   ((trie--node-p node)
	    (cond
	     ;; matching data node: add data to the stack and we're done
	     ((trie--node-data-p node)
	      (when (tNFA-match-p state)
		(setq groups (tNFA-group-data state))
		(push (cons (if groups (cons groups seq) seq)
			    (trie--node-data node))
		      store))
	      nil)  ; return nil to exit loop

	     ;; wildcard transition: add new node stack
	     ((tNFA-wildcard-p state)
	      (push (list seq
			  (funcall stack-createfun
				   (trie--node-subtree node) reverse)
			  state pos)
		    store))

	     (t ;; non-wildcard transition: add all possible next nodes
	      (dolist (chr (sort (tNFA-transitions state)
				 (if reverse
				     comparison-function
				   `(lambda (a b)
				      (,comparison-function b a)))))
		(when (and (setq n (trie--node-find
				    node (vector chr) lookupfun))
			   (setq s (tNFA-next-state state chr pos)))
		  (push (list (trie--seq-append seq chr) n s (1+ pos))
			store)))
	      t)))  ; return t to keep looping

	   ;; otherwise, stack element is a node stack...
	   (t
	    ;; if node stack is empty, dump it and keep repopulating
	    (if (funcall stack-emptyfun node)
		t  ; return t to keep looping
	      ;; otherwise, add node stack back, and add next node from
	      ;; stack
	      (push (list seq node state pos) store)
	      (setq node (funcall stack-popfun node)
		    state (tNFA-next-state state
					   (trie--node-split node) pos))
	      (when state
		;; matching data node: add data to the stack and we're
		;; done
		(if (trie--node-data-p node)
		    (progn
		      (push (cons seq (trie--node-data node)) store)
		      nil)  ; return nil to exit loop
		  ;; normal node: add it to the stack and keep
		  ;; repopulating
		  (push (list
			 (trie--seq-append seq (trie--node-split node))
			 node state (1+ pos))
			store)))))
	   ))))
  store)



;; ----------------------------------------------------------------
;;            Pretty-print tries during edebug

;; Note:
;; -----

;; We advise the `edebug-prin1' and `edebug-prin1-to-string' functions
;; (actually, aliases) so that they print "#<trie>" instead of the full
;; print form for tries.
;;
;; This is because, if left to its own devices, edebug hangs for ages
;; whilst printing large tries, and you either have to wait for a *very*
;; long time for it to finish, or kill Emacs entirely. (Even C-g C-g
;; fails!)
;;
;; We do this also for lists of tries, since those occur quite often,
;; but not for other sequence types or deeper nested structures, to keep
;; the implementation as simple as possible.
;;
;; Since the print form of a trie is practically incomprehensible
;; anyway, we don't lose much by doing this. If you *really* want to
;; print tries in full whilst edebugging, despite this warning, disable
;; the advice.
;;
;; FIXME: We should probably use the `cust-print' features instead of advice
;; here.


(eval-when-compile
  (require 'edebug)
  (require 'advice))


(defun trie--edebug-pretty-print (object)
  (cond
   ((trie-p object) "#<trie>")
   ((null object) "nil")
   ((let ((tlist object) (test t))
      (while (or (trie-p (car-safe tlist))
		 (and tlist (setq test nil)))
	(setq tlist (cdr tlist)))
      test)
    (concat "(" (mapconcat (lambda (_dummy) "#<trie>") object " ") ")"))
;; ((vectorp object)
;;  (let ((pretty "[") (len (length object)))
;;    (dotimes (i (1- len))
;; 	(setq pretty
;; 	      (concat pretty
;; 		      (if (trie-p (aref object i))
;; 			  "#<trie>" (prin1-to-string (aref object i))) " ")))
;;    (concat pretty
;; 	      (if (trie-p (aref object (1- len)))
;; 		  "#<trie>" (prin1-to-string (aref object (1- len))))
;; 	      "]")))
   ))


(when (fboundp 'ad-define-subr-args)
  (ad-define-subr-args 'edebug-prin1 '(object &optional printcharfun)))

(defadvice edebug-prin1
  (around trie activate compile preactivate)
  (let ((pretty (trie--edebug-pretty-print object)))
    (if pretty
	(progn
	  (prin1 pretty printcharfun)
	  (setq ad-return-value pretty))
    ad-do-it)))


(when (fboundp 'ad-define-subr-args)
  (ad-define-subr-args 'edebug-prin1-to-string '(object &optional noescape)))

(defadvice edebug-prin1-to-string
  (around trie activate compile preactivate)
  (let ((pretty (trie--edebug-pretty-print object)))
    (if pretty
	(setq ad-return-value pretty)
      ad-do-it)))



(provide 'trie)

;;; trie.el ends here
