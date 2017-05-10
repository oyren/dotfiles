;;; dict-tree.el --- Dictionary data structure  -*- lexical-binding: t; -*-

;; Copyright (C) 2004-2012  Free Software Foundation, Inc

;; Author: Toby Cubitt <toby-predictive@dr-qubit.org>
;; Version: 0.12.8
;; Keywords: extensions, matching, data structures
;;           trie, tree, dictionary, completion, regexp
;; Package-Requires: ((trie "0.2.5") (tNFA "0.1.1") (heap "0.3"))
;; URL: http://www.dr-qubit.org/emacs.php

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
;; A dictionary is used to store strings, along with arbitrary data associated
;; with each string. As well as basic data insertion, manipulation and
;; retrieval, a dictionary can perform prefix searches on those strings,
;; retrieving all strings with a given prefix in either alphabetical or any
;; other order (see the `dictree-complete' and `dictree-complete-ordered'
;; functions), and is able to cache results in order to speed up those
;; searches. The package also provides persistent storage of the data
;; structures to files.
;;
;; You create a dictionary using `dictree-create', add entries to it using
;; `dictree-insert', lookup entries using `dictree-lookup', find completions
;; of sequences using `dictree-complete', find completions and sort them in
;; any order you speficy using `dictree-complete-ordered', map over it using
;; `dictree-map' and `dictree-mapcar', save it to a file using `dictree-save'
;; or `dictree-write', and load from file it using `dictree-load'. Various
;; other useful functions are also provided.
;;
;; This package uses the trie package trie.el. the tagged NFA package tNFA.el,
;; and the heap package heap.el.


;;; Code:

(eval-when-compile (require 'cl))
(require 'trie)
(require 'tNFA)
(require 'bytecomp)



;;; ================================================================
;;;            Replacements for CL and Elisp functions

;; copied from cl-extra.el
(defun dictree--subseq (seq start &optional end)
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



;; `goto-line' without messing around with mark and messages
(defmacro dictree--goto-line (line)
  "Goto line LINE, counting from line 1 at beginning of buffer."
  `(progn
     (goto-char 1)
     (if (eq selective-display t)
	 (re-search-forward "[\n\C-m]" nil 'end (1- ,line))
       (forward-line (1- ,line)))))



;;; ====================================================================
;;;  Internal functions and variables for use in the dictionary package

(defvar dictree-loaded-list nil
  "Stores list of loaded dictionaries.")


;; ----------------------------------------------------------------
;;                   Dictionary data cell structures

;; Note: It would be more elegant to use a defstruct for the data cells,
;;       but the problem is that the resulting setf in
;;       `dictree--wrap-insfun' won't get expanded into the cell-data
;;       accessor function at compile-time because it's burried inside a
;;       backquote construct. Not only is it inelegant to have to expand
;;       macros at run-time whenever `dictree--wrap-insfun' is called,
;;       but it also requires the 'cl-macs package to be loaded at
;;       run-time rather than just at compile-time. We could use
;;       `lexical-let' instead, but it doesn't seem worth it here.

;; wrap data in a cons cell
(defalias 'dictree--cell-create 'cons)  ; INTERNAL USE ONLY

;; get data component from data cons cell
(defalias 'dictree--cell-data 'car)  ; INTERNAL USE ONLY

;; get property list component from data cons cell
(defalias 'dictree--cell-plist 'cdr)  ; INTERNAL USE ONLY

;; set data component of data cons cell
(defalias 'dictree--cell-set-data 'setcar)  ; INTERNAL USE ONLY

;; set property list component of data cons cell
(defalias 'dictree--cell-set-plist 'setcdr)  ; INTERNAL USE ONLY

;; define setf methods so we can use setf abstraction wherever possible
(defsetf dictree--cell-data dictree--cell-set-data)
(defsetf dictree--cell-plist dictree--cell-set-plist)


;; ----------------------------------------------------------------
;;                 Dictionary cache entry structures

;; Note: We *could* use a defstruct for the cache entries, but for
;;       something this simple it doesn't seem worth it, especially
;;       given that we're using the defalias approach anyway for the
;;       data cells (above).

;; Construct and return a completion cache entry
(defalias 'dictree--cache-create 'cons)  ; INTERNAL USE ONLY

;; Return the completions list for cache entry CACHE
(defalias 'dictree--cache-results 'car)  ; INTERNAL USE ONLY

;; Return the max number of completions returned for cache entry CACHE
(defalias 'dictree--cache-maxnum 'cdr)  ; INTERNAL USE ONLY

;; Set the completions list for cache entry CACHE
(defalias 'dictree--cache-set-completions 'setcar)  ; INTERNAL USE ONLY

;; Set the completions list for cache entry CACHE
(defalias 'dictree--cache-set-maxnum 'setcdr)  ; INTERNAL USE ONLY


;; ----------------------------------------------------------------
;;                     Wrapping functions

;; return wrapped insfun to deal with data wrapping
(if (trie-lexical-binding-p)
    (defun dictree--wrap-insfun (insfun)  ; INTERNAL USE ONLY
      (lambda (new old)
	(dictree--cell-set-data old (funcall insfun
					     (dictree--cell-data new)
					     (dictree--cell-data old)))
	old))
  (defun dictree--wrap-insfun (insfun)  ; INTERNAL USE ONLY
    `(lambda (new old)
       (dictree--cell-set-data old (,insfun (dictree--cell-data new)
					    (dictree--cell-data old)))
       old)))


;; return wrapped rankfun to deal with data wrapping
(if (trie-lexical-binding-p)
    (defun dictree--wrap-rankfun (rankfun)  ; INTERNAL USE ONLY
      (lambda (a b)
	(funcall rankfun
		 (cons (car a) (dictree--cell-data (cdr a)))
		 (cons (car b) (dictree--cell-data (cdr b))))))
  (defun dictree--wrap-rankfun (rankfun)  ; INTERNAL USE ONLY
    `(lambda (a b)
       (,rankfun (cons (car a) (dictree--cell-data (cdr a)))
		(cons (car b) (dictree--cell-data (cdr b)))))))


;; return wrapped combfun to deal with data wrapping
(if (trie-lexical-binding-p)
    (defun dictree--wrap-combfun (combfun)  ; INTERNAL USE ONLY
      (lambda (cell1 cell2)
	(cons (funcall combfun
		       (dictree--cell-data cell1)
		       (dictree--cell-data cell2))
	      (append (dictree--cell-plist cell1)
		      (dictree--cell-plist cell2)))))
  (defun dictree--wrap-combfun (combfun)  ; INTERNAL USE ONLY
    `(lambda (cell1 cell2)
       (cons (,combfun (dictree--cell-data cell1)
		       (dictree--cell-data cell2))
	    (append (dictree--cell-plist cell1)
		    (dictree--cell-plist cell2))))))


;; return wrapped filter function to deal with data wrapping
(if (trie-lexical-binding-p)
    (defun dictree--wrap-filter (filter)  ; INTERNAL USE ONLY
      (lambda (key data) (funcall filter key (dictree--cell-data data))))
  (defun dictree--wrap-filter (filter)  ; INTERNAL USE ONLY
    `(lambda (key data) (,filter key (dictree--cell-data data)))))


;; return wrapped result function to deal with data wrapping
(if (trie-lexical-binding-p)
    (defun dictree--wrap-resultfun (resultfun)  ; INTERNAL USE ONLY
      (lambda (res)
	(funcall resultfun (car res) (dictree--cell-data (cdr res)))))
  (defun dictree--wrap-resultfun (resultfun)  ; INTERNAL USE ONLY
    `(lambda (res) (,resultfun (car res) (dictree--cell-data (cdr res))))))




;; ----------------------------------------------------------------
;;                 The dictionary data structures

(defstruct
  (dictree-
   :named
   (:constructor nil)
   (:constructor dictree--create
		 (&optional
		  filename
		  (name (and filename
			     (file-name-sans-extension
			      (file-name-nondirectory filename))))
		  autosave
		  _unlisted
		  (comparison-function '<)
		  (insert-function (lambda (a _b) a))
		  (rank-function (lambda (a b) (> (cdr a) (cdr b))))
		  (cache-policy 'time)
		  (cache-update-policy 'synchronize)
		  lookup-cache-threshold
		  complete-cache-threshold
		  complete-ranked-cache-threshold
		  regexp-cache-threshold
		  regexp-ranked-cache-threshold
		  key-savefun key-loadfun
		  data-savefun data-loadfun
		  plist-savefun plist-loadfun
		  (trie-type 'avl)
		  &aux
		  (modified nil)
		  (trie (make-trie comparison-function trie-type))
		  (insfun (dictree--wrap-insfun insert-function))
		  (rankfun (dictree--wrap-rankfun rank-function))
		  (lookup-cache
		   (if lookup-cache-threshold
		       (make-hash-table :test 'equal)
		     nil))
		  (complete-cache
		   (if complete-cache-threshold
		       (make-hash-table :test 'equal)
		     nil))
		  (complete-ranked-cache
		   (if complete-ranked-cache-threshold
		       (make-hash-table :test 'equal)
		     nil))
		  (regexp-cache
		   (if regexp-cache-threshold
		       (make-hash-table :test 'equal)
		     nil))
		  (regexp-ranked-cache
		   (if regexp-ranked-cache-threshold
		       (make-hash-table :test 'equal)
		     nil))
		  (meta-dict-list nil)
		  ))
   (:constructor dictree--create-custom
		 (&optional
		  filename
		  (name (and filename
			     (file-name-sans-extension
			      (file-name-nondirectory filename))))
		  autosave
		  _unlisted
		  (comparison-function '<)
		  (insert-function (lambda (a _b) a))
		  (rank-function (lambda (a b) (> (cdr a) (cdr b))))
		  (cache-policy 'time)
		  (cache-update-policy 'synchronize)
		  lookup-cache-threshold
		  complete-cache-threshold
		  complete-ranked-cache-threshold
		  regexp-cache-threshold
		  regexp-ranked-cache-threshold
		  key-savefun key-loadfun
		  data-savefun data-loadfun
		  plist-savefun plist-loadfun
		  &key
		  createfun insertfun deletefun
		  lookupfun mapfun emptyfun
		  stack-createfun stack-popfun stack-emptyfun
		  transform-for-print transform-from-read
		  &aux
		  (modified nil)
		  (trie (make-trie-custom
			 comparison-function
			 :createfun createfun
			 :insertfun insertfun
			 :deletefun deletefun
			 :lookupfun lookupfun
			 :mapfun mapfun
			 :emptyfun emptyfun
			 :stack-createfun stack-createfun
			 :stack-popfun stack-popfun
			 :stack-emptyfun stack-emptyfun
			 :transform-for-print transform-for-print
			 :transform-from-read transform-from-read))
		  (insfun (dictree--wrap-insfun insert-function))
		  (rankfun (dictree--wrap-rankfun rank-function))
		  (lookup-cache
		   (if lookup-cache-threshold
		       (make-hash-table :test 'equal)
		     nil))
		  (complete-cache
		   (if complete-cache-threshold
		       (make-hash-table :test 'equal)
		     nil))
		  (complete-ranked-cache
		   (if complete-ranked-cache-threshold
		       (make-hash-table :test 'equal)
		     nil))
		  (regexp-cache
		   (if regexp-cache-threshold
		       (make-hash-table :test 'equal)
		     nil))
		  (regexp-ranked-cache
		   (if regexp-ranked-cache-threshold
		       (make-hash-table :test 'equal)
		     nil))
		  (meta-dict-list nil)
		  ))
   (:copier dictree--copy))
  name filename autosave modified
  comparison-function insert-function insfun rank-function rankfun
  cache-policy cache-update-policy
  lookup-cache lookup-cache-threshold
  complete-cache complete-cache-threshold
  complete-ranked-cache complete-ranked-cache-threshold
  regexp-cache regexp-cache-threshold
  regexp-ranked-cache regexp-ranked-cache-threshold
  key-savefun key-loadfun
  data-savefun data-loadfun
  plist-savefun plist-loadfun
  trie meta-dict-list)


(defstruct
  (dictree--meta-dict
   :named
   (:constructor nil)
   (:constructor dictree--meta-dict-create
		 (dictionary-list
		  &optional
		  filename
		  (name (file-name-sans-extension
			 (file-name-nondirectory filename)))
		  autosave
		  _unlisted
		  (combine-function '+)
		  (cache-policy 'time)
		  (cache-update-policy 'synchronize)
		  lookup-cache-threshold
		  complete-cache-threshold
		  complete-ranked-cache-threshold
		  regexp-cache-threshold
		  regexp-ranked-cache-threshold
		  &aux
		  (dictlist
		   (mapcar
		    (lambda (dic)
		      (cond
		       ((dictree-p dic) dic)
		       ((symbolp dic) (symbol-value dic))
		       (t (error "Invalid object in DICTIONARY-LIST"))))
		    dictionary-list))
		  (combfun (dictree--wrap-combfun combine-function))
		  (lookup-cache
		   (if lookup-cache-threshold
		       (make-hash-table :test 'equal)
		     nil))
		  (complete-cache
		   (if complete-cache-threshold
		       (make-hash-table :test 'equal)
		     nil))
		  (complete-ranked-cache
		   (if complete-ranked-cache-threshold
		       (make-hash-table :test 'equal)
		     nil))
		  (regexp-cache
		   (if regexp-cache-threshold
		       (make-hash-table :test 'equal)
		     nil))
		  (regexp-ranked-cache
		   (if regexp-ranked-cache-threshold
		       (make-hash-table :test 'equal)
		     nil))
		  ))
   (:copier dictree--meta-dict-copy))
  name filename autosave modified
  combine-function combfun
  cache-policy cache-update-policy
  lookup-cache lookup-cache-threshold
  complete-cache complete-cache-threshold
  complete-ranked-cache complete-ranked-cache-threshold
  regexp-cache regexp-cache-threshold
  regexp-ranked-cache regexp-ranked-cache-threshold
  dictlist meta-dict-list)



;; ----------------------------------------------------------------
;;           Miscelaneous internal functions and macros

(defun dictree--trielist (dict)
  ;; Return a list of all the tries on which DICT is based. If DICT is a
  ;; meta-dict, this recursively descends the hierarchy, gathering all
  ;; the tries from the base dictionaries.
  (let (accumulate)
    (dictree--do-trielist dict)
    accumulate))

(defun dictree--do-trielist (dict)
  (declare (special accumulate))
  (if (dictree-meta-dict-p dict)
      (mapc 'dictree--do-trielist (dictree--meta-dict-dictlist dict))
    (setq accumulate (cons (dictree--trie dict) accumulate))))


(defun dictree--merge (list1 list2 cmpfun &optional combfun maxnum)
  ;; Destructively merge together sorted lists LIST1 and LIST2, sorting
  ;; elements according to CMPFUN. For non-null MAXNUM, only the first
  ;; MAXNUM are kept. For non-null COMBFUN, duplicate elements will be
  ;; merged by passing the two elements as arguments to COMBFUN, and
  ;; using the return value as the merged element.
  (or (listp list1) (setq list1 (append list1 nil)))
  (or (listp list2) (setq list2 (append list2 nil)))
  (let (res (i 0))

    ;; build up result list backwards
    (while (and list1 list2 (or (null maxnum) (< (incf i) maxnum)))
      ;; move smaller element to result list
      (if (funcall cmpfun (car list1) (car list2))
	  (push (pop list1) res)
	(if (funcall cmpfun (car list2) (car list1))
	    (push (pop list2) res)
	  ;; if elements are equal, merge them for non-null COMBFUN
	  (if combfun
	      (push (funcall combfun (pop list1) (pop list2))
		    res)
	    ;; otherwise, add both to result list, in order
	    (push (pop list1) res)
	    (push (pop list2) res)))))

    ;; return result if we already have MAXNUM entries
    (if (and maxnum (= i maxnum))
	(nreverse res)
      ;; otherwise, return result plus enough leftover entries to make
      ;; up MAXNUM (only one of list1 or list2 will be non-nil)
      (let (tmp)
	(or (null maxnum)
	    (and (setq tmp (nthcdr (- maxnum i 1) list1))
		 (setcdr tmp nil))
	    (and (setq tmp (nthcdr (- maxnum i 1) list2))
		 (setcdr tmp nil)))
	(nconc (nreverse res) list1 list2)))
    ))


;; (defun dictree--merge-sort (list sortfun &optional combfun)
;;   ;; Destructively sort LIST according to SORTFUN, combining
;;   ;; identical elements using COMBFUN if supplied.
;;   (dictree--do-merge-sort list (/ (length list) 2) sortfun combfun))


;; (defun dictree--do-merge-sort (list1 len sortfun combfun)
;;   ;; Merge sort LIST according to SORTFUN, combining identical
;;   ;; elements using COMBFUN.
;;   (let* ((p (nthcdr (1- len) list1))
;; 	 (list2 (cdr p)))
;;     (setcdr p nil)
;;     (dictree--merge
;;      (dictree--do-merge-sort list1 (/ len 2) sortfun combfun)
;;      (dictree--do-merge-sort list2 (/ len 2) sortfun combfun)
;;      sortfun combfun)))




;;; ================================================================
;;;    The (mostly) public functions which operate on dictionaries

;;;###autoload
(defun make-dictree
  (&optional
   name filename autosave unlisted
   comparison-function insert-function rank-function
   cache-policy cache-update-policy
   lookup-cache-threshold
   complete-cache-threshold
   complete-ranked-cache-threshold
   regexp-cache-threshold
   regexp-ranked-cache-threshold
   key-savefun key-loadfun
   data-savefun data-loadfun
   plist-savefun plist-loadfun
   trie-type)
  "Create an empty dictionary and return it.

If NAME is supplied, the dictionary is stored in the variable
NAME. Defaults to FILENAME stripped of directory and
extension. (Regardless of the value of NAME, the dictionary will
be stored in the default variable name when it is reloaded from
file.)

FILENAME supplies a directory and file name to use when saving
the dictionary. If the AUTOSAVE flag is non-nil, then the
dictionary will automatically be saved to this file when it is
unloaded or when exiting Emacs.

If UNLISTED is non-nil, the dictionary will not be added to the
list of loaded dictionaries. Note that this disables autosaving.

COMPARE-FUNCTION sets the function used to compare elements of
the keys. It should take two arguments, A and B, both of the type
contained by the sequences used as keys \(e.g. if the keys will
be strings, the function will be passed two characters\). It
should return t if the first is \"less than\" the
second. Defaults to `<'.

INSERT-FUNCTION sets the function used to insert data into the
dictionary. It should take two arguments: the new data, and the
data already in the dictionary, and should return the data to
insert. Defaults to replacing any existing data with the new
data.

RANK-FUNCTION sets the function used to rank the results of
`dictree-complete'. It should take two arguments, each a cons
whose car is a dictree key (a sequence) and whose cdr is the data
associated with that key. It should return non-nil if the first
argument is \"better\" than the second, nil otherwise. It
defaults to \"lexical\" comparison of the keys, ignoring the data
\(which is not very useful, since an unranked `dictree-complete'
query already does this much more efficiently\).

CACHE-POLICY should be a symbol ('time, 'length, or 'both), which
determines which query operations are cached. The 'time setting
caches queries that take longer (in seconds) than the
corresponding CACHE-THRESHOLD value. The 'length setting caches
lookups of key sequences that are longer than
LOOKUP-CACHE-THRESHOLD value (since those are likely to be the
slower ones), and caches completions of prefixes that are shorter
than the corresponding CACHE-THRESHOLD (since those are likely to
be the slower ones in that case). The setting 'both requires both
conditions to be satisfied simultaneously. In this case,
CACHE-THRESHOLD must be a plist with properties :time and :length
specifying the corresponding cache thresholds.

CACHE-UPDATE-POLICY should be a symbol ('synchronize or 'delete),
which determines how the caches are updated when data is inserted
or deleted. The former updates tainted cache entries, which makes
queries faster but insertion and deletion slower, whereas the
latter deletes any tainted cache entries, which makes queries
slower but insertion and deletion faster.

The CACHE-THRESHOLD settings set the threshold for caching the
corresponding dictionary query (lookup, completion, ranked
completion). The meaning of these values depends on the setting
of CACHE-POLICY (see above).

All CACHE-THRESHOLD's default to nil. The values nil and t are
special. If a CACHE-THRESHOLD is set to nil, no caching is done
for that type of query. If it is t, everything is cached for that
type of query \(similar behaviour can be obtained by setting the
CACHE-THRESHOLD to 0, but it is better to use t\).

KEY-SAVEFUN, DATA-SAVEFUN and PLIST-SAVEFUN are functions used to
convert keys, data and property lists into lisp objects that have
a valid read syntax, for writing to file. DATA-SAVEFUN and
PLIST-SAVEFUN are used when saving the dictionary (see
`dictree-save' and `dictree-write'), and all three functions are
used when dumping the contents of the dictionary \(see
`dictree-dump-to-buffer' and `dictree-dump-to-file'\).
KEY-SAVEFUN, DATA-SAVEFUN and PLIST-SAVEFUN should each accept
one argument: a key, data or property list from DICT,
respectively. They should return a lisp object which has a valid
read syntax. When defining these functions, be careful not to
accidentally modify the lisp object in the dictionary; usually,
you will need to make a copy before converting it.

KEY-LOADFUN, DATA-LOADFUN and PLIST-LOADFUN are used to convert
keys, data and property lists back again when loading a
dictionary (only DATA-LOADFUN and PLIST-LOADFUN, see
`dictree-save' and `dictree-write') or populating it from a
file (all three, see `dictree-populate-from-file'). They should
accept one argument: a lisp object of the type produced by the
corresponding SAVEFUN, and return a lisp object to use in the
loaded dictionary.

TRIE-TYPE sets the type of trie to use as the underlying data
structure. See `trie-create' for details."

  ;; sadly, passing null values over-rides the defaults in the defstruct
  ;; dictree--create, so we have to explicitly set the defaults again
  ;; here
  (or name (setq name (and filename (file-name-sans-extension
				     (file-name-nondirectory filename)))))
  (or comparison-function (setq comparison-function '<))
  (or insert-function (setq insert-function (lambda (a _b) a)))
  (or rank-function (setq rank-function (lambda (a b) (> (cdr a) (cdr b)))))
  (or cache-policy (setq cache-policy 'time))
  (or cache-update-policy (setq cache-update-policy 'synchronize))
  (or trie-type (setq trie-type 'avl))

  (let ((dict
	 (dictree--create
	  filename (when name (symbol-name name)) autosave unlisted
	  comparison-function insert-function rank-function
	  cache-policy cache-update-policy
	  lookup-cache-threshold
	  complete-cache-threshold
	  complete-ranked-cache-threshold
	  regexp-cache-threshold
	  regexp-ranked-cache-threshold
	  key-savefun key-loadfun
	  data-savefun data-loadfun
	  plist-savefun plist-loadfun
	  trie-type)))
    ;; store dictionary in variable NAME
    (when name (set name dict))
    ;; add it to loaded dictionary list, unless it's unlisted
    (unless (or (null name) unlisted)
      (push dict dictree-loaded-list))
    dict))


;;;###autoload
(defalias 'dictree-create 'make-dictree)


;;;###autoload
(defun* make-dictree-custom
    (&optional
     name filename autosave unlisted
     &key
     comparison-function insert-function rank-function
     cache-policy cache-update-policy
     lookup-cache-threshold
     complete-cache-threshold
     complete-ranked-cache-threshold
     regexp-cache-threshold
     regexp-ranked-cache-threshold
     key-savefun key-loadfun
     data-savefun data-loadfun
     plist-savefun plist-loadfun
     createfun insertfun deletefun lookupfun mapfun emptyfun
     stack-createfun stack-popfun stack-emptyfun
     transform-for-print transform-from-read)
  "Create an empty dictionary and return it.

The NAME through PLIST-LOADFUN arguments are as for
`dictree-create' (which see).

The remaining arguments control the type of trie to use as the
underlying data structure. See `trie-create' for details."

  ;; sadly, passing null values over-rides the defaults in the defstruct
  ;; dictree--create, so we have to explicitly set the defaults again
  ;; here
  (or name (setq name (and filename (file-name-sans-extension
				     (file-name-nondirectory filename)))))
  (or comparison-function (setq comparison-function '<))
  (or insert-function (setq insert-function (lambda (a _b) a)))
  (or rank-function (setq rank-function (lambda (a b) (< (cdr a) (cdr b)))))
  (or cache-policy (setq cache-policy 'time))
  (or cache-update-policy (setq cache-update-policy 'synchronize))

  (let ((dict
	 (dictree--create-custom
	  filename (when name (symbol-name name)) autosave unlisted
	  comparison-function insert-function rank-function
	  cache-policy cache-update-policy
	  lookup-cache-threshold
	  complete-cache-threshold
	  complete-ranked-cache-threshold
	  regexp-cache-threshold
	  regexp-ranked-cache-threshold
	  key-savefun key-loadfun
	  data-savefun data-loadfun
	  plist-savefun plist-loadfun
	  :createfun createfun
	  :insertfun insertfun
	  :deletefun deletefun
	  :lookupfun lookupfun
	  :mapfun mapfun
	  :emptyfun emptyfun
	  :stack-createfun stack-createfun
	  :stack-popfun stack-popfun
	  :stack-emptyfun stack-emptyfun
	  :transform-for-print transform-for-print
	  :transform-from-read transform-from-read)))
    ;; store dictionary in variable NAME
    (when name (set name dict))
    ;; add it to loaded dictionary list, unless it's unlisted
    (unless (or (null name) unlisted)
      (push dict dictree-loaded-list))
    dict))


;;;###autoload
(defalias 'dictree-create-custom 'make-dictree-custom)


;;;###autoload
(defun make-dictree-meta-dict
  (dictionary-list
   &optional
   name filename autosave unlisted
   combine-function
   cache-policy cache-update-policy
   lookup-cache-threshold
   complete-cache-threshold
   complete-ranked-cache-threshold
   regexp-cache-threshold
   regexp-ranked-cache-threshold)
  "Create a meta-dictionary based on the list of dictionaries
in DICTIONARY-LIST.

COMBINE-FUNCTION is used to combine data from different
dictionaries. It is passed two pieces of data, each an
association of the same key, but in different dictionaries. It
should return a combined datum.

The other arguments are as for `dictree-create'. Note that
caching is only possible if NAME is supplied, otherwise the
cache-threshold arguments are ignored."

  ;; sadly, passing null values over-rides the defaults in the defstruct
  ;; `dictree--create', so we have to explicitly set the defaults again
  ;; here
  (or name (setq name (and filename
			   (file-name-sans-extension
			    (file-name-nondirectory filename)))))
  (or combine-function (setq combine-function '+))
  (or cache-policy (setq cache-policy 'time))
  (or cache-update-policy (setq cache-update-policy 'synchronize))

  (let ((dict
	 (dictree--meta-dict-create
	  dictionary-list filename (when name (symbol-name name))
	  autosave unlisted
	  combine-function
	  cache-policy cache-update-policy
	  (when name lookup-cache-threshold)
	  (when name complete-cache-threshold)
	  (when name complete-ranked-cache-threshold)
	  (when name regexp-cache-threshold)
	  (when name regexp-ranked-cache-threshold))
	 ))
    ;; store dictionary in variable NAME
    (when name (set name dict))
    ;; add it to loaded dictionary list, unless it's unlisted
    (unless (or (null name) unlisted)
      (push dict dictree-loaded-list))
    ;; update meta-dict-list cells of constituent dictionaries
    (unless (or (null name)
		(not (or lookup-cache-threshold
			 complete-cache-threshold
			 complete-ranked-cache-threshold
			 regexp-cache-threshold
			 regexp-ranked-cache-threshold)))
      (mapc
       (lambda (dic)
	 (if (symbolp dic) (setq dic (symbol-value dic)))
	 (setf (dictree--meta-dict-list dic)
	       (cons dict (dictree--meta-dict-list dic))))
       dictionary-list))
    dict))

(defalias 'dictree-create-meta-dict 'make-dictree-meta-dict)


;;;###autoload
(defun dictree-p (obj)
  "Return t if OBJ is a dictionary tree, nil otherwise."
  (or (dictree--p obj) (dictree--meta-dict-p obj)))


(defalias 'dictree-meta-dict-p 'dictree--meta-dict-p
  "Return t if argument is a meta-dictionary, nil otherwise.")

(defun dictree-empty-p (dict)
  "Return t if the dictionary DICT is empty, nil otherwise."
  (if (dictree--meta-dict-p dict)
      (catch 'nonempty
	(mapc (lambda (dic)
		(if (not (dictree-empty-p dic)) (throw 'nonempty t)))
	      (dictree--meta-dict-dictlist dict)))
    (trie-empty (dictree--trie dict))))

(defsubst dictree-autosave (dict)
  "Return dictionary's autosave flag."
  (if (dictree--meta-dict-p dict)
      (dictree--meta-dict-autosave dict)
    (dictree--autosave dict)))

(defsetf dictree-autosave (dict) (val)
  ;; setf method for dictionary autosave flag
  `(if (dictree--meta-dict-p ,dict)
       (setf (dictree--meta-dict-autosave ,dict) ,val)
     (setf (dictree--autosave ,dict) ,val)))

(defsubst dictree-modified (dict)
  "Return dictionary's modified flag."
  (if (dictree--meta-dict-p dict)
      (dictree--meta-dict-modified dict)
    (dictree--modified dict)))

(defsetf dictree-modified (dict) (val)
  ;; setf method for dictionary modified flag
  `(if (dictree--meta-dict-p ,dict)
       (setf (dictree--meta-dict-modified ,dict) ,val)
     (setf (dictree--modified ,dict) ,val)))

(defsubst dictree-name (dict)
  "Return dictionary DICT's name."
  (if (dictree--meta-dict-p dict)
      (dictree--meta-dict-name dict)
    (dictree--name dict)))

(defsetf dictree-name (dict) (name)
  ;; setf method for dictionary name
  `(if (dictree--meta-dict-p ,dict)
       (setf (dictree--meta-dict-name ,dict) ,name)
    (setf (dictree--name ,dict) ,name)))

(defsubst dictree-filename (dict)
  "Return dictionary DICT's associated file name."
  (if (dictree--meta-dict-p dict)
      (dictree--meta-dict-filename dict)
    (dictree--filename dict)))

(defsetf dictree-filename (dict) (filename)
  ;; setf method for dictionary filename
  `(if (dictree--meta-dict-p ,dict)
       (setf (dictree--meta-dict-filename ,dict) ,filename)
     (setf (dictree--filename ,dict) ,filename)))

(defun dictree-comparison-function (dict)
  "Return dictionary DICT's comparison function."
  (if (dictree--meta-dict-p dict)
      (dictree-comparison-function
       (car (dictree--meta-dict-dictlist dict)))
    (dictree--comparison-function dict)))

(defalias 'dictree-insert-function 'dictree--insert-function
  "Return the insertion function for dictionary DICT.")

(defun dictree-rank-function (dict)
  "Return the rank function for dictionary DICT"
  (if (dictree--meta-dict-p dict)
      (dictree-rank-function (car (dictree--meta-dict-dictlist dict)))
    (dictree--rank-function dict)))

(defun dictree-rankfun (dict)
  ;; Return the rank function for dictionary DICT
  (if (dictree--meta-dict-p dict)
      (dictree-rankfun (car (dictree--meta-dict-dictlist dict)))
    (dictree--rankfun dict)))

(defalias 'dictree-meta-dict-combine-function
  'dictree--meta-dict-combine-function
  "Return the combine function for meta-dictionary DICT.")

(defalias 'dictree-meta-dict-dictlist
  'dictree--meta-dict-dictlist
  "Return the list of constituent dictionaries
for meta-dictionary DICT.")

(defsubst dictree-cache-policy (dict)
  "Return the cache policy for dictionary DICT."
  (if (dictree--meta-dict-p dict)
      (dictree--meta-dict-cache-policy dict)
    (dictree--cache-policy dict)))

(defsubst dictree-cache-update-policy (dict)
  "Return the cache update policy for dictionary DICT."
  (if (dictree--meta-dict-p dict)
      (dictree--meta-dict-cache-update-policy dict)
    (dictree--cache-update-policy dict)))

(defsubst dictree-lookup-cache-threshold (dict)
  "Return the lookup cache threshold for dictionary DICT."
  (if (dictree--meta-dict-p dict)
      (dictree--meta-dict-lookup-cache-threshold dict)
    (dictree--lookup-cache-threshold dict)))

(defsetf dictree-lookup-cache-threshold (dict) (param)
  ;; setf method for lookup cache threshold
  `(if (dictree--meta-dict-p ,dict)
       (setf (dictree--meta-dict-lookup-cache-threshold ,dict)
	     ,param)
     (setf (dictree--lookup-cache-threshold ,dict)
	   ,param)))

(defsubst dictree-lookup-cache (dict)
  ;; Return the lookup cache for dictionary DICT.
  (if (dictree--meta-dict-p dict)
      (dictree--meta-dict-lookup-cache dict)
    (dictree--lookup-cache dict)))

(defsubst dictree-complete-cache-threshold (dict)
  "Return the completion cache threshold for dictionary DICT."
  (if (dictree--meta-dict-p dict)
      (dictree--meta-dict-complete-cache-threshold dict)
    (dictree--complete-cache-threshold dict)))

(defsetf dictree-complete-cache-threshold (dict) (param)
  ;; setf method for completion cache threshold
  `(if (dictree--meta-dict-p ,dict)
       (setf (dictree--meta-dict-complete-cache-threshold ,dict)
	     ,param)
     (setf (dictree--complete-cache-threshold ,dict)
	   ,param)))

(defun dictree-complete-cache (dict)
  ;; Return the completion cache for dictionary DICT.
  (if (dictree--meta-dict-p dict)
      (dictree--meta-dict-complete-cache dict)
    (dictree--complete-cache dict)))

(defsubst dictree-complete-ranked-cache-threshold (dict)
  "Return the ranked completion cache threshold for dictionary DICT."
  (if (dictree--meta-dict-p dict)
      (dictree--meta-dict-complete-ranked-cache-threshold dict)
    (dictree--complete-ranked-cache-threshold dict)))

(defsetf dictree-complete-ranked-cache-threshold (dict) (param)
  ;; setf method for ranked completion cache threshold
  `(if (dictree--meta-dict-p ,dict)
       (setf (dictree--meta-dict-complete-ranked-cache-threshold ,dict)
	     ,param)
     (setf (dictree--complete-ranked-cache-threshold ,dict)
	   ,param)))

(defun dictree-complete-ranked-cache (dict)
  ;; Return the ranked completion cache for dictionary DICT.
  (if (dictree--meta-dict-p dict)
      (dictree--meta-dict-complete-ranked-cache dict)
    (dictree--complete-ranked-cache dict)))

(defsubst dictree-regexp-cache-threshold (dict)
  "Return the regexp cache threshold for dictionary DICT."
  (if (dictree--meta-dict-p dict)
      (dictree--meta-dict-regexp-cache-threshold dict)
    (dictree--regexp-cache-threshold dict)))

(defsetf dictree-regexp-cache-threshold (dict) (param)
  ;; setf method for regexp cache threshold
  `(if (dictree--meta-dict-p ,dict)
       (setf (dictree--meta-dict-regexp-cache-threshold ,dict)
	     ,param)
     (setf (dictree--regexp-cache-threshold ,dict)
	   ,param)))

(defun dictree-regexp-cache (dict)
  ;; Return the regexp cache for dictionary DICT.
  (if (dictree--meta-dict-p dict)
      (dictree--meta-dict-regexp-cache dict)
    (dictree--regexp-cache dict)))

(defsubst dictree-regexp-ranked-cache-threshold (dict)
  "Return the ranked regexp cache threshold for dictionary DICT."
  (if (dictree--meta-dict-p dict)
      (dictree--meta-dict-regexp-ranked-cache-threshold dict)
    (dictree--regexp-ranked-cache-threshold dict)))

(defsetf dictree-regexp-ranked-cache-threshold (dict) (param)
  ;; setf method for ranked regexp cache threshold
  `(if (dictree--meta-dict-p ,dict)
       (setf (dictree--meta-dict-regexp-ranked-cache-threshold ,dict)
	     ,param)
     (setf (dictree--regexp-ranked-cache-threshold ,dict)
	   ,param)))

(defun dictree-regexp-ranked-cache (dict)
  ;; Return the ranked regexp cache for dictionary DICT.
  (if (dictree--meta-dict-p dict)
      (dictree--meta-dict-regexp-ranked-cache dict)
    (dictree--regexp-ranked-cache dict)))



;; ----------------------------------------------------------------
;;                  Inserting and deleting data

(defun dictree-insert (dict key &optional data insert-function)
  "Insert KEY and DATA into dictionary DICT.
If KEY does not already exist, this creates it. How the data is
inserted depends on the dictionary's insertion function \(see
`dictree-create'\).

The optional INSERT-FUNCTION over-rides the dictionary's own
insertion function. If KEY already exists in DICT,
INSERT-FUNCTION is called with two arguments: the data DATA, and
the data associated with KEY in the dictionary. Its return value
becomes the new association for KEY."

  ;; if dictionary is a meta-dictionary, insert key into all the
  ;; dictionaries it's based on
  (if (dictree--meta-dict-p dict)
      (mapc (lambda (dic)
	      (dictree-insert dic key data insert-function))
	    (dictree--meta-dict-dictlist dict))

    ;; otherwise...
    (let (newdata)
      ;; set the dictionary's modified flag
      (setf (dictree-modified dict) t)
      ;; insert key in dictionary's ternary search tree
      (setq newdata
	    (trie-insert
	     (dictree--trie dict) key (dictree--cell-create data nil)
	     (or (and insert-function
		      (dictree--wrap-insfun insert-function))
		 (dictree--insfun dict))))
      ;; update dictionary's caches
      (dictree--update-cache dict key newdata)
      ;; update cache's of any meta-dictionaries based on dict
      (mapc (lambda (dic) (dictree--update-cache dic key newdata))
	    (dictree--meta-dict-list dict))

      ;; return the new data
      (dictree--cell-data newdata))))



(defun dictree-delete (dict key &optional test)
  "Delete KEY from DICT.
Returns non-nil if KEY was deleted, nil if KEY was not in DICT.

If TEST is supplied, it should be a function that accepts three
arguments: the key being deleted, its associated data, and its
associated property list. The key will then only be deleted if
TEST returns non-nil."

  (let ((dictree--delete-test test)
	deleted del)
    (cond
     ;; if DICT is a meta-dictionary, delete KEY from all dictionaries
     ;; it's based on
     ((dictree--meta-dict-p dict)
      (dolist (dic (dictree--meta-dict-dictlist dict))
	(when (setq del (dictree-delete dic key))
	  (setq deleted (cons del deleted))))
      (setf (dictree-modified dict) (and deleted t))
      (setq deleted (nreverse deleted)))

     ;; otherwise...
     (t
      (setq deleted
	    (trie-delete (dictree--trie dict) key
			 (when dictree--delete-test
			   (lambda (k cell)
			     (funcall dictree--delete-test
				      k (dictree--cell-data cell)
				      (dictree--cell-plist cell))))))
      ;; if key was deleted, have to update the caches
      (when deleted
	(dictree--update-cache dict key nil t)
	(setf (dictree-modified dict) t)
	;; update cache's of any meta-dictionaries based on DICT
	(mapc (lambda (dic)
		(dictree--update-cache dic key nil t))
	      (dictree--meta-dict-list dict)))))

    ;; return deleted key/data pair
    (when deleted
      (cons (car deleted) (dictree--cell-data (cdr deleted))))))



;; ----------------------------------------------------------------
;;                     Cache updating

(defun dictree--prefix-p (prefix str)
  "Return t if PREFIX is a prefix of STR, nil otherwise.

PREFIX and STR can be any sequence type (string, vector, or
list), but they must both be the same type. PREFIX can also be a
list of sequences, in which case it returns t if any element of
PREFIX is a prefix of STR."
  ;; wrap prefix in a list if necessary
  ;; FIXME: the test for a list of prefixes, below, will fail if the
  ;;        PREFIX sequence is a list, and the elements of PREFIX are
  ;;        themselves lists (there might be no easy way to fully fix
  ;;        this...)
  (when (or (atom prefix)
	    (and (listp prefix) (not (sequencep (car prefix)))))
    (setq prefix (list prefix)))
  (let (len)
    (catch 'is-prefix
      (dolist (pfx prefix)
	(setq len (length pfx))
	(when (and (<= len (length str))
		   (equal pfx (dictree--subseq str 0 len)))
	  (throw 'is-prefix t))))))


(defun dictree--above-cache-threshold-p
  (time length policy threshold &optional cache-long-keys)
  ;; Return t if query taking TIME seconds for a key of length LENGTH
  ;; should be cached according to the cache POLICY and
  ;; THRESHOLD. Otherwise, return nil. Optional argument CACHE-LONG-KEYS
  ;; means that keys of length longer than THRESHOLD are to be
  ;; cached. Default is keys of length shorter than THRESHOLD.
  (and threshold
       (or (eq threshold t)
	   (and (eq policy 'time) (>= time threshold))
	   ;; note: we cache lookups of *longer* keys, because those are
	   ;;       likely to be slower ones
	   (and (eq policy 'length)
		(if cache-long-keys
		    (>= length threshold) (<= length threshold)))
	   (and (eq policy 'both)
		(or (>= time (plist-get threshold :time))
		    (if cache-long-keys
			(>= length (plist-get threshold :length))
		      (<= length (plist-get threshold :length))))))))


(defun dictree--update-cache (dict key newdata &optional deleted)
  ;; Synchronise dictionary DICT's caches, given that the data
  ;; associated with KEY has been changed to NEWDATA, or KEY has been
  ;; deleted if DELETED is non-nil (NEWDATA is ignored in that case)."
  (let (arg reverse cache)

    ;; synchronise the lookup cache if dict is a meta-dictionary, since
    ;; it's not done automatically
    (when (and (dictree--meta-dict-p dict)
	       (dictree--meta-dict-lookup-cache-threshold dict))
      (setq cache (dictree--lookup-cache dict))
      (cond
       ;; if updating dirty cache entries...
       ((eq (dictree-cache-update-policy dict) 'synchronize)
	(when (gethash key cache)
	  (if deleted (remhash key cache) (puthash key newdata cache))))
       ;; if deleting dirty cache entries...
       (t (remhash key cache))))

    ;; synchronize the completion cache, if it exists
    (when (dictree-complete-cache-threshold dict)
      (setq cache (dictree-complete-cache dict))
      ;; check every cache entry to see if it matches
      (maphash
       (lambda (cache-key cache-entry)
	 (setq arg (car cache-key))
	 (when (dictree--prefix-p arg key)
	   (setq reverse (cdr cache-key))
	    (cond
	     ;; if updating dirty cache entries...
	     ((eq (dictree-cache-update-policy dict) 'synchronize)
	      (dictree--synchronize-completion-cache
	       dict cache-entry arg reverse key newdata deleted))
	     ;; if deleting dirty cache entries...
	     (t (remhash (cons arg reverse) cache)))))
       cache))

    ;; synchronize the ranked completion cache, if it exists
    (when (dictree-complete-ranked-cache-threshold dict)
      (setq cache (dictree-complete-ranked-cache dict))
      ;; check every cache entry to see if it matches
      (maphash
       (lambda (cache-key cache-entry)
	 (setq arg (car cache-key))
	 (when (dictree--prefix-p arg key)
	   (setq reverse (cdr cache-key))
	    (cond
	     ;; if updating dirty cache entries...
	     ((eq (dictree-cache-update-policy dict) 'synchronize)
	      (dictree--synchronize-ranked-completion-cache
	       dict cache-entry arg reverse key newdata deleted))
	     ;; if deleting dirty cache entries...
	     (t (remhash (cons arg reverse) cache)))))
       cache))

    ;; synchronize the regexp cache, if it exists
    (when (dictree-regexp-cache-threshold dict)
      (setq cache (dictree--regexp-cache dict))
      ;; check every cache entry to see if it matches
      (maphash
       (lambda (cache-key cache-entry)
	 (setq arg (car cache-key))
	 (when (tNFA-regexp-match
		arg key :test (dictree--comparison-function dict))
	   (setq reverse (cdr cache-key))
	   (cond
	    ;; if updating dirty cache entries...
	    ((eq (dictree-cache-update-policy dict) 'synchronize)
	     (dictree--synchronize-regexp-cache
	      dict cache-entry arg reverse key newdata deleted))
	    ;; if deleting dirty cache entries...
	    (t (remhash (cons arg reverse) cache)))))
       cache))

    ;; synchronize the ranked regexp cache, if it exists
    (when (dictree-regexp-ranked-cache-threshold dict)
      (setq cache (dictree-regexp-ranked-cache dict))
      ;; have to check every cache entry to see if it matches
      (maphash
       (lambda (cache-key cache-entry)
	 (setq arg (car cache-key))
	 (when (tNFA-regexp-match
		arg key :test (dictree--comparison-function dict))
	   (setq reverse (cdr cache-key))
	   (cond
	    ;; if updating dirty cache entries...
	    ((eq (dictree-cache-update-policy dict) 'synchronize)
	     (dictree--synchronize-ranked-regexp-cache
	      dict cache-entry arg reverse key newdata deleted))
	    ;; if deleting dirty cache entries...
	    (t (remhash (cons arg reverse) cache)))))
       cache))
    ))



(defun dictree--synchronize-completion-cache
  (dict cache-entry arg reverse key newdata deleted)
  ;; Synchronize DICT's completion CACHE-ENTRY for ARG and REVERSE, for
  ;; a KEY whose data was either updated to NEWDATA or DELETED.
  (let* ((completions (dictree--cache-results cache-entry))
	 (maxnum (dictree--cache-maxnum cache-entry))
	 (cmpl (assoc key completions)))
    ;; if key was...
    (cond
     ;; deleted and in cached result: remove cache entry and re-run the
     ;; same completion to update the cache
     ((and deleted cmpl)
      (remhash (cons arg reverse) (dictree-complete-cache dict))
      (dictree-complete dict arg nil maxnum reverse))
     ;; modified and not in cached result: merge it into the completion
     ;; list, retaining only the first maxnum
     ((and (not deleted) (not cmpl))
      (dictree--cache-set-completions
       cache-entry
       (dictree--merge
	(list (cons key newdata)) completions
	`(lambda (a b)
	   (,(trie-construct-sortfun
	      (dictree-comparison-function dict))
	    (car a) (car b)))
	(when (dictree--meta-dict-p dict)
	  (dictree--meta-dict-combfun dict))
	maxnum)))
     ;; modified and in the cached result: update the associated data if
     ;; dict is a meta-dictionary (this is done automatically for a
     ;; normal dict)
     ((and (not deleted) cmpl (dictree--meta-dict-p dict))
      (setcdr cmpl newdata))
     ;; deleted and not in cached result: requires no action
     )))



(defun dictree--synchronize-ranked-completion-cache
  (dict cache-entry arg reverse key newdata deleted)
  ;; Synchronize DICT's ranked completion CACHE-ENTRY for ARG and
  ;; REVERSE, for a KEY whose data was either updated to NEWDATA or
  ;; DELETED.
  (let* ((completions (dictree--cache-results cache-entry))
	 (maxnum (dictree--cache-maxnum cache-entry))
	 (cmpl (assoc key completions))
	 (cache (dictree--complete-ranked-cache dict)))
    ;; if key was...
    (cond
     ;; deleted and in cached result: remove cache entry and re-run the
     ;; same query to update the cache
     ((and deleted cmpl)
      (remhash (cons arg reverse) cache)
      (dictree-complete dict arg 'ranked maxnum reverse))
     ;; modified and not in cached result: merge it into the completion
     ;; list, retaining only the first maxnum
     ((and (not deleted) (not cmpl))
      (dictree--cache-set-completions
       cache-entry
       (dictree--merge
	(list (cons key newdata)) completions
	(dictree-rankfun dict)
	(when (dictree--meta-dict-p dict)
	  (dictree--meta-dict-combfun dict))
	maxnum)))
     ;; modified and in the cached result: update the associated data if
     ;; dict is a meta-dictionary (this is done automatically for a
     ;; normal dict), re-sort, and if key is now at end of list re-run
     ;; the same query to update the cache
     ((and (not deleted) cmpl)
      (when (dictree--meta-dict-p dict) (setcdr cmpl newdata))
      (dictree--cache-set-completions
       cache-entry (sort completions (dictree-rankfun dict)))
      (when (equal key (car (last completions)))
	(remhash (cons arg reverse) cache)
	(dictree-complete dict arg 'ranked maxnum reverse)))
     ;; deleted and not in cached result: requires no action
     )))


(defun dictree--synchronize-regexp-cache
  (dict cache-entry arg reverse key newdata deleted)
  ;; Synchronize DICT's completion CACHE-ENTRY for ARG and REVERSE, for
  ;; a KEY whose data was either updated to NEWDATA or DELETED.
  (let* ((completions (dictree--cache-results cache-entry))
	 (maxnum (dictree--cache-maxnum cache-entry))
	 group-data
	 (cmpl (catch 'found
		 (dolist (c completions)
		   (if (and (listp (car c))
			    (or (stringp (caar c))
				(vectorp (caar c))
				(listp (caar c))))
		       (when (equal key (caar c)) (throw 'found c))
		     (when (equal key (car c)) (throw 'found c)))))))
    ;; if key was...
    (cond
     ;; deleted and in cached result: remove cache entry and re-run the
     ;; same completion to update the cache
     ((and deleted cmpl)
      (remhash (cons arg reverse) (dictree-complete-cache dict))
      (dictree-regexp-search dict arg nil maxnum reverse))
     ;; modified and not in cached result: merge it into the completion
     ;; list, retaining only the first maxnum
     ((and (not deleted) (not cmpl))
      (save-match-data
	(set-match-data nil)
	(tNFA-regexp-match arg key
			   :test (dictree--comparison-function dict))
	(when (setq group-data (nthcdr 2 (match-data)))
	  (setq key (cons key group-data))))
      (dictree--cache-set-completions
       cache-entry
       (dictree--merge
	(list (cons key newdata)) completions
	`(lambda (a b)
	   (,(trie-construct-sortfun (dictree-comparison-function dict))
	    ,(if group-data '(caar a) '(car a))
	    ,(if group-data '(caar b) '(car b))))
	(when (dictree--meta-dict-p dict)
	  (dictree--meta-dict-combfun dict))
	maxnum)))
     ;; modified and in the cached result: update the associated data if
     ;; dict is a meta-dictionary (this is done automatically for a
     ;; normal dict)
     ((and (not deleted) cmpl (dictree--meta-dict-p dict))
      (setcdr cmpl newdata))
     ;; deleted and not in cached result: requires no action
     )))



(defun dictree--synchronize-ranked-regexp-cache
  (dict cache-entry arg reverse key newdata deleted)
  ;; Synchronize DICT's ranked regexp CACHE-ENTRY for ARG and REVERSE,
  ;; for a KEY whose data was either updated to NEWDATA or DELETED.
  (let ((completions (dictree--cache-results cache-entry))
	(maxnum (dictree--cache-maxnum cache-entry))
	(cache (dictree--regexp-ranked-cache dict))
	cmpl group-data)
    (setq group-data (and (listp (caar completions))
			  (or (stringp (caar (car completions)))
			      (vectorp (caar (car completions)))
			      (listp (caar (car completions))))))
    (setq cmpl
	  (catch 'found
	    (dolist (c completions)
	      (if group-data
		  (when (equal key (caar c)) (throw 'found c))
		(when (equal key (car c)) (throw 'found c))))))
    ;; if key was...
    (cond
     ;; deleted and in cached result: remove cache entry and re-run the
     ;; same query to update the cache
     ((and deleted cmpl)
      (remhash (cons arg reverse) cache)
      (dictree-regexp-search dict arg 'ranked maxnum reverse))
     ;; modified and not in cached result: merge it into the completion
     ;; list, retaining only the first maxnum
     ((and (not deleted) (not cmpl))
      (save-match-data
	(set-match-data nil)
	(tNFA-regexp-match arg key
			   :test (dictree--comparison-function dict))
	(when (setq group-data (nthcdr 2 (match-data)))
	  (setq key (cons key group-data))))
      (dictree--cache-set-completions
       cache-entry
       (dictree--merge
	(list (cons key newdata)) completions
	(dictree-rankfun dict)
	(when (dictree--meta-dict-p dict)
	  (dictree--meta-dict-combfun dict))
	maxnum)))
     ;; modified and in the cached result: update the associated data if
     ;; dict is a meta-dictionary (this is done automatically for a
     ;; normal dict), re-sort, and if key is now at end of list re-run
     ;; the same query to update the cache
     ((and (not deleted) cmpl)
      (when (dictree--meta-dict-p dict) (setcdr cmpl newdata))
      (dictree--cache-set-completions
       cache-entry
       (sort completions
	     (if group-data
		 `(lambda (a b)
		    (,(dictree-rankfun dict)
		     (cons (caar a) (cdr a))
		     (cons (caar b) (cdr b))))
	       (dictree-rankfun dict))))
      (when (equal key (car (last completions)))
	(remhash (cons arg reverse) cache)
	(dictree-complete dict arg 'ranked maxnum reverse)))
     ;; deleted and not in cached result: requires no action
     )))


(defun dictree-clear-caches (dict)
  "Clear all DICT's query caches."
  (interactive (list (read-dict "Dictionary: ")))
  (when (and (called-interactively-p 'any) (symbolp dict))
    (setq dict (symbol-value dict)))
  (dolist (cachefun '(dictree-lookup-cache
		      dictree-complete-cache
		      dictree-complete-ranked-cache
		      dictree-regexp-cache
		      dictree-regexp-ranked-cache))
    (when (funcall cachefun dict)
      (clrhash (funcall cachefun dict))))
  (when (called-interactively-p 'interactive)
    (message "Cleared caches for dictionary %s" (dictree-name dict))))




;; ----------------------------------------------------------------
;;                        Retrieving data

(defun dictree-member (dict key &optional nilflag)
  "Return the data associated with KEY in dictionary DICT,
or nil if KEY is not in the dictionary.

Optional argument NILFLAG specifies a value to return instead of
nil if KEY does not exist in TREE. This allows a non-existent KEY
to be distinguished from an element with a null association. (See
also `dictree-member-p' for testing existence alone.)"
  (let* ((data (dictree--lookup dict key nilflag)))
    (if (eq data nilflag)
	nilflag
      (dictree--cell-data data))))

(defalias 'dictree-lookup 'dictree-member)

(defun dictree-member-p (dict key)
  "Return t if KEY exists in DICT, nil otherwise."
  (let ((flag '(nil)))
    (not (eq flag (dictree-member dict key flag)))))


(defun dictree--lookup (dict key nilflag)
  ;; Return association of KEY in DICT, or NILFLAG if KEY does not
  ;; exist. Does not do any data/meta-data unwrapping

  (let* ((flag '(nil))
	 (data flag)
	 time)
    ;; if KEY is in the cache, then we're done
    (unless (and (dictree-lookup-cache dict)
		 (setq data (gethash key (dictree--lookup-cache dict))))

      ;; otherwise, we have to look in the dictionary itself...
      (cond
       ;; if DICT is a meta-dict, look in its constituent dictionaries
       ((dictree--meta-dict-p dict)
	(let (newdata (newflag '(nil)))
	  ;; time the lookup for caching
	  (setq time (float-time))
	  ;; look in each constituent dictionary in turn
	  (dolist (dic (dictree--meta-dict-dictlist dict))
	    (setq newdata (dictree--lookup dic key newflag))
	    ;; skip dictionary if it doesn't contain KEY
	    (unless (eq newdata newflag)
	      ;; if we haven't found KEY before, we have now!
	      (if (eq data flag) (setq data newdata)
		;; otherwise, combine the previous data with the new
		;; data
		(setq data (funcall (dictree--meta-dict-combfun dict)
				    data newdata)))))
	  (setq time (- (float-time) time))))

       ;; otherwise, DICT is a normal dictionary, so look in it's trie
       (t
	;; time the lookup for caching
	(setq time (float-time))
	(setq data (trie-member (dictree--trie dict) key flag))
	(setq time (- (float-time) time))))

      ;; if lookup found something, and we're above the lookup
      ;; cache-threshold, cache the result
      (when (and (not (eq data flag))
		 (dictree--above-cache-threshold-p
		  time (length key) (dictree-cache-policy dict)
		  (dictree-lookup-cache-threshold dict) 'long-keys))
	(setf (dictree-modified dict) t)
	(puthash key data (dictree-lookup-cache dict))))

    ;; return the desired data
    (if (eq data flag) nilflag data)))



;; ----------------------------------------------------------------
;;                 Getting and setting meta-data

(defun dictree-put-property (dict key property value)
  "Set PROPERTY for KEY in dictionary DICT.
PROPERTY should be a symbol. Returns VALUE if successful, nil if
KEY was not found in DICT.

Note that if DICT is a meta-dictionary, then this will set KEY's
PROPERTY to VALUE in *all* its constituent dictionaries.

Unlike the data associated with a key (cf. `dictree-insert'),
properties are not included in the results of queries on the
dictionary \(`dictree-lookup', `dictree-complete',
`dictree-complete-ordered'\), nor do they affect the outcome of
any of the queries. They merely serves to tag a key with some
additional information, and can only be retrieved using
`dictree-get-property'."

  ;; sort out arguments
  (and (symbolp dict) (setq dict (symbol-value dict)))
  (cond
   ;; set PROPERTY for KEY in all constituent dicts of a meta-dict
   ((dictree--meta-dict-p dict)
    (warn "Setting %s property for key %s in all constituent\
 dictionaries of meta-dicttionary %s" property key (dictree-name dict))
    (setf (dictree-modified dict) t)
    (let (dictree--put-property-ret)
      (mapc (lambda (dic k p v)
	      (setq dictree--put-property-ret
		    (or dictree--put-property-ret
			(dictree-put-property dic k p v))))
	    (dictree--meta-dict-dictlist dict))
      ;; return VALUE if KEY was found in at least one constituent dict
      dictree--put-property-ret))
   (t  ;; set PROPERTY for KEY in normal dict
    (let ((cell (trie-member (dictree--trie dict) key)))
      (when cell
	(setf (dictree-modified dict) t)
	(setf (dictree--cell-plist cell)
	      (plist-put (dictree--cell-plist cell) property value))
	value)))  ; return VALUE
   ))



(defun dictree-delete-property (dict key property)
  "Delete PROPERTY from KEY in dictionary DICT.
Returns the new property list for KEY, with PROPERTY deleted.

Setting PROPERTY to nil using `dictree-put-property' is not quite
the same thing as deleting it, since null property values can
still be detected by supplying the optional argument to
`dictree-get-propery' (which see).

Note that if DICT is a meta-dictionary, then this will delete
KEY's PROPERTY in *all* its constituent dictionaries."
  ;; sort out arguments
  (and (symbolp dict) (setq dict (symbol-value dict)))
  (cond
   ;; delete PROPERTY from KEY in all constituent dicts of a meta-dict
   ((dictree--meta-dict-p dict)
    (warn "Deleting %s property from key %s in all constituent\
 dictionaries of meta-dicttionary %s" property key (dictree-name dict))
    (setf (dictree-modified dict) t)
    (mapcar (lambda (dic k p) (dictree-delete-property dic k p))
	    (dictree--meta-dict-dictlist dict)))
   (t  ;; delete PROPERTY from KEY in normal dict
    (let* ((cell (trie-member (dictree--trie dict) key))
	   plist tail)
      (when (and cell
		 (setq tail
		       (plist-member
			(setq plist (dictree--cell-plist cell))
			property)))
	(setf (dictree-modified dict) t)
	;; delete property and value from plist
	(setcdr tail (cddr tail))
	(setq plist (delq property plist))
	(setf (dictree--cell-plist cell) plist))))
   ))



(defun dictree-get-property (dict key property &optional nilflag)
  "Get the value of PROPERTY for KEY in dictionary DICT,
or return nil if KEY is not in the dictionary.

Optional argument NILFLAG specifies a value to return instead of
nil if KEY does not exist in TREE. This allows a non-existent KEY
to be distinguished from a key for which PROPERTY is not
set. (See also `dictree-member-p' for testing existence alone.)"
  (let ((cell (dictree--lookup dict key nilflag)))
    (unless (eq cell nilflag)
      (plist-get (dictree--cell-plist cell) property))))




;; ----------------------------------------------------------------
;;                        Mapping functions

(defun dictree-mapc (function dict &optional type reverse)
  "Apply FUNCTION to all entries in dictionary DICT,
for side-effects only.

FUNCTION will be passed two arguments: a key of type
TYPE ('string, 'vector, or 'list, defaulting to 'vector) from the
dictionary, and the data associated with that key. The dictionary
entries will be traversed in \"lexical\" order, i.e. the order
defined by the dictionary's comparison function (cf.
`dictree-create').

If TYPE is 'string, it must be possible to apply the function
`string' to the elements of sequences stored in DICT.

FUNCTION is applied in ascending order, or descending order if
REVERSE is non-nil.

Note: to avoid nasty dynamic scoping bugs, FUNCTION must *not*
bind any variables with names commencing \"--\"."

  ;; "rename" FUNCTION to something hopefully unique to lessen the
  ;; likelihood of dynamic scoping bugs caused by a supplied function
  ;; binding a variable with the same name as one of the arguments
  (let ((--dictree-mapc--function function))
    (dictree--mapc
     (lambda (key data _plist)
       (funcall --dictree-mapc--function key data))
     dict type reverse)))



(defun dictree--mapc (function dict &optional type reverse)
  ;; Like `dictree-mapc', but FUNCTION is passed three arguments: the
  ;; key, the data, and the property list, instead of just key and data.

  ;; try to avoid dynamic binding bugs
  (let ((--dictree--mapc--function function))
    (if (dictree--meta-dict-p dict)
	;; for a meta-dict, use a dictree-stack
	(let ((stack (dictree-stack dict))
	      entry)
	  (while (setq entry (dictree--stack-pop stack))
	    (funcall --dictree--mapc--function
		     (car entry)
		     (dictree--cell-data (cdr entry))
		     (dictree--cell-plist (cdr entry)))))
      ;; for a normal dictionary, map the function over its trie
      (trie-mapc
       (lambda (key cell)
	 (funcall --dictree--mapc--function
		  key
		  (dictree--cell-data cell)
		  (dictree--cell-plist cell)))
       (dictree--trie dict)
       type reverse)
      )))



(defun dictree-mapf (function combinator dict &optional type reverse)
  "Apply FUNCTION to all entries in dictionary DICT,
and combine the results using COMBINATOR.

FUNCTION should take two arguments: a key sequence from the
dictionary and its associated data.

Optional argument TYPE (one of the symbols vector, lisp or
string; defaults to vector) sets the type of sequence passed to
FUNCTION. If TYPE is 'string, it must be possible to apply the
function `string' to the individual elements of key sequences
stored in DICT.

The FUNCTION will be applied and the results combined in
asscending \"lexical\" order (i.e. the order defined by the
dictionary's comparison function; cf. `dictree-create'), or
descending order if REVERSE is non-nil.

Note: to avoid nasty dynamic scoping bugs, FUNCTION and
COMBINATOR must *not* bind any variables with names
commencing \"--\"."

  ;; try to avoid dynamic scoping bugs
  (let ((--dictree-mapf--function function)
	(--dictree-mapf--combinator combinator))

    ;; for a normal dictionary, map the function over its trie
    (if (not (dictree--meta-dict-p dict))
	(trie-mapf
	 `(lambda (key data)
	    (,--dictree-mapf--function key (dictree--cell-data data)))
	 --dictree-mapf--combinator (dictree--trie dict) type reverse)

      ;; for a meta-dict, use a dictree-stack
      (let ((--dictree-mapf--stack (dictree-stack dict))
	    --dictree-mapf--entry
	    --dictree-mapf--accumulate)
	(while (setq --dictree-mapf--entry
		     (dictree-stack-pop --dictree-mapf--stack))
	  (setq --dictree-mapf--accumulate
		(funcall --dictree-mapf--combinator
			 (funcall --dictree-mapf--function
				  (car --dictree-mapf--entry)
				  (cdr --dictree-mapf--entry))
			 --dictree-mapf--accumulate)))
	--dictree-mapf--accumulate))))



(defun dictree-mapcar (function dict &optional type reverse)
  "Apply FUNCTION to all entries in dictionary DICT,
and make a list of the results.

FUNCTION should take two arguments: a key sequence from the
dictionary and its associated data.

Optional argument TYPE (one of the symbols vector, lisp or
string; defaults to vector) sets the type of sequence passed to
FUNCTION. If TYPE is 'string, it must be possible to apply the
function `string' to the individual elements of key sequences
stored in DICT.

The FUNCTION will be applied and the results combined in
asscending \"lexical\" order \(i.e. the order defined by the
dictionary's comparison function; cf. `dictree-create'\), or
descending order if REVERSE is non-nil.

Note that if you don't care about the order in which FUNCTION is
applied, just that the resulting list is in the correct order,
then

  (trie-mapf function 'cons trie type (not reverse))

is more efficient.

Note: to avoid nasty dynamic scoping bugs, FUNCTION must *not*
bind any variables with names commencing \"--\"."
  (nreverse (dictree-mapf function 'cons dict type reverse)))



(defun dictree-size (dict)
  "Return the number of entries in dictionary DICT.
Interactively, DICT is read from the mini-buffer."
  (interactive (list (read-dict "Dictionary: ")))
  (when (and (called-interactively-p 'any) (symbolp dict))
    (setq dict (symbol-value dict)))
  (let ((count 0))
    (dictree-mapc (lambda (&rest _dummy) (incf count)) dict)
    (when (called-interactively-p 'interactive)
      (message "Dictionary %s contains %d entries"
	       (dictree--name dict) count))
    count))



;; ----------------------------------------------------------------
;;                        Using dictrees as stacks

;; A dictree--meta-stack is the meta-dict version of a dictree-stack
;; (the ordinary version is just a single trie-stack). It consists of a
;; heap of trie-stacks for its constituent tries, where the heap order
;; is the usual lexical order over the keys at the top of the
;; trie-stacks.

(defstruct
  (dictree--meta-stack
   (:constructor nil)
   (:constructor dictree--meta-stack-create
		 (dict &optional (type 'vector) reverse
		  &aux
		  (combfun (dictree--meta-dict-combfun dict))
		  (sortfun (trie-construct-sortfun
			    (dictree-comparison-function dict)))
		  (heap (heap-create
			 (dictree--construct-meta-stack-heapfun sortfun)
			 (length (dictree--trielist dict))))
		  (pushed '())
		  (_dummy (mapc
			   (lambda (dic)
			     (heap-add
			      heap (trie-stack dic type reverse)))
			   (dictree--trielist dict)))))
   (:constructor dictree--complete-meta-stack-create
		 (dict prefix &optional reverse
		  &aux
		  (combfun (dictree--meta-dict-combfun dict))
		  (sortfun (trie-construct-sortfun
			    (dictree-comparison-function dict)))
		  (heap (heap-create
			 (dictree--construct-meta-stack-heapfun
			  sortfun reverse)
			 (length (dictree--trielist dict))))
		  (pushed '())
		  (_dummy (mapc
			   (lambda (trie)
			     (let ((stack (trie-complete-stack
					   trie prefix reverse)))
			       (unless (trie-stack-empty-p stack)
				 (heap-add heap stack))))
			   (dictree--trielist dict)))))
      (:constructor dictree--regexp-meta-stack-create
		 (dict regexp &optional reverse
		  &aux
		  (combfun (dictree--meta-dict-combfun dict))
		  (sortfun (trie-construct-sortfun
			    (dictree-comparison-function dict)))
		  (heap (heap-create
			 (dictree--construct-meta-stack-heapfun
			  sortfun reverse)
			 (length (dictree--trielist dict))))
		  (pushed '())
		  (_dummy (mapc
			   (lambda (trie)
			     (let ((stack (trie-regexp-stack
					   trie regexp reverse)))
			       (unless (trie-stack-empty-p stack)
				 (heap-add heap stack))))
			   (dictree--trielist dict)))))
   (:copier nil))
  combfun sortfun heap pushed)



(defun dictree--construct-meta-stack-heapfun (sortfun &optional reverse)
  ;; Wrap SORTFUN, which sorts keys, so it can act on
  ;; dictree--meta-stack elements.
  (if reverse
      `(lambda (b a) (,sortfun (car (dictree-stack-first a))
			       (car (dictree-stack-first b))))
    `(lambda (a b) (,sortfun (car (dictree-stack-first a))
			     (car (dictree-stack-first b))))))


(defun dictree-stack (dict &optional type reverse)
  "Create an object that allows DICT to be accessed as a stack.

The stack is sorted in \"lexical\" order, i.e. the order defined
by the DICT's comparison function, or in reverse order if REVERSE
is non-nil. Calling `dictree-stack-pop' pops the top element (a
key and its associated data) from the stack.

Optional argument TYPE (one of the symbols vector, lisp or
string) sets the type of sequence used for the keys.

Note that any modification to DICT *immediately* invalidates all
dictree-stacks created before the modification (in particular,
calling `dictree-stack-pop' will give unpredictable results).

Operations on dictree-stacks are significantly more efficient
than constructing a real stack from the dictionary and using
standard stack functions. As such, they can be useful in
implementing efficient algorithms on dictionaries. However, in
cases where mapping functions `dictree-mapc', `dictree-mapcar' or
`dictree-mapf' would be sufficient, it is better to use one of
those instead."
  (if (dictree--meta-dict-p dict)
      (dictree--meta-stack-create dict type reverse)
    (trie-stack (dictree--trie dict) type reverse)))


(defun dictree-complete-stack (dict prefix &optional reverse)
  "Return an object that allows completions of PREFIX to be accessed
as if they were a stack.

The stack is sorted in \"lexical\" order, i.e. the order defined
by DICT's comparison function, or in reverse order if REVERSE is
non-nil. Calling `dictree-stack-pop' pops the top element (a key
and its associated data) from the stack.

PREFIX must be a sequence (vector, list or string) that forms the
initial part of a TRIE key. (If PREFIX is a string, it must be
possible to apply `string' to individual elements of TRIE keys.)
The completions returned in the alist will be sequences of the
same type as KEY. If PREFIX is a list of sequences, completions
of all sequences in the list are included in the stack. All
sequences in the list must be of the same type.

Note that any modification to DICT *immediately* invalidates all
trie-stacks created before the modification (in particular,
calling `dictree-stack-pop' will give unpredictable results).

Operations on dictree-stacks are significantly more efficient
than constructing a real stack from completions of PREFIX in DICT
and using standard stack functions. As such, they can be useful
in implementing efficient algorithms on tries. However, in cases
where `dictree-complete' or `dictree-complete-ordered' is
sufficient, it is better to use one of those instead."
  (if (dictree--meta-dict-p dict)
      (dictree--complete-meta-stack-create dict prefix reverse)
    (trie-complete-stack (dictree--trie dict) prefix reverse)))


(defun dictree-regexp-stack (dict regexp &optional reverse)
  "Return an object that allows REGEXP matches to be accessed
as if they were a stack.

The stack is sorted in \"lexical\" order, i.e. the order defined
by DICT's comparison function, or in reverse order if REVERSE is
non-nil. Calling `dictree-stack-pop' pops the top element (a key
and its associated data) from the stack.

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
each match is no longer just a key. Instead, it is a list whose
first element is the matching key, and whose remaining elements
are cons cells whose cars and cdrs give the start and end indices
of the elements that matched the corresponding groups, in order.

Note that any modification to DICT *immediately* invalidates all
trie-stacks created before the modification (in particular,
calling `dictree-stack-pop' will give unpredictable results).

Operations on dictree-stacks are significantly more efficient
than constructing a real stack from completions of PREFIX in DICT
and using standard stack functions. As such, they can be useful
in implementing efficient algorithms on tries. However, in cases
where `dictree-complete' or `dictree-complete-ordered' is
sufficient, it is better to use one of those instead."
  (if (dictree--meta-dict-p dict)
      (dictree--regexp-meta-stack-create dict regexp reverse)
    (trie-regexp-stack (dictree--trie dict) regexp reverse)))


(defun dictree-stack-pop (dictree-stack)
  "Pop the first element from the DICTREE-STACK.
Returns nil if the stack is empty."
  (cond
   ;; if elements have been pushed onto a dict stack, pop those first
   ;; FIXME: shouldn't be using internal trie functions!
   ((and (trie-stack-p dictree-stack)
	 (trie--stack-pushed dictree-stack))
    (trie-stack-pop dictree-stack))
   ;; if elements have been pushed onto a meta-dict stack, pop those
   ;; first
   ((and (dictree--meta-stack-p dictree-stack)
	 (dictree--meta-stack-pushed dictree-stack))
    (pop (dictree--meta-stack-pushed dictree-stack)))
   ;; otherwise, pop first element from dictree-stack
   (t (let ((popped (dictree--stack-pop dictree-stack)))
	(when popped
	  (cons (car popped) (dictree--cell-data (cdr popped))))))
   ))


(defun dictree-stack-push (element dictree-stack)
  "Push ELEMENT onto DICTREE-STACK."
  (if (trie-stack-p dictree-stack)
      ;; normal dict
      (trie-stack-push element dictree-stack)
    ;; meta-dict
    (push element (dictree--meta-stack-pushed dictree-stack))))


(defun dictree-stack-first (dictree-stack)
  "Return the first element from DICTREE-STACK, without removing it.
Returns nil if the stack is empty."
  ;; if elements have been pushed onto the stack, return first of those
  (if (and (dictree--meta-stack-p dictree-stack)
	   (dictree--meta-stack-pushed dictree-stack))
      (car (dictree--meta-stack-pushed dictree-stack))
    ;; otherwise, return first element from dictree-stack
    (let ((first (dictree--stack-first dictree-stack)))
      (cons (car first) (dictree--cell-data (cdr first))))))


(defun dictree-stack-empty-p (dictree-stack)
  "Return t if DICTREE-STACK is empty, nil otherwise."
  (if (trie-stack-p dictree-stack)
      ;; normal dict
      (trie-stack-empty-p dictree-stack)
    ;; meta-dict
    (and (heap-empty (dictree--meta-stack-heap dictree-stack))
	 (null (dictree--meta-stack-pushed dictree-stack)))))


(defun dictree--stack-first (dictree-stack)
  "Return the first element from DICTREE-STACK, without removing it.
Returns nil if the stack is empty."
  (if (trie-stack-p dictree-stack)
      ;; normal dict
      (trie-stack-first dictree-stack)
    ;; meta-dict
    (if (dictree--meta-stack-pushed dictree-stack)
	;; pushed element
	(car (dictree--meta-stack-pushed dictree-stack))
      ;; dictree-stack element
      (dictree--stack-first
       (heap-root (dictree--meta-stack-heap dictree-stack))))))


(defun dictree--stack-pop (dictree-stack)
  ;; Pop the raw first element from DICTREE-STACK. Returns nil if the
  ;; stack is empty.

  ;; dictree-stack for normal dictionaries is a trie-stack
  (if (trie-stack-p dictree-stack)
      (trie-stack-pop dictree-stack)

    ;; meta-dictionary dictree-stack...more work!
    ;; if elements have been pushed onto meta-dict stack, pop those
    ;; first
    (if (dictree--meta-stack-pushed dictree-stack)
	(pop (dictree--meta-stack-pushed dictree-stack))
      ;; otherwise...
      (let ((heap (dictree--meta-stack-heap dictree-stack))
	    (sortfun (dictree--meta-stack-sortfun dictree-stack))
	    stack curr next)
	(unless (heap-empty heap)
	  ;; remove the first dictree-stack from the heap, pop it's
	  ;; first element, and add it back to the heap (note that it
	  ;; will almost certainly not end up at the root again)
	  (setq stack (heap-delete-root heap))
	  (setq curr (dictree--stack-pop stack))
	  (unless (dictree-stack-empty-p stack) (heap-add heap stack))
	  ;; peek at the first element of the stack now at the root of
	  ;; the heap
	  (unless (heap-empty heap)
	    (setq next (dictree--stack-first (heap-root heap)))
	    ;; repeat this as long as we keep finding elements with the
	    ;; same key, combining them together as we go
	    (when (dictree--meta-stack-combfun dictree-stack)
	      (while (and (null (funcall sortfun
					 (car curr) (car next)))
			  (null (funcall sortfun
					 (car next) (car curr))))
		(setq stack (heap-delete-root heap))
		(setq next (dictree--stack-pop stack))
		(setq curr
		      (cons
		       (car curr)
		       (dictree--cell-create
			(funcall
			 (dictree--meta-stack-combfun dictree-stack)
			 (dictree--cell-data (cdr curr))
			 (dictree--cell-data (cdr next)))
			(append (dictree--cell-plist (cdr curr))
				(dictree--cell-plist (cdr next))))))
		(heap-add heap stack)
		(setq next (dictree--stack-first (heap-root heap))))))
	  ;; return the combined dictionary element
	  curr)))))




;; ----------------------------------------------------------------
;;             Functions for building advanced queries

(defun dictree--query
  (dict arg cachefun cacheparamfun triefun stackfun
   &optional rank-function maxnum reverse no-cache filter resultfun)
  ;; Return results of querying DICT with argument ARG using TRIEFUN or
  ;; STACKFUN. If result of calling CACHEPARAMFUN on DICT is non-nil,
  ;; look first for cached result in cache returned by calling CACHEFUN
  ;; on DICT, and cache result if query fulfils caching conditions. If
  ;; RANK-FUNCTION is non-nil, return results ordered accordingly. If
  ;; MAXNUM is an integer, only the first MAXNUM results will be
  ;; returned. If REVERSE is non-nil, results are in reverse order. A
  ;; non-nil NO-CACHE prevents caching of results, irrespective of
  ;; DICT's cache settings. If supplied, only results that pass FILTER
  ;; are included. A non-nil RESULTFUN is applied to results before
  ;; adding them to final results list. Otherwise, an alist of key-data
  ;; associations is returned.

  ;; wrap DICT in a list if necessary
  (when (dictree-p dict) (setq dict (list dict)))

  (let (cache cacheparam completions cmpl cache-entry)
    ;; map over all dictionaries in list
    (dolist (dic dict)
      (setq cache (funcall cachefun dic)
	    cacheparam (funcall cacheparamfun dic))
      (cond
       ;; If FILTER or custom RANK-FUNCTION was specified, look in trie
       ;; since we don't cache custom searches. We pass a slightly
       ;; redefined filter to `trie-complete' to deal with data
       ;; wrapping.
       ((or filter
	    (and rank-function
		 (not (eq rank-function (dictree-rank-function dic)))))
	(setq cmpl
	      (dictree--do-query dic arg triefun stackfun
				 (dictree--wrap-rankfun rank-function)
				 maxnum reverse
				 (when filter
				   (dictree--wrap-filter filter)))))


       ;; if there's a cached result with enough completions, use it
       ((and (setq cache-entry
		   (if cacheparam
		       (gethash (cons arg reverse) cache)
		     nil))
	     (or (null (dictree--cache-maxnum cache-entry))
		 (and maxnum
		      (<= maxnum (dictree--cache-maxnum cache-entry)))))
	(setq cmpl (dictree--cache-results cache-entry))
	;; drop any excess completions
	(when (and maxnum
		   (or (null (dictree--cache-maxnum cache-entry))
		       (> (dictree--cache-maxnum cache-entry) maxnum)))
	  (setcdr (nthcdr (1- maxnum) completions) nil)))


       ;; if there was nothing useful in the cache, do query and time it
       (t
	(let (time)
	  (setq time (float-time))
	  (setq cmpl
		(dictree--do-query
		 dic arg triefun stackfun
		 (when rank-function
		   (dictree--wrap-rankfun rank-function))
		 maxnum reverse nil))
	  (setq time (- (float-time) time))
	  ;; if we're above the dictionary's completion cache threshold,
	  ;; cache the result
	  (when (and (not no-cache)
		     (dictree--above-cache-threshold-p
		      time (length arg) (dictree-cache-policy dic)
		      cacheparam))
	    (setf (dictree-modified dic) t)
	    (puthash (cons arg reverse)
		     (dictree--cache-create cmpl maxnum)
		     cache)))))

      ;; merge new completion into completions list
      (setq completions
	    (dictree--merge
	     completions cmpl
	     (if rank-function
		 (dictree--wrap-rankfun rank-function)
	       `(lambda (a b)
		  (,(trie-construct-sortfun
		     (dictree-comparison-function (car dict)))
		   (car a) (car b))))
	     nil maxnum)))

    ;; return completions list, applying RESULTFUN is specified,
    ;; otherwise just stripping meta-data
    (mapcar
     (if resultfun
	 (dictree--wrap-resultfun resultfun)
       (lambda (el) (cons (car el) (dictree--cell-data (cdr el)))))
     completions)))



(defun dictree--do-query
  (dict arg triefun stackfun &optional rank-function maxnum reverse filter)
  ;; Return first MAXNUM results of querying DICT with ARG using TRIEFUN
  ;; or STACKFUN that satisfy FILTER, ordered according to RANK-FUNCTION
  ;; (defaulting to "lexical" order).

  ;; for a meta-dict, use a dictree-stack
  (if (dictree--meta-dict-p dict)
      (let ((stack (funcall stackfun dict arg reverse))
	    (heap (when rank-function
		    (heap-create   ; heap order is inverse of rank order
			(if reverse
			    rank-function
			  (lambda (a b)
			    (not (funcall rank-function a b))))
			(1+ maxnum))))
	    (i 0) cmpl completions)
	;; pop MAXNUM completions from the stack
	(while (and (or (null maxnum) (< i maxnum))
		    (setq cmpl (dictree--stack-pop stack)))
	  ;; check completion passes FILTER
	  (when (or (null filter) (funcall filter cmpl))
	    (if rank-function
		(heap-add heap cmpl)   ; for ranked query, add to heap
	      (push cmpl completions)) ; for lexical query, add to list
	    (incf i)))
	(if (null rank-function)
	    ;; for lexical query, reverse and return completion list (we
	    ;; built it backwards)
	    (nreverse completions)
	  ;; for ranked query, pass rest of completions through heap
	  (while (setq cmpl (dictree--stack-pop stack))
	    (heap-add heap cmpl)
	    (heap-delete-root heap))
	  ;; extract completions from heap
	  (while (setq cmpl (heap-delete-root heap))
	    (push cmpl completions))
	  completions))  ; return completion list

    ;; for a normal dict, call corresponding trie function on dict's
    ;; trie. Note: could use a dictree-stack here too - would it be more
    ;; efficient?
    (funcall triefun
	     (dictree--trie dict) arg rank-function
	     maxnum reverse filter)))



;; ----------------------------------------------------------------
;;                        Completing

(defun dictree-complete
  (dict prefix
   &optional rank-function maxnum reverse no-cache filter resultfun)
  "Return an alist containing all completions of PREFIX in DICT
along with their associated data, sorted according to
RANK-FUNCTION (defaulting to \"lexical\" order, i.e. the order
defined by the dictionary's comparison function,
cf. `dictree-create'). Return nil if no completions are found.

PREFIX can also be a list of sequences, in which case completions of
all elements in the list are returned, merged together in a
single sorted alist.

DICT can also be a list of dictionaries, in which case
completions are sought in all dictionaries in the list. (Note
that if the same key appears in multiple dictionaries, the alist
may contain the same key multiple times, each copy associated
with the data from a different dictionary. If you want to combine
identical keys, use a meta-dictionary; see
`dictree-create-meta-dict'.)

If optional argument RANK-FUNCTION is any non-nil value that is
not a function, the completions are sorted according to the
dictionary's rank-function (see `dictree-create'). Any non-nil
value that *is* a function over-rides this. In that case,
RANK-FUNCTION should accept two arguments, both cons cells. The
car of each contains a sequence from the trie (of the same type
as PREFIX), the cdr contains its associated data. The
RANK-FUNCTION should return non-nil if first argument is ranked
strictly higher than the second, nil otherwise.

The optional integer argument MAXNUM limits the results to the
first MAXNUM completions. The default is to return all matches.

If the optional argument NO-CACHE is non-nil, it prevents caching
of the result. Ignored for dictionaries that do not have
completion caching enabled.

The FILTER argument sets a filter function for the
completions. For each potential completion, it is passed two
arguments: the completion, and its associated data. If the filter
function returns nil, the completion is not included in the
results, and doesn't count towards MAXNUM.

RESULTFUN defines a function used to process results before
adding them to the final result list. If specified, it should
accept two arguments: a key and its associated data. It's return
value is what gets added to the final result list, instead of the
default key-data cons cell."
  ;; run completion query
  (dictree--query
   dict prefix
   (if rank-function
       'dictree-complete-ranked-cache
     'dictree-complete-cache)
   (if rank-function
       'dictree-complete-ranked-cache-threshold
     'dictree-complete-cache-threshold)
   'trie-complete 'dictree-complete-stack
   (when rank-function
     (if (functionp rank-function)
	 rank-function
       (dictree-rank-function (if (listp dict) (car dict) dict))))
   maxnum reverse no-cache filter resultfun))



(defun dictree-collection-function (dict string predicate all)
  "Function for use in `try-completion', `all-completions',
and `completing-read'. To complete from dictionary DICT, use the
following as the COLLECTION argument of any of those functions:

  (lambda (string predicate all)
    (dictree-collection-function dict string predicate all))

Note that PREDICATE will be called with two arguments: the
completion, and its associated data."
  (let ((completions
	 (dictree-complete dict string nil nil nil nil
			   predicate (lambda (key _data) key))))
    (if all completions (try-completion "" completions))))



;; ----------------------------------------------------------------
;;                      Regexp search

(defun dictree-regexp-search
  (dict regexp
   &optional rank-function maxnum reverse no-cache filter resultfun)
  "Return an alist containing all matches for REGEXP in TRIE
along with their associated data, in the order defined by
RANKFUN, defauling to \"lexical\" order (i.e. the order defined
by the trie's comparison function).  If REVERSE is non-nil, the
completions are sorted in the reverse order. Returns nil if no
completions are found.

DICT can also be a list of dictionaries, in which case matches
are sought in all dictionaries in the list. (Note that if the
same key appears in multiple dictionaries, the alist may contain
the same key multiple times, each copy associated with the data
from a different dictionary. If you want to combine identical
keys, use a meta-dictionary; see `dictree-create-meta-dict'.)

REGEXP is a regular expression, but it need not necessarily be a
string. It must be a sequence (vector, list of string) whose
elements are either elements of the same type as elements of the
trie keys (which behave as literals in the regexp), or any of the
usual regexp special characters and backslash constructs. If
REGEXP is a string, it must be possible to apply `string' to
individual elements of the keys stored in the trie. The matches
returned in the alist will be sequences of the same type as KEY.

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

If optional argument RANK-FUNCTION is any non-nil value that is
not a function, the matches are sorted according to the
dictionary's rank-function (see `dictree-create'). Any non-nil
value that *is* a function over-rides this. In that case,
RANK-FUNCTION should accept two arguments, both cons cells. The
car of each contains a sequence from the dictionary (of the same
type as PREFIX), the cdr contains its associated data. The
RANK-FUNCTION should return non-nil if first argument is ranked
strictly higher than the second, nil otherwise.

The optional integer argument MAXNUM limits the results to the
first MAXNUM matches. The default is to return all matches.

If the optional argument NO-CACHE is non-nil, it prevents caching
of the result. Ignored for dictionaries that do not have wildcard
caching enabled.

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
  ;; run regexp query
  (dictree--query
   dict regexp
   (if rank-function
       'dictree-regexp-ranked-cache
     'dictree-regexp-cache)
   (if rank-function
       'dictree-regexp-ranked-cache-threshold
     'dictree-regexp-cache-threshold)
   'trie-regexp-search 'dictree-regexp-stack
   (when rank-function
     (if (functionp rank-function)
	 rank-function
       (dictree-rank-function (if (listp dict) (car dict) dict))))
   maxnum reverse no-cache filter resultfun))




;; ----------------------------------------------------------------
;;                    Persistent storage

(defun dictree-save (dict &optional compilation)
  "Save dictionary DICT to it's associated file.
Use `dictree-write' to save to a different file.

Optional argument COMPILATION determines whether to save the
dictionary in compiled or uncompiled form. The default is to save
both forms. See `dictree-write'.

Interactively, DICT is read from the mini-buffer."
  (interactive (list (read-dict "Dictionary: ")))
  (when (and (called-interactively-p 'any) (symbolp dict))
    (setq dict (symbol-value dict)))

  (let ((filename (dictree-filename dict)))
    ;; if dictionary has no associated file, prompt for one
    (unless (and filename (> (length filename) 0))
      (setq filename
	    (read-file-name
	     (format "Save dictionary %s to file\
 (leave blank to NOT save): "
		     (dictree-name dict))
	     nil "")))

    ;; if filename is blank, don't save
    (if (string= filename "")
	(message "Dictionary %s NOT saved" (dictree-name dict))
      ;; otherwise write dictionary to file
      (setf (dictree-filename dict) filename)
      (dictree-write dict filename t compilation))))



(defun dictree-write (dict &optional filename overwrite compilation)
  "Write dictionary DICT to file FILENAME.
Defaults to dictionary's current filename if FILENAME is not
specified (like `dictree-save').

If optional argument OVERWRITE is non-nil, no confirmation will
be asked for before overwriting an existing file.

The default is to create both compiled and uncompiled versions of
the dictionary, with extensions .elc and .el respectively (if
FILENAME has either of these extensions, they are stripped off
before proceeding). The compiled version is always used in
preference to the uncomplied version, as it loads
faster. However, only the uncompiled version is portable between
different Emacs versions.

If optional argument COMPILATION is the symbol 'compiled, only
the compiled version will be created, whereas if it is the symbol
'uncompiled, only the uncompiled version will be created.

Interactively, DICT and FILENAME are read from the mini-buffer,
and OVERWRITE is the prefix argument."
  (interactive (list (read-dict "Dictionary: ")
		     (read-file-name "Write dictionary to file: "
				     nil "")
		     current-prefix-arg))
  (when (and (called-interactively-p 'any) (symbolp dict))
    (setq dict (symbol-value dict)))
  ;; default to DICT's current file, if any
  (when (or (null filename)
	    (and (called-interactively-p 'any) (string= filename "")))
    (setq filename (dictree-filename dict)))
  (if (null filename)
      (progn
	(message "Dictionary %s NOT written" (dictree-name dict))
	nil)  ; indicate dictionary wasn't written

    (let (dictname buff tmpfile)
      ;; remove any .el(c) extension from filename
      (cond
       ((and (> (length filename) 3)
	     (string= (substring filename -3) ".el"))
	(setq filename (substring filename 0 -3)))
       ((and (> (length filename) 4)
	     (string= (substring filename -4) ".elc"))
	(setq filename (substring filename 0 -4))))
      ;; create saved dictionary name from filename
      (setq dictname (file-name-nondirectory filename))

      (save-excursion
	;; create a temporary file
	(setq buff
	      (find-file-noselect
	       (setq tmpfile (make-temp-file dictname))))
	(set-buffer buff)
	;; call the appropriate write function to write the dictionary code
	(if (dictree--meta-dict-p dict)
	    (dictree--write-meta-dict-code dict dictname filename)
	  (dictree--write-dict-code dict dictname filename))
	(save-buffer)
	(kill-buffer buff))

      ;; prompt to overwrite if necessary
      (when (or overwrite
		(and
		 (or (eq compilation 'compiled)
		     (not (file-exists-p (concat filename ".el"))))
		 (or (eq compilation 'uncompiled)
		     (not (file-exists-p (concat filename ".elc")))))
		(y-or-n-p
		 (format "File %s already exists. Overwrite? "
			 (concat filename ".el(c)"))))
	(condition-case nil
	    (progn
	      ;; move the uncompiled version to its final destination
	      (unless (eq compilation 'compiled)
		(copy-file tmpfile (concat filename ".el") t))
	      ;; byte-compile and move the compiled version to its final
	      ;; destination
	      (unless (eq compilation 'uncompiled)
		(if (save-window-excursion
		      (let ((byte-compile-disable-print-circle t)
			    err)
			(setq err (byte-compile-file tmpfile))
			err))
		    (rename-file (concat tmpfile ".elc")
				 (concat filename ".elc") t)
		  (error ""))))
	  (error "Error writing dictionary. Dictionary %s NOT saved"
		 dictname))

	;; if writing to a different name, unload dictionary under old
	;; name and reload it under new one
	(setf (dictree-modified dict) nil)
	(setf (dictree-filename dict) filename)
	(unless (string= dictname (dictree-name dict))
	  (dictree-unload dict)
	  (dictree-load filename)))

      (delete-file tmpfile)
      (message "Dictionary %s saved to %s" dictname filename)
      t)  ; return t to indicate dictionary was successfully saved
    ))



(defun dictree-save-modified (&optional dict ask compilation force
					no-fail-query)
  "Save all modified dictionaries that have their autosave flag set.
Returns t if all dictionaries were successfully saved. Otherwise,
inform the user about the dictionaries which failed to save
properly, ask them whether they wish to continue anyway, and
return t or nil accordingly.

If optional argument DICT is a list of dictionaries or a single
dictionary, only save those.

If optional argument ASK is non-nil, ask for confirmation before
saving.

Optional argument COMPILATION determines whether to save the
dictionaries in compiled or uncompiled form. The default is to
save both forms. See `dictree-write'.

If optional argument FORCE is non-nil, save modified dictionaries
irrespective of their autosave flag.

If optional argument NO-FAIL-QUERY is non-nil, the user will not
be queried if a dictionary fails to save properly, and the return
value is always nil.

Interactively, FORCE is the prefix argument, and the user will not be
asked whether they wish to continue after a failed save."
  (interactive "P")

  ;; sort out arguments
  (when (and (called-interactively-p 'any) dict) (setq dict nil force t))
  (when (dictree-p dict) (setq dict (list dict)))

  ;; For each dictionary in list / each loaded dictionary, check if
  ;; dictionary has been modified. If so, save it if autosave is set or
  ;; FORCE is non-nil.
  (let (save-failures)
    (dolist (dic (if (null dict)
		     dictree-loaded-list
		   dict))
      (when (and (dictree-modified dic)
		 (or force (dictree-autosave dic))
		 (or (not ask)
		     (y-or-n-p (format "Save modified dictionary %s? "
				       (dictree-filename dic)))))
	(condition-case nil
	    (progn
	      (dictree-save dic compilation)
	      (setf (dictree-modified dic) nil))
	  (error (push dic save-failures)))))

    ;; prompt if dictionary saving failed
    (if save-failures
	(if (or (called-interactively-p 'any) no-fail-query)
	    (progn
	      (message
	       (concat
		"Error: failed to save the following modified "
		"dictionaries: "
		(mapconcat 'dictree--name save-failures ", ")))
	      nil)
	  (yes-or-no-p
	   (concat "Error: failed to save the following modified "
		   "dictionaries: "
		   (mapconcat 'dictree--name save-failures ", ")
		   "; continue anyway? ")))
      t)))


;; Add the dictree-save-modified function to the kill-emacs-hook to save
;; modified dictionaries when exiting emacs
(add-hook 'kill-emacs-query-functions 'dictree-save-modified)



;;;###autoload
(defun dictree-load (file)
  "Load a dictionary object from file FILE.
Returns the dictionary if successful, nil otherwise.

Interactively, FILE is read from the mini-buffer."
  (interactive (list (read-dict "Load dictionary: " nil nil t t)))

  ;; sort out dictionary name and file name
  (if (or (symbolp file) (dictree-p file))
      (message "Dictionary %s already loaded" (dictree-name file))

    ;; load the dictionary
    (if (not (load file t))
	;; if loading failed, throw error interactively, return nil
	;; non-interactively
	(if (called-interactively-p 'any)
	    (error "Cannot open dictionary file: %s" file)
	  nil)

      (let (dictname dict)
	(setq dictname
	      (file-name-nondirectory (file-name-sans-extension file))
	      dict (symbol-value (intern-soft dictname)))
	(if (not (dictree-p dict))
	    ;; if loading failed, throw error interactively, return nil
	    ;; non-interactively
	    (if (called-interactively-p 'any)
		(error "Error loading dictionary file: %s" file)
	      nil)

	  ;; ensure the dictionary name and file name associated with
	  ;; the dictionary match the file it was loaded from
	  (when (and (string= (file-name-nondirectory file) file)
		     (setq file
			   (locate-file file load-path load-suffixes)))
	    (setf (dictree-filename dict) file))
	  (setf (dictree-name dict) dictname)

	  ;; make sure the dictionary is in dictree-loaded-list
	  ;; (normally the lisp code in the dictionary itself should do
	  ;; this, but just to make sure...)
	  (unless (memq dict dictree-loaded-list)
	    (push dict dictree-loaded-list))
	  (message (format "Loaded dictionary %s" dictname))

	  ;; return dictionary
	  dict)))))



(defun dictree-unload (dict &optional dont-save)
  "Unload dictionary DICT.
If optional argument DONT-SAVE is non-nil, the dictionary will
NOT be saved even if its autosave flag is set.

Interactively, DICT is read from the mini-buffer, and DONT-SAVE
is the prefix argument."
  (interactive (list (read-dict "Dictionary: ")
		     current-prefix-arg))
  (when (and (called-interactively-p 'any) (symbolp dict))
    (setq dict (symbol-value dict)))

  ;; if dictionary has been modified, autosave is set and not overidden,
  ;; save it first
  (when (and (dictree-modified dict)
	     (null dont-save)
	     (or (eq (dictree-autosave dict) t)
		 (and (eq (dictree-autosave dict) 'ask)
		      (y-or-n-p
		       (format
			"Dictionary %s modified.\
 Save before unloading? "
			(dictree-name dict))))))
    (dictree-save dict))

  ;; if unloading a meta-dict, remove reference to it from constituent
  ;; dictionaries' meta-dict-list cell
  (when (dictree--meta-dict-p dict)
    (mapc
     (lambda (dic)
       (setf (dictree--meta-dict-list dic)
	     (delq dict (dictree--meta-dict-list dic))))
     (dictree--meta-dict-dictlist dict)))

  ;; remove dictionary from list of loaded dictionaries and unload it
  (setq dictree-loaded-list (delq dict dictree-loaded-list))
  (unintern (dictree-name dict))
  (message "Dictionary %s unloaded" (dictree-name dict)))



(defun dictree--write-dict-code (dict dictname filename)
  ;; Write code for normal dictionary DICT to current buffer, giving it
  ;; the name DICTNAME and file FILENAME.
  (let (hashcode tmpdict tmptrie lookup-alist
	complete-alist complete-ranked-alist
	regexp-alist regexp-ranked-alist)

    ;; --- convert trie data ---
    ;; if dictionary doesn't use any custom save functions, write
    ;; dictionary's trie directly as is
    (setq tmptrie (dictree--trie dict))
    ;; otherwise, create a temporary trie and populate it with the
    ;; converted contents of the dictionary's trie
    (when (or (dictree--data-savefun dict)
	      (dictree--plist-savefun dict))
      (setq tmptrie
	    (trie-create-custom
	     (trie-comparison-function tmptrie)
	     :createfun (trie--createfun tmptrie)
	     :insertfun (trie--insertfun tmptrie)
	     :deletefun (trie--deletefun tmptrie)
	     :lookupfun (trie--lookupfun tmptrie)
	     :mapfun (trie--mapfun tmptrie)
	     :emptyfun (trie--emptyfun tmptrie)
	     :stack-createfun (trie--stack-createfun tmptrie)
	     :stack-popfun (trie--stack-popfun tmptrie)
	     :stack-emptyfun (trie--stack-emptyfun tmptrie)))
      (trie-mapc
       (lambda (key cell)
	 (trie-insert tmptrie key
		      (dictree--cell-create
		       (funcall (or (dictree--data-savefun dict)
				    'identity)
				(dictree--cell-data cell))
		       (funcall (or (dictree--plist-savefun dict)
				    'identity)
				(dictree--cell-plist cell)))))
       (dictree--trie dict))

      ;; generate code to convert contents of trie back to original form
      (setq hashcode
	    (concat
	     hashcode
	     " (trie-map\n"
	     "  (lambda (key cell)\n"
	     "     (dictree--cell-create\n"
	     (if (dictree--data-loadfun dict)
		 (concat
		  "(funcall (dictree--data-loadfun " dictname ")\n"
		  "         (dictree--cell-data cell))\n")
	       "   (dictree--cell-data cell)\n")
	     (if (dictree--plist-loadfun dict)
		 (concat
		  "(funcall (dictree--plist-loadfun " dictname ")\n"
		  "         (dictree--cell-plist cell))))\n")
	       "   (dictree--cell-plist cell)))\n")
	     " (dictree--trie " dictname "))\n")))


    ;; --- convert caches for writing to file ---
    ;; hash tables have no read syntax in older Emacsen, so we convert
    ;; them to alists for writing
    (unless (featurep 'hashtable-print-readable)
      ;; convert lookup cache hash table to alist, if it exists
      (when (dictree--lookup-cache-threshold dict)
	(maphash
	 (lambda (key val)
	   (push
	    (cons key
		  (cons (mapcar 'car (dictree--cache-results val))
			(dictree--cache-maxnum val)))
	    lookup-alist))
	 (dictree--lookup-cache dict))
	;; generate code to reconstruct the lookup hash table
	(setq hashcode
	      (concat
	       hashcode
	       "(let ((lookup-cache (make-hash-table :test 'equal))\n"
	       "      (trie (dictree--trie " dictname ")))\n"
	       "  (mapc\n"
	       "   (lambda (entry)\n"
	       "     (puthash\n"
	       "      (car entry)\n"
	       "      (dictree--cache-create\n"
	       "       (mapcar\n"
	       "        (lambda (key)\n"
	       "          (cons key (trie-member trie key)))\n"
	       "        (dictree--cache-results (cdr entry)))\n"
	       "       (dictree--cache-maxnum (cdr entry)))\n"
	       "      lookup-cache))\n"
	       "   (dictree--lookup-cache " dictname "))\n"
	       "  (setf (dictree--lookup-cache " dictname ")\n"
	       "        lookup-cache))\n")))

      ;; convert query caches, if they exist
      (dolist (cache-details
	       '((dictree--complete-cache-threshold
		  complete-alist dictree--complete-cache)
		 (dictree--complete-ranked-cache-threshold
		  complete-ranked-alist dictree--complete-ranked-cache)
		 (dictree--regexp-cache-threshold
		  regexp-alist dictree--regexp-cache)
		 (dictree--regexp-ranked-cache-threshold
		  regexp-ranked-alist dictree--regexp-ranked-cache)))
	(when (funcall (nth 0 cache-details) dict)
	  ;; convert hash table to alist
	  (set (nth 1 cache-details)
	       (let (alist)
		 (maphash
		  (lambda (key val)
		    (push
		     (cons key
			   (cons
			    (mapcar 'car (dictree--cache-results val))
			    (dictree--cache-maxnum val)))
		     alist))
		  (funcall (nth 2 cache-details) dict))
		 alist))
	  ;; generate code to reconstruct hash table from alist
	  (setq
	   hashcode
	   (concat
	    hashcode
	    "(let ((cache (make-hash-table :test 'equal))\n"
	    "      (trie (dictree--trie " dictname ")))\n"
	    "  (mapc\n"
	    "   (lambda (entry)\n"
	    "     (puthash\n"
	    "      (car entry)\n"
	    "      (dictree--cache-create\n"
	    "       (mapcar\n"
	    "        (lambda (key)\n"
	    "          (cons key\n"
	    "                (trie-member\n"
	    "                 trie (if (stringp key) key (car key)))))\n"
	    "        (dictree--cache-results (cdr entry)))\n"
	    "       (dictree--cache-maxnum (cdr entry)))\n"
	    "      cache))\n"
	    "   (" (symbol-name (nth 2 cache-details)) " " dictname "))\n"
	    "  (setf (" (symbol-name (nth 2 cache-details)) " "
	              dictname ")\n"
	    "        cache))\n")))))


    ;; --- write to file ---
    ;; generate the structure to save
    (setq tmpdict (dictree--copy dict))
    (setf (dictree--trie tmpdict) tmptrie
	  (dictree--name tmpdict) dictname
	  (dictree--filename tmpdict) filename
	  (dictree--modified tmpdict) nil
	  (dictree--meta-dict-list tmpdict) nil)
    (unless (featurep 'hashtable-print-readable)
      (setf (dictree--lookup-cache tmpdict) lookup-alist
	    (dictree--complete-cache tmpdict) complete-alist
	    (dictree--complete-ranked-cache tmpdict) complete-ranked-alist
	    (dictree--regexp-cache tmpdict) regexp-alist
	    (dictree--regexp-ranked-cache tmpdict) regexp-ranked-alist))

    ;; write lisp code that generates the dictionary object
    (let ((print-circle t) (print-level nil) (print-length nil))
      (insert "(eval-when-compile (require 'cl))\n")
      (insert "(require 'dict-tree)\n")
      (insert "(defvar " dictname " nil \"Dictionary " dictname ".\")\n")
      (unwind-protect
	  (progn
	    ;; transform trie to print form
	    (trie-transform-for-print (dictree--trie tmpdict))
	    (insert "(setq " dictname
		    " '" (prin1-to-string tmpdict) ")\n"))
	;; if dictionary doesn't use any custom save functions, tmpdict's trie
	;; is identical to original dict, so transform it back to usable form
	;; on write error
	(unless (or (dictree--data-savefun dict)
		    (dictree--plist-savefun dict))
	  (trie-transform-from-read (dictree--trie tmpdict))))
      (insert "(trie-transform-from-read (dictree--trie "
	      dictname "))\n")
      (when hashcode (insert hashcode))
      (insert "(unless (memq " dictname " dictree-loaded-list)\n"
	      "  (push " dictname " dictree-loaded-list))\n"))))



(defun dictree--write-meta-dict-code (dict dictname filename)
  ;; Write code for meta-dictionary DICT to current buffer, giving it
  ;; the name DICTNAME and file FILENAME.
  (let (hashcode tmpdict lookup-alist
	complete-alist complete-ranked-alist
	regexp-alist regexp-ranked-alist)

    ;; --- convert caches for writing to file ---
    ;; hash tables have no read syntax in older Emacsen, so we convert
    ;; them to alists for writing
    (unless (featurep 'hashtable-print-readable)
      ;; convert lookup cache hash table to an alist, if it exists
      (when (dictree--meta-dict-lookup-cache-threshold dict)
	(maphash (lambda (key val)
		   (push (cons key (mapcar 'car val)) lookup-alist))
		 (dictree--meta-dict-lookup-cache dict))
	;; generate code to reconstruct the lookup hash table
	(setq hashcode
	      (concat
	       hashcode
	       "(let ((cache (make-hash-table :test 'equal)))\n"
	       "  (mapc (lambda (entry)\n"
	       "    (puthash (car entry) (cdr entry) cache))\n"
	       "    (dictree--meta-dict-lookup-cache " dictname "))\n"
	       "  (setf (dictree--meta-dict-lookup-cache " dictname ")\n"
	       "        cache))\n")))

      ;; convert query caches, if they exist
      (dolist (cache-details
	       '((dictree--meta-dict-complete-cache-threshold
		  complete-alist
		  dictree--meta-dict-complete-cache)
		 (dictree--meta-dict-complete-ranked-cache-threshold
		  complete-ranked-alist
		  dictree--meta-dict-complete-ranked-cache)
		 (dictree--meta-dict-regexp-cache-threshold
		  regexp-alist
		  dictree--meta-dict-regexp-cache)
		 (dictree--meta-dict-regexp-ranked-cache-threshold
		  regexp-ranked-alist
		  dictree--meta-dict-regexp-ranked-cache)))
	(when (funcall (nth 0 cache-details) dict)
	  ;; convert hash table to alist
	  (set (nth 1 cache-details)
	       (let (alist)
		 (maphash
		  (lambda (key val) (push (cons key val) alist))
		  (funcall (nth 2 cache-details) dict))
		 alist))
	  ;; generate code to reconstruct hash table from alist
	  (setq
	   hashcode
	   (concat
	    hashcode
	    "(let ((cache (make-hash-table :test 'equal)))\n"
	    "  (mapc (lambda (entry)\n"
	    "    (puthash (car entry) (cdr entry) cache))\n"
	    "    (" (symbol-name (nth 2 cache-details)) " "
	            dictname "))\n"
	    "  (setf (" (symbol-name (nth 2 cache-details)) " "
	                dictname ")\n"
	    "        cache))\n")))))


    ;; --- write to file ---
    ;; generate the structure to save
    (setq tmpdict (dictree--meta-dict-copy dict))
    (setf (dictree--meta-dict-name tmpdict) dictname
	  (dictree--meta-dict-filename tmpdict) filename
	  (dictree--meta-dict-modified tmpdict) nil
	  (dictree--meta-dict-meta-dict-list tmpdict) nil
	  (dictree--meta-dict-dictlist tmpdict)
	    (mapcar (lambda (dic) (intern (dictree-name dic)))
		    (dictree--meta-dict-dictlist dict)))
    (unless (featurep 'hashtable-print-readable)
      (setf (dictree--meta-dict-lookup-cache tmpdict) lookup-alist
	    (dictree--meta-dict-complete-cache tmpdict) complete-alist
	    (dictree--meta-dict-complete-ranked-cache tmpdict)
	      complete-ranked-alist
	    (dictree--meta-dict-regexp-cache tmpdict) regexp-alist
	    (dictree--meta-dict-regexp-ranked-cache tmpdict)
	      regexp-ranked-alist))

    ;; write lisp code that generates the dictionary object
    (let ((print-circle t) (print-level nil) (print-length nil))
      (insert "(eval-when-compile (require 'cl))\n"
	      "(require 'dict-tree)\n")
      (mapc
       (lambda (dic)
	 (insert "(unless (dictree-load \"" (dictree-filename dic) "\")\n"
		 "        (error \"Failed to load dictionary \\\""
		 (dictree-name dic) "\\\" required by meta-dict \\\""
		 dictname "\\\"\"))\n"))
       (dictree--meta-dict-dictlist dict))
      (insert "(defvar " dictname " nil \"Dictionary " dictname ".\")\n"
	      "(setq " dictname " " (prin1-to-string tmpdict) ")\n"
	      "(setf (dictree--meta-dict-dictlist " dictname ")\n"
	      "      (mapcar 'eval (dictree--meta-dict-dictlist "
	                            dictname ")))\n")
      (when hashcode (insert hashcode))
      (insert "(unless (memq " dictname " dictree-loaded-list)"
	      " (push " dictname " dictree-loaded-list))\n"))))



;; ----------------------------------------------------------------
;;                Dumping and restoring contents

(defun dictree-populate-from-file
  (dict file
   &optional insert-function key-loadfun data-loadfun plist-loadfun
   balance)
  "Populate dictionary DICT from the key list in file FILE.

Each line of FILE should contain a key, either a string
\(delimited by \"\), a vector, or a list. (Use the escape
sequence \\\" to include a \" in a string.) If a line does not
contain a key, it is silently ignored.

Each line can optionally include data and a property list (in
that order) to be associated with the key. If present, these
should separated from each other and the key by whitespace.

INSERT-FUNCTION, KEY-LOAD-FUNCTION, DATA-LOAD-FUNCTION and
PLIST-LOAD-FUNCTION override the corresponding default functions
for DICT (see `dictree-create').

Interactively, DICT and FILE are read from the mini-buffer.


Technicalities:

The key, data and property list are read as lisp expressions
using `read'. The keys will be read from FILE in order, unless
BALANCE is non-nil, in which case they are read from the median
element outwards (which can help ensure efficient data structures
are created when using a trie that is not self-balancing, see
`dictree-create')."
  (interactive (list (read-dict "Dictionary: ")
		     (read-file-name "File to populate from: "
				     nil "" t)))
  (when (and (called-interactively-p 'any) (symbolp dict))
    (setq dict (symbol-value dict)))

  (if (and (called-interactively-p 'any) (string= file ""))
      (message "No file specified; dictionary %s NOT populated"
	       (dictree-name dict))

    (unless (dictree--meta-dict-p dict)
      (unless key-loadfun
	(setq key-loadfun (dictree--key-loadfun dict)))
      (unless data-loadfun
	(setq data-loadfun (dictree--data-loadfun dict)))
      (unless plist-loadfun
	(setq plist-loadfun (dictree--plist-loadfun dict))))

    (save-excursion
      (let ((buff (find-file-noselect file)))
	(set-buffer buff)

	;; insert the keys starting from the median to ensure a
	;; reasonably well-balanced tree
	(let* ((lines (count-lines (point-min) (point-max)))
	       (midpt (+ (/ lines 2) (mod lines 2)))
	       entry)
	  (message "Inserting keys in %s...(1 of %d)"
		   (dictree-name dict) lines)
	  ;; insert the median key and set the dictionary's modified
	  ;; flag
	  (if balance
	      (dictree--goto-line midpt)
	    (goto-char (point-min)))
	  (when (setq entry
		      (condition-case nil
			  (dictree--read-line key-loadfun data-loadfun
					      plist-loadfun)
			(error (error "Error reading line %d of %s"
				      midpt file))))
	    (dictree-insert dict (car entry) (nth 1 entry)
			    insert-function)
	    (setf (dictree--cell-plist
		   (dictree--lookup dict (car entry) nil))
		  (nth 2 entry)))
	  ;; insert keys successively further away from the median in
	  ;; both directions
	  (dotimes (i (1- (if balance midpt lines)))
	    (if balance
		(dictree--goto-line (+ midpt i 1))
	      (forward-line 1))
	    (when (setq entry
			(condition-case nil
			    (dictree--read-line key-loadfun data-loadfun
						plist-loadfun)
			  (error (error "Error reading line %d of %s"
					(+ midpt i 1) file))))
	      (dictree-insert dict (car entry) (nth 1 entry)
			      insert-function)
	      (setf (dictree--cell-plist
		     (dictree--lookup dict (car entry) nil))
		    (nth 2 entry)))
	    (when (= 49 (mod i 50))
	      (message "Inserting keys in %s...(%d of %d)"
		       (dictree-name dict)
		       (if balance (+ (* 2 i) 2) i)
		       lines))
	    (when balance
	      (dictree--goto-line (- midpt i 1))
	      (when (setq entry
			  (condition-case nil
			      (dictree--read-line key-loadfun data-loadfun
						  plist-loadfun)
			    (error (error "Error reading line %d of %s"
					  (- midpt i 1) file))))
		(dictree-insert dict (car entry)
				(nth 1 entry) insert-function)
		(setf
		 (dictree--cell-plist
		  (dictree--lookup dict (car entry) nil))
		 (nth 2 entry)))))

	  ;; if inserting from mid-point out, and file contains an even
	  ;; number of keys, we still have to add the last one
	  (when (and balance (= 0 (mod lines 2)))
	    (dictree--goto-line lines)
	    (when (setq entry
			(condition-case nil
			    (dictree--read-line key-loadfun data-loadfun
						plist-loadfun)
			  (error (error "Error reading line %d of %s"
					lines file))))
	      (dictree-insert dict (car entry) (nth 1 entry)
			      insert-function)
	      (setf (dictree--cell-plist
		     (dictree--lookup dict (car entry) nil))
		    (nth 2 entry))))

	  (message "Inserting keys in %s...done" (dictree-name dict)))
	(kill-buffer buff)))))



(defun dictree--read-line
  (&optional key-loadfun data-loadfun plist-loadfun)
  ;; Return a list containing the key, data (if any, otherwise nil) and
  ;; property list (ditto) at the current line of the current buffer.
  (save-excursion
    (let (key data plist)
      ;; read key
      (beginning-of-line)
      (when (setq key (read (current-buffer)))
	(when key-loadfun (setq key (funcall key-loadfun key)))
	;; if there's anything after the key, use it as data
	(unless (eq (line-end-position) (point))
	  (setq data (read (current-buffer))))
	(when data-loadfun (setq data (funcall data-loadfun data)))
	;; if there's anything after the data, use it as the property list
	(unless (eq (line-end-position) (point))
	  (setq plist (read (current-buffer))))
	(when plist-loadfun (funcall plist-loadfun plist))
	;; return what we've read
	(list key data plist)))))



(defun dictree-dump-to-buffer (dict &optional buffer type)
  "Dump keys and their associated data
from dictionary DICT to BUFFER, in the same format as that used
by `dictree-populate-from-file'. If BUFFER exists, data will be
appended to the end of it. Otherwise, a new buffer will be
created. If BUFFER is omitted, the current buffer is used.

TYPE determines the type of sequence to use to represent the
keys, and should be one of 'string, 'vector or 'list. The default
is 'vector.

Note that if the data does not have a read syntax, the dumped
data can not be used to recreate the dictionary using
`dictree-populate-from-file'.

Interactively, DICT and BUFFER are read from the mini-buffer,
TYPE is always 'string."
  (interactive (list (read-dict "Dictionary: ")
		     (read-buffer
		      "Buffer to dump to (defaults to current): "
		      (buffer-name (current-buffer)))
		     'string))
  (when (and (called-interactively-p 'any) (symbolp dict))
    (setq dict (symbol-value dict)))

  ;; select the buffer, creating it if necessary
  (if buffer
      (setq buffer (get-buffer-create buffer))
    (setq buffer (current-buffer)))
  (set-buffer buffer)

  ;; move point to end of buffer and make sure it's at start of new line
  (goto-char (point-max))
  (unless (= (point) (line-beginning-position))
    (insert "\n"))

  ;; dump keys
  (message "Dumping keys from %s to %s..."
	   (dictree-name dict) (buffer-name buffer))
  (let ((count 0) (dictsize (dictree-size dict)))
    (message "Dumping keys from %s to %s...(key 1 of %d)"
	     (dictree-name dict) (buffer-name buffer) dictsize)

    ;; map dump function over dictionary
    (dictree--mapc
     (lambda (key data plist)
       (when (= 99 (mod count 100))
	 (message "Dumping keys from %s to %s...(key %d of %d)"
		  (dictree-name dict) (buffer-name buffer)
		  (1+ count) dictsize))
       (insert (prin1-to-string
		(funcall (or (dictree--key-savefun dict) 'identity)
			 key)))
       (when (setq data
		   (funcall (or (dictree--data-savefun dict) 'identity)
			    data))
	 (insert " " (prin1-to-string data)))
       (when (setq plist
		   (funcall (or (dictree--plist-savefun dict) 'identity)
			    plist))
	 (unless data (insert " nil"))
	 (insert " " (prin1-to-string plist)))
       (insert "\n")
       (setq count (1+ count)))
     dict type)  ; dictree-mapc target

    (message "Dumping keys from %s to %s...done"
	     (dictree-name dict) (buffer-name buffer)))
  (switch-to-buffer buffer))



(defun dictree-dump-to-file (dict filename &optional type overwrite)
  "Dump keys and their associated data
from dictionary DICT to a text file FILENAME, in the same format
as that used by `dictree-populate-from-file'. Prompts to overwrite
FILENAME if it already exists, unless OVERWRITE is non-nil.

TYPE determines the type of sequence to use to represent the
keys, and should be one of 'string, 'vector or 'list. The default
is 'vector.

Note that if the data does not have a read syntax and no , the dumped
data can not be used to recreate the dictionary using
`dictree-populate-from-file'.

Interactively, DICT and FILE are read from the mini-buffer,
OVERWRITE is the prefix argument, and TYPE is always 'string."
  (interactive (list (read-dict "Dictionary: ")
		     (read-file-name "File to dump to: " nil "")))
  (when (and (called-interactively-p 'any) (symbolp dict))
    (setq dict (symbol-value dict)))

  (if (and (called-interactively-p 'any) (string= filename ""))
      (message "Dictionary %s NOT dumped" (dictree-name dict))

    ;; check if file exists, and prompt to overwrite it if necessary
    (if (and (file-exists-p filename)
	     (not overwrite)
	     (not (y-or-n-p
		   (format "File %s already exists. Overwrite? "
			   filename))))
	(message "Key dump cancelled")

      (let (buff)
	;; create temporary buffer, dump keys to it, and save to
	;; FILENAME
	(setq buff (generate-new-buffer filename))
	(save-window-excursion
	  (dictree-dump-to-buffer dict buff type)
	  (write-file filename))
	(kill-buffer buff)))))




;; ----------------------------------------------------------------
;;                     Minibuffer completion

(defvar dictree-history nil
  "History list for commands that read a dictionary name.")

(defvar dictree-loaded-history nil
  "History list for commands that read a loaded dictionary name.")


;;;###autoload
(defun read-dict
  (prompt &optional default dictlist allow-unloaded allow-unmatched)
  "Read the name of a dictionary with completion, and return it.

Prompt with PROMPT. By default, return DEFAULT. If DICTLIST is
supplied, only complete on dictionaries in that list.

If ALLOW-UNLOADED is non-nil, also complete on the names of
unloaded dictionaries (actually, on any Elisp file in the current
`load-path' restricted to subdirectories of your home directory
whose file name starts with \"dict-\"). If an unloaded dictionary
is read, the name of the Elisp file will be returned, without
extension, suitable for passing to `load-library'."

  (let (dictname paths)
    ;; when allowing unloaded dictionaries...
    (when allow-unloaded
      ;; get paths in load-path that are subdirectories of home
      ;; directory
      (dolist (d load-path)
	(when (eq (aref d 0) ?~) (push d paths)))
      ;; gather names of all Elisp libraries in this restricted
      ;; load-path
      (dolist (f (all-completions
		  "" (apply-partially 'locate-file-completion-table
				      paths (get-load-suffixes))))
	(when (and (null (file-name-directory f))
		   (and (> (length f) 5)
			(string= (substring f 0 5) "dict-"))
		   (null (file-name-extension f))
		   (not (member (file-name-sans-extension f) dictname)))
	  (push (file-name-sans-extension f) dictname))))
    ;; gather names of loaded dictionaries
    (mapc (lambda (dict)
	    (unless (or (null (dictree-name dict))
			(member (dictree-name dict) dictname))
	      (push (list (dictree-name dict)) dictname)))
	  (or dictlist dictree-loaded-list))
    ;; do completing-read
    (setq dictname (completing-read
		    prompt
		    (if allow-unmatched
			(completion-table-in-turn
			 dictname 'read-file-name-internal)
		      dictname)
		    nil (not allow-unmatched) nil
		    (if allow-unloaded
			'dictree-history
		      'dictree-loaded-history)
		    (and (dictree-p default) (dictree-name default))))
    ;; return dictionary
    (cond
     ;; if user typed a file name, return that
     ((and allow-unmatched (file-regular-p dictname)) dictname)
     ;; if user selected a loaded dictionary, return dict itself
     ((condition-case nil
	  (dictree-p (symbol-value (intern-soft dictname)))
	(void-variable nil))
      (intern-soft dictname))
     ;; if user selected an unloaded dictionary, return dict name
     ((and allow-unloaded (stringp dictname)) dictname)
     ;; if DEFAULT was specified, return that
     (default default)
     ;; should never get here!
     (t (error "Unknown error reading dictionary")))
    ))



;; ----------------------------------------------------------------
;;            Pretty-print dictionaries during edebug

;; We advise the `edebug-prin1' and `edebug-prin1-to-string' functions
;; (actually, aliases) so that they print "#<dict-tree NAME>" instead of
;; the full print form for dictionaries.
;;
;; This is because, if left to its own devices, edebug hangs for ages
;; whilst printing large dictionaries, and you either have to wait for a
;; *very* long time for it to finish, or kill Emacs entirely. (Even C-g
;; C-g fails!)
;;
;; We do this also for lists of dictionaries, since those occur quite
;; often, but not for other sequence types or deeper nested structures,
;; to keep the implementation as simple as possible.
;;
;; Since the print form of a dictionary is practically incomprehensible
;; anyway, we don't lose much by doing this. If you *really* want to
;; print dictionaries in full whilst edebugging, despite this warning,
;; disable the advice.
;;
;; FIXME: We should probably use the `cust-print' features instead of advice
;; here.


(eval-when-compile
  (require 'edebug)
  (require 'advice))


(defun dictree--edebug-pretty-print (object)
  (cond
   ((dictree-p object)
    (concat "#<dict-tree \"" (dictree-name object) "\">"))
   ((null object) "nil")
   ((let ((dlist object) (test t))
      (while (or (dictree-p (car-safe dlist))
		 (and dlist (setq test nil)))
	(setq dlist (cdr dlist)))
      test)
    (concat "(" (mapconcat (lambda (d)
			     (concat "#<dict-tree \""
				     (dictree-name d) "\">"))
			   object " ") ")"))
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
  (around dictree activate compile preactivate)
  (let ((pretty (dictree--edebug-pretty-print object)))
    (if pretty
	(progn
	  (prin1 pretty printcharfun)
	  (setq ad-return-value pretty))
    ad-do-it)))


(when (fboundp 'ad-define-subr-args)
  (ad-define-subr-args 'edebug-prin1-to-string '(object &optional noescape)))

(defadvice edebug-prin1-to-string
  (around dictree activate compile preactivate)
  (let ((pretty (dictree--edebug-pretty-print object)))
    (if pretty
	(setq ad-return-value pretty)
      ad-do-it)))



(provide 'dict-tree)

;;; dict-tree.el ends here
