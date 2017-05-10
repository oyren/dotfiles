;; -*- lexical-binding: t; -*-
;;; tNFA.el --- Tagged non-deterministic finite-state automata

;; Copyright (C) 2008-2010, 2012   Free Software Foundation, Inc

;; Author: Toby Cubitt <toby-predictive@dr-qubit.org>
;; Version: 0.1.1
;; Keywords: extensions, matching, data structures
;;           tNFA, NFA, DFA, finite state automata, automata, regexp
;; Package-Requires: ((queue "0.1"))
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
;; A tagged, non-deterministic finite state automata (NFA) is an abstract
;; computing machine that recognises regular languages. In layman's terms,
;; they are used to decide whether a string matches a regular expression. The
;; "tagged" part allows the NFA to do group-capture: it returns information
;; about which parts of a string matched which subgroup of the regular
;; expression.
;;
;; Why re-implement regular expression matching when Emacs comes with
;; extensive built-in support for regexps? Primarily, because some algorithms
;; require access to the NFA states produced part way through the regular
;; expression matching process (see the trie.el package for an
;; example). Secondarily, because Emacs regexps only work on strings, whereas
;; regular expressions can usefully be used in Elisp code to match other
;; sequence types, not just strings.
;;
;; A tagged NFA can be created from a regular expression using
;; `tNFA-from-regexp', and its state can be updated using
;; `tNFA-next-state'. You can discover whether a state is a matching state
;; using `tNFA-match-p', extract subgroup capture data from it using
;; `tNFA-group-data', check whether a state has any wildcard transitions using
;; `tNFA-wildcard-p', and get a list of non-wildcard transitions using
;; `tNFA-transitions'. Finally, `tNFA-regexp-match' uses tagged NFAs to decide
;; whether a regexp matches a given string.
;;
;; Note that Emacs' regexps are not regular expressions in the original
;; meaning of that phrase. Emacs regexps implement additional features (in
;; particular, back-references) that allow them to match far more than just
;; regular languages. This comes at a cost: regexp matching can potentially be
;; very slow (NP-hard in fact, though the hard cases rarely crop up in
;; practise), whereas there are efficient (polynomial-time) algorithms for
;; matching regular expressions (in the original sense). Therefore, this
;; package only supports a subset of the full Emacs regular expression
;; syntax. See the function docstrings for more information.
;;
;; This package essentially implements Laurikari's algorithm, as described in
;; his master's thesis, but it builds the corresponding tagged deterministic
;; finite state automaton (DFA) on-the-fly as needed.
;;
;; This package uses the queue package queue.el.


;;; Code:

(eval-when-compile (require 'cl))
(require 'queue)



;;; ================================================================
;;;                  Replcements for CL functions

(defun* tNFA--assoc (item alist &key (test 'eq))
  ;; Return first cons cell in ALIST whose CAR matches ITEM according to
  ;; :test function (defaulting to `eq')
  (while (and alist
	      (or (not (consp (car alist)))
		  (not (funcall test item (caar alist)))))
    (setq alist (cdr alist)))
  (car alist))



;;; ================================================================
;;;                    Data structures

;;; ----------------------------------------------------------------
;;;                    tagged NFA states

(defstruct
  (tNFA--state
   (:constructor nil)
   (:constructor tNFA--state-create-initial
		 (NFA-state num-tags min-tags max-tags
		  &aux
		  (tags (tNFA--tags-create num-tags min-tags max-tags))))
   (:constructor tNFA--state-create (NFA-state tags))
   (:copier nil))
  NFA-state tags)

(defmacro tNFA--state-id (state)
  `(tNFA--NFA-state-id (tNFA--state-NFA-state ,state)))

(defmacro tNFA--state-type (state)
  `(tNFA--NFA-state-type (tNFA--state-NFA-state ,state)))

(defmacro tNFA--state-label (state)
  `(tNFA--NFA-state-label (tNFA--state-NFA-state ,state)))

(defmacro tNFA--state-in-degree (state)
  `(tNFA--NFA-state-in-degree (tNFA--state-NFA-state ,state)))

(defmacro tNFA--state-next (state)
  `(tNFA--NFA-state-next (tNFA--state-NFA-state ,state)))

(defmacro tNFA--state-count (state)
  `(tNFA--NFA-state-count (tNFA--state-NFA-state ,state)))



;;; ----------------------------------------------------------------
;;;                         NFA states

(declare (special NFA--state-id))

(defstruct
  (tNFA--NFA-state
   (:type vector)
   (:constructor nil)
   (:constructor tNFA---NFA-state-create
		 (&optional type label next
		  &aux
		  (in-degree 0)
		  (count 0)
		  (id (incf NFA--state-id))
		  ;; (dummy
		  ;;  (when next
		  ;;    (setf (tNFA--NFA-state-count next)
		  ;; 	   (incf (tNFA--NFA-state-in-degree next)))))
		  ))
   (:constructor tNFA--NFA-state-create-branch
		 (&rest next
		  &aux
		  (type 'branch)
		  (in-degree 0)
		  (count 0)
		  (id (incf NFA--state-id))))
   (:constructor tNFA---NFA-state-create-tag
		 (tag &optional next
		  &aux
		  (type 'tag)
		  (label tag)
		  (in-degree 0)
		  (count 0)
		  (id (incf NFA--state-id))
		  ;; (dummy
		  ;;  (when next
		  ;;    (setf (tNFA--NFA-state-count next)
		  ;; 	   (incf (tNFA--NFA-state-in-degree next)))))
		  ))
   (:copier nil))
  id type label in-degree
  count tNFA-state  ; used internally in NFA evolution algorithms
  next)


;; Define these via defun instead of using the dummy argument in the
;; above defstruct to work around a mysterious byte-compiler bug.

(defun tNFA--NFA-state-create (&optional type label next)
  (when next
    (setf (tNFA--NFA-state-count next)
	  (incf (tNFA--NFA-state-in-degree next))))
    (tNFA---NFA-state-create type label next))

(defun tNFA--NFA-state-create-tag (tag &optional next)
  (when next
    (setf (tNFA--NFA-state-count next)
	  (incf (tNFA--NFA-state-in-degree next))))
    (tNFA---NFA-state-create-tag tag next))


;; tag number for a tagged epsilon transition is stored in label slot
(defalias 'tNFA--NFA-state-tag 'tNFA--NFA-state-label)

(defmacro tNFA--NFA-state-tags (state)
  `(tNFA--state-tags (tNFA--NFA-state-tNFA-state ,state)))


(defun tNFA--NFA-state-patch (attach state)
  ;; patch STATE onto ATTACH. Return value is meaningless
  (setf
   (tNFA--NFA-state-type attach)
     (tNFA--NFA-state-type state)
   (tNFA--NFA-state-label attach)
     (tNFA--NFA-state-label state)
   (tNFA--NFA-state-next attach)
     (tNFA--NFA-state-next state)
   (tNFA--NFA-state-count state)
     (incf (tNFA--NFA-state-in-degree state))))


(defun tNFA--NFA-state-make-epsilon (state next)
  ;; create an epsilon transition from STATE to NEXT
  (setf
   (tNFA--NFA-state-type state)  'epsilon
   (tNFA--NFA-state-label state) nil
   (tNFA--NFA-state-next state)  next
   (tNFA--NFA-state-count next)
     (incf (tNFA--NFA-state-in-degree next))))


(defun tNFA--NFA-state-make-branch (state next)
  ;; create a branch from STATE to all states in NEXT list
  (setf (tNFA--NFA-state-type state)  'branch
	(tNFA--NFA-state-label state) nil
	(tNFA--NFA-state-next state)  next)
  (dolist (n next)
    (setf (tNFA--NFA-state-count n)
	  (incf (tNFA--NFA-state-in-degree n)))))


(defun tNFA--NFA-state-copy (state)
  ;; Return a copy of STATE. The next link is *not* copied, it is `eq'
  ;; to the original next link. Use `tNFA--fragment-copy' if you want to
  ;; recursively copy a chain of states. Note: NFA--state-id must be
  ;; bound to something appropriate when this function is called.
  (let ((copy (copy-sequence state)))
    (setf (tNFA--NFA-state-id copy) (incf NFA--state-id))
    copy))



;;; ----------------------------------------------------------------
;;;                        NFA fragments

(defstruct
  (tNFA--fragment
   (:type vector)
   (:constructor nil)
   (:constructor tNFA--fragment-create (initial final))
   (:copier nil))
  initial final)


(defun tNFA--fragment-patch (frag1 frag2)
  ;; patch FRAG2 onto end of FRAG1; return value is meaningless
  (tNFA--NFA-state-patch (tNFA--fragment-final frag1)
			(tNFA--fragment-initial frag2))
  (setf (tNFA--fragment-final frag1) (tNFA--fragment-final frag2)))


(defun tNFA--fragment-copy (fragment)
  ;; return a copy of FRAGMENT.
  (declare (special copied-states))
  (let (copied-states)
    (tNFA--fragment-create
     (tNFA--do-fragment-copy (tNFA--fragment-initial fragment))
     (cdr (assq (tNFA--fragment-final fragment) copied-states)))))


(defun tNFA--do-fragment-copy (state)
  ;; return a copy of STATE, recursively following and copying links
  ;; (note: NFA--state-id must be bound to something appropriate when
  ;; this is called)
  (declare (special copied-states))
  (let ((copy (tNFA--NFA-state-copy state)))
    (push (cons state copy) copied-states)

    ;; if STATE is a branch, copy all links
    (cond
     ((eq (tNFA--NFA-state-type copy) 'branch)
      (setf (tNFA--NFA-state-next copy)
	    (mapcar (lambda (next)
		      (or (cdr (assq next copied-states))
			  (tNFA--do-fragment-copy next)))
		    (tNFA--NFA-state-next copy))))

     ;; if state doesn't have a next link, return
     ((or (eq (tNFA--NFA-state-type copy) 'match)
	  (null (tNFA--NFA-state-type copy))))

     ;; otherwise, copy next link
     ((tNFA--NFA-state-type copy)
      ;; for a non-branch STATE, copy next link
      (setf (tNFA--NFA-state-next copy)
	    ;; if we've already copied next state, set next link to that
	    (or (cdr (assq (tNFA--NFA-state-next copy) copied-states))
		;; otherwise, recursively copy next state
		(tNFA--do-fragment-copy (tNFA--NFA-state-next copy))))))
    copy))



;;; ----------------------------------------------------------------
;;;                      DFA states

(defstruct
  (tNFA--DFA-state
   :named
   (:constructor nil)
   (:constructor tNFA--DFA-state--create
		 (list pool
		  &key
		  (test 'eq)
		  &aux
		  (transitions ())))
   (:copier nil))
  list transitions test wildcard match pool)


(defun* tNFA--DFA-state-create (state-list state-pool &key (test 'eq))
  ;; create DFA state and add it to the state pool
  (let ((DFA-state (tNFA--DFA-state--create
		    state-list state-pool :test test))
	transition)
    (puthash state-list DFA-state (tNFA--DFA-state-pool DFA-state))

    (dolist (state state-list)
      ;; if state in state list is...
      (cond
       ;; literal state: add literal transition
       ((eq (tNFA--state-type state) 'literal)
	(setq transition (cons (tNFA--state-label state) t))
	(unless (member transition (tNFA--DFA-state-transitions DFA-state))
	  (setf (tNFA--DFA-state-transitions DFA-state)
		(append (tNFA--DFA-state-transitions DFA-state)
			(list transition)))))

       ;; character alternative: add transitions for all alternatives
       ((eq (tNFA--state-type state) 'char-alt)
	(dolist (c (tNFA--state-label state))
	  (setq transition (cons c t))
	  (unless (member transition (tNFA--DFA-state-transitions DFA-state))
	    (setf (tNFA--DFA-state-transitions DFA-state)
		  (append (tNFA--DFA-state-transitions DFA-state)
			(list transition))))))

       ;; wildcard or negated character alternative: add wildcard
       ;; transistion
       ((or (eq (tNFA--state-type state) 'wildcard)
	    (eq (tNFA--state-type state) 'neg-char-alt))
	(setf (tNFA--DFA-state-wildcard DFA-state) t))

       ;; match state: set match tags
       ((eq (tNFA--state-type state) 'match)
	(setf (tNFA--DFA-state-match DFA-state)
	      (tNFA--state-tags state)))))

    ;; return constructed state
    DFA-state))


(defun* tNFA--DFA-state-create-initial (state-list &key (test 'eq))
  ;; create initial DFA state from initial tNFA state INITIAL-STATE
  (tNFA--DFA-state-create state-list
			  (make-hash-table :test 'equal)
			  :test test))


(defalias 'tNFA-match-p 'tNFA--DFA-state-match
  "Return non-nil if STATE is a matching state, otherwise return nil.")


(defalias 'tNFA-wildcard-p 'tNFA--DFA-state-wildcard
  "Return non-nil if STATE has a wildcard transition,
 otherwise return nil.")


(defun tNFA-transitions (state)
  "Return list of literal transitions from tNFA state STATE."
  (mapcar 'car (tNFA--DFA-state-transitions state)))



;;; ----------------------------------------------------------------
;;;                      tag tables

(defun tNFA--tags-create (num-tags min-tags max-tags)
  ;; construct a new tags table
  (let ((vec (make-vector num-tags nil)))
    (dolist (tag min-tags)
      (aset vec tag (cons -1 'min)))
    (dolist (tag max-tags)
      (aset vec tag (cons -1 'max)))
    vec))


(defun tNFA--tags-copy (tags)
  ;; return a copy of TAGS table
  (let* ((len (length tags))
	 (vec (make-vector len nil)))
    (dotimes (i len)
      (aset vec i (cons (car (aref tags i))
			(cdr (aref tags i)))))
    vec))


(defmacro tNFA--tags-set (tags tag val)
  ;; set value of TAG in TAGS table to VAL
  `(setcar (aref ,tags ,tag) ,val))


(defmacro tNFA--tags-get (tags tag)
  ;; get value of TAG in TAGS table
  `(car (aref ,tags ,tag)))


(defmacro tNFA--tags-type (tags tag)
  ;; return tag type ('min or 'max)
  `(cdr (aref ,tags ,tag)))


(defun tNFA--tags< (val tag tags)
  ;; return non-nil if VAL takes precedence over the value of TAG in
  ;; TAGS table, nil otherwise
  (setq tag (aref tags tag))
  (or (and (eq (cdr tag) 'min)
	   (< val (car tag)))
    ;;(and (eq (cdr tag) 'max)
	   (> val (car tag));)
	   ))


(defun tNFA--tags-to-groups (tags)
  ;; Convert TAGS table to a list of indices of group matches. The n'th
  ;; element of the list is a cons cell, whose car is the starting index
  ;; of the nth group and whose cdr is its end index. If a group didn't
  ;; match, the corresponding list element will be null."
  (let ((groups (make-list (/ (length tags) 2) nil))
	group-stack
	(grp 0))
    (dotimes (i (length tags))
      (if (eq (tNFA--tags-type tags i) 'max)
	  (unless (= (tNFA--tags-get tags i) -1)
	    (setf (nth (caar group-stack) groups)
		  (cons (cdr (pop group-stack))
			(tNFA--tags-get tags i))))
	(unless (= (tNFA--tags-get tags i) -1)
	  (push (cons grp (tNFA--tags-get tags i)) group-stack))
	(incf grp)))
    groups))




;;; ================================================================
;;;                        Regexp -> tNFA

;;;###autoload
(defun* tNFA-from-regexp (regexp &key (test 'eq))
  "Create a tagged NFA that recognizes the regular expression REGEXP.
The return value is the initial state of the tagged NFA.

REGEXP can be any sequence type (vector, list, or string); it
need not be an actual string. Special characters in REGEXP are
still just that: elements of the sequence that are characters
which have a special meaning in regexps.

The :test keyword argument specifies how to test whether two
individual elements of STRING are identical. The default is `eq'.

Only a subset of the full Emacs regular expression syntax is
supported. There is no support for regexp constructs that are
only meaningful for strings (character ranges and character
classes inside character alternatives, and syntax-related
backslash constructs). Back-references and non-greedy postfix
operators are not supported, so `?' after a postfix operator
loses its special meaning. Also, matches are always anchored, so
`$' and `^' lose their special meanings (use `.*' at the
beginning and end of the regexp to get an unanchored match)."

  ;; convert regexp to list, build NFA, and return initial state
  (declare (special NFA--state-id))
  (destructuring-bind (fragment num-tags min-tags max-tags regexp)
      (let ((NFA--state-id -1))
	(tNFA--from-regexp (append regexp nil) 0 '() '() 'top-level))
    (if regexp
	(error "Syntax error in regexp: missing \"(\"")
      (setf (tNFA--NFA-state-type (tNFA--fragment-final fragment))
	    'match)
      (tNFA--DFA-state-create-initial
       (tNFA--epsilon-boundary
	(list
	 (tNFA--state-create-initial
	  (tNFA--fragment-initial fragment) num-tags min-tags max-tags))
	0)
       :test test)
      )))


(defmacro tNFA--regexp-postfix-p (regexp)
  ;; return t if next token in REGEXP is a postfix operator, nil
  ;; otherwise
  `(or (eq (car ,regexp) ?*)
       (eq (car ,regexp) ?+)
       (eq (car ,regexp) ??)
       (and (eq (car ,regexp) ?\\)
	    (cdr ,regexp)
	    (eq (cadr ,regexp) ?{))))


(defun tNFA--from-regexp (regexp num-tags min-tags max-tags
				 &optional top-level shy-group)
  ;; Construct a tagged NFA fragment from REGEXP, up to first end-group
  ;; character or end of REGEXP. The TAGS arguments are used to pass the
  ;; tags created so far. A non-nil TOP-LEVEL indicates that REGEXP is
  ;; the complete regexp, so we're constructing the entire tNFA. A
  ;; non-nil SHY-GROUP indicates that we're constructing a shy subgroup
  ;; fragment. (Both optional arguments are only used for spotting
  ;; syntax errors in REGEXP.)
  ;;
  ;; Returns a list: (FRAGMENT NUM-TAGS MIN-TAGS MAX-TAGS
  ;; REGEXP). FRAGMENT is the constructed tNFA fragment, REGEXP is the
  ;; remaining, unused portion of the regexp, and the TAGS return values
  ;; give the tags created so far.

  (let* ((new (tNFA--NFA-state-create))
	 (fragment-stack (list (tNFA--fragment-create new new)))
	 fragment copy attach token type group-end-tag)

    (catch 'constructed
      (while t
	(setq regexp (tNFA--regexp-next-token regexp)
	      type   (nth 0 regexp)
	      token  (nth 1 regexp)
	      regexp (nth 2 regexp))
	(setq fragment nil
	      group-end-tag nil)

	;; ----- construct new fragment -----
	(cond
	 ;; syntax error: missing )
	 ((and (null type) (not top-level))
	  (error "Syntax error in regexp:\
 extra \"(\" or missing \")\""))

	 ;; syntax error: extra )
	 ((and (eq type 'shy-group-end) top-level)
	  (error "Syntax error in regexp:\
 extra \")\" or missing \"(\""))

	 ;; syntax error: ) ending a shy group
	 ((and (eq type 'shy-group-end) (not shy-group))
	  (error "Syntax error in regexp: \"(\" matched with \")?\""))

	 ;; syntax error: )? ending a group
	 ((and (eq type 'group-end) shy-group)
	  (error "Syntax error in regexp: \"(?\" matched with \")\""))

	 ;; syntax error: postfix operator not after atom
	 ((eq type 'postfix)
	  (error "Syntax error in regexp: unexpected \"%s\""
		 (char-to-string token)))


	 ;; regexp atom: construct new literal fragment
	 ((or (eq type 'literal) (eq type 'wildcard)
	      (eq type 'char-alt) (eq type 'neg-char-alt))
	  (setq new (tNFA--NFA-state-create
		     type token (tNFA--NFA-state-create))
		fragment (tNFA--fragment-create
			  new (tNFA--NFA-state-next new))))

	 ;; shy subgroup start: recursively construct subgroup fragment
	 ((eq type 'shy-group-start)
	  (setq new (tNFA--from-regexp
		     regexp num-tags min-tags max-tags nil t)
		num-tags (nth 1 new)
		min-tags (nth 2 new)
		max-tags (nth 3 new)
		regexp   (nth 4 new)
		fragment (nth 0 new)))

	 ;; subgroup start: add minimize tag to current fragment, and
	 ;;                 recursively construct subgroup fragment
	 ((eq type 'group-start)
	  (setq new (tNFA--NFA-state-create))
	  (setq fragment
		(tNFA--fragment-create
		 (tNFA--NFA-state-create-tag
		  (car (push (1- (incf num-tags)) min-tags))
		  new)
		 new))
	  (tNFA--fragment-patch (car fragment-stack) fragment)
	  ;; reserve next tag number for subgroup end tag
	  (setq group-end-tag num-tags)
	  (incf num-tags)
	  ;; recursively construct subgroup fragment
	  (setq new (tNFA--from-regexp
		     regexp num-tags min-tags max-tags)
		num-tags (nth 1 new)
		min-tags (nth 2 new)
		max-tags (nth 3 new)
		regexp   (nth 4 new)
		fragment (nth 0 new)))


	 ;; end of regexp or subgroup: ...
	 ((or (null type) (eq type 'shy-group-end) (eq type 'group-end))

	  ;; if fragment-stack contains only one fragment, throw
	  ;; fragment up to recursion level above
	  (cond
	   ((null (nth 1 fragment-stack))
	    (throw 'constructed
		   (list (car fragment-stack)
			 num-tags min-tags max-tags regexp)))

	   ;; if fragment-stack contains multiple alternation fragments,
	   ;; attach them all together
	   ;;
	   ;;          .--fragment--.
	   ;;         /              \
	   ;;        /----fragment----\
	   ;;       /                  \
	   ;;   ---o------fragment------o--->
	   ;;       \        .         /
	   ;;        \       .        /
	   ;;                .
	   (t
	    ;; create a new fragment containing start and end of
	    ;; alternation
	    (setq fragment
		  (tNFA--fragment-create
		   (tNFA--NFA-state-create-branch)
		   (tNFA--NFA-state-create)))
	    ;; patch alternation fragments into new fragment
	    (dolist (frag fragment-stack)
	      (push (tNFA--fragment-initial frag)
		    (tNFA--NFA-state-next
		     (tNFA--fragment-initial fragment)))
	      (setf (tNFA--NFA-state-count
		     (tNFA--fragment-initial frag))
		    (incf (tNFA--NFA-state-in-degree
			   (tNFA--fragment-initial frag))))
	      (tNFA--NFA-state-make-epsilon (tNFA--fragment-final frag)
				      (tNFA--fragment-final fragment)))
	    ;; throw constructed fragment up to recursion level above
	    (throw 'constructed
		   (list fragment num-tags min-tags max-tags regexp)))
	   ))

	 ;; | alternation: start new fragment
	 ((eq type 'alternation)
	  (setq new (tNFA--NFA-state-create))
	  (push (tNFA--fragment-create new new) fragment-stack)))


	;; ----- attach new fragment -----
	(when fragment
	  ;; if next token is not a postfix operator, attach new
	  ;; fragment onto end of current NFA fragment
	  (if (not (tNFA--regexp-postfix-p regexp))
	      (tNFA--fragment-patch (car fragment-stack) fragment)

	    ;; if next token is a postfix operator, splice new fragment
	    ;; into NFA as appropriate
	    (when (eq type 'alternation)
	      (error "Syntax error in regexp: unexpected \"%s\""
		     (char-to-string token)))
	    (setq regexp (tNFA--regexp-next-token regexp)
		  type   (nth 0 regexp)
		  token  (nth 1 regexp)
		  regexp (nth 2 regexp))

	    (while fragment
	      (setq attach (tNFA--fragment-final (car fragment-stack)))
	      (setq new (tNFA--NFA-state-create))
	      (cond

	       ;; * postfix = \{0,\}:
	       ;;
	       ;;    .--fragment--.
	       ;;   /              \
	       ;;   \        ______/
	       ;;    \      /
	       ;;  ---attach-----new---
	       ;;
	       ((and (eq (car token) 0) (null (cdr token)))
		(tNFA--NFA-state-make-branch
		 attach (list (tNFA--fragment-initial fragment) new))
		(tNFA--NFA-state-make-epsilon
		 (tNFA--fragment-final fragment) attach)
		(setf (tNFA--fragment-final (car fragment-stack)) new)
		(setq fragment nil))

	       ;; + postfix = \{1,\}:
	       ;;
	       ;;      .----.
	       ;;     /      \
	       ;;    /        \
	       ;;    \        /
	       ;;  ---fragment-----new---
	       ;;
	       ((and (eq (car token) 1) (null (cdr token)))
		(tNFA--NFA-state-patch
		 attach (tNFA--fragment-initial fragment))
		(tNFA--NFA-state-make-branch
		 (tNFA--fragment-final fragment) (list attach new))
		(setf (tNFA--fragment-final (car fragment-stack)) new)
		(setq fragment nil))

	       ;; \{0,n\} (note: ? postfix = \{0,1\}):
	       ;;
	       ;;            .--fragment--.
	       ;;           /              \
	       ;;  ---attach                new---
	       ;;           \______________/
	       ;;
	       ((eq (car token) 0)
		;; ? postfix = \{0,1\}: after this we're done
		(if (eq (cdr token) 1)
		    (setq copy nil)
		  (setq copy (tNFA--fragment-copy fragment)))
		;; attach fragment
		(tNFA--NFA-state-make-branch
		 attach (list (tNFA--fragment-initial fragment) new))
		(tNFA--NFA-state-make-epsilon
		 (tNFA--fragment-final fragment) new)
		(setf (tNFA--fragment-final (car fragment-stack)) new)
		;; prepare for next iteration
		(decf (cdr token))
		(setq fragment copy))

	       ;; \{n,\} or \{n,m\}:
	       ;;
	       ;;  ---attach----fragment----new---
	       ;;
	       (t
		(setq copy (tNFA--fragment-copy fragment))
		(tNFA--fragment-patch (car fragment-stack) fragment)
		;; prepare for next iteration
		(decf (car token))
		(when (cdr token) (decf (cdr token)))
		(if (eq (cdr token) 0)
		    (setq fragment nil)
		  (setq fragment copy)))
	       )))


	  ;; if ending a group, add a maximize tag to end
	  (when group-end-tag
	    (setq new (tNFA--NFA-state-create)
		  fragment (tNFA--fragment-create
			    (tNFA--NFA-state-create-tag
			     group-end-tag new)
			    new))
	    (push group-end-tag max-tags)
	    (tNFA--fragment-patch (car fragment-stack) fragment)))
	))  ; end of infinite loop and catch
    ))



;; Note: hard-coding the parsing like this is ugly, though sufficient
;;       for our purposes. Perhaps it would be more elegant to implement
;;       this in terms of a proper parser...

(defun tNFA--regexp-next-token (regexp)
  ;; if regexp is empty, return null values for next token type, token
  ;; and remaining regexp
  (if (null regexp)
      (list nil nil nil)

    (let ((token (pop regexp))
	  (type 'literal))  ; assume token is literal initially
      (cond

       ;; [: gobble up to closing ]
       ((eq token ?\[)
	;; character alternatives are stored in lists
	(setq token '())
	(cond
	 ;; gobble ] appearing straight after [
	 ((eq (car regexp) ?\]) (push (pop regexp) token))
	 ;; gobble ] appearing straight after [^
	 ((and (eq (car regexp) ?^) (eq (nth 1 regexp) ?\]))
	  (push (pop regexp) token)
	  (push (pop regexp) token)))
	;; gobble everything up to closing ]
	(while (not (eq (car regexp) ?\]))
	  (push (pop regexp) token)
	  (unless regexp
	    (error "Syntax error in regexp: missing \"]\"")))
	(pop regexp)  ; dump closing ]
	(if (not (eq (car (last token)) ?^))
	    (setq type 'char-alt)
	  (setq type 'neg-char-alt)
	  (setq token (butlast token))))

       ;; ]: syntax error (always gobbled when parsing [)
       ((eq token ?\])
	(error "Syntax error in regexp: missing \"[\""))

       ;; . * + ?: set appropriate type
       ((eq token ?*) (setq type 'postfix token (cons 0 nil)))
       ((eq token ?+) (setq type 'postfix token (cons 1 nil)))
       ((eq token ??) (setq type 'postfix token (cons 0 1)))
       ((eq token ?.) (setq type 'wildcard))

       ;; \: look at next character
       ((eq token ?\\)
	(unless (setq token (pop regexp))
	  (error "Syntax error in regexp:\
 missing character after \"\\\""))
	(cond
	 ;; |: alternation
	 ((eq token ?|) (setq type 'alternation))
	 ;; \(?: shy group start
	 ((and (eq token ?\() (eq (car regexp) ??))
	  (setq type 'shy-group-start)
	  (pop regexp))
	 ;; \)?: shy group end
	 ((and (eq token ?\)) (eq (car regexp) ??))
	  (setq type 'shy-group-end)
	  (pop regexp))
	 ;; \(: group start
	 ((eq token ?\() (setq type 'group-start))
	 ;; \): group end
	 ((eq token ?\)) (setq type 'group-end))

	 ;; \{: postfix repetition operator
	 ((eq token ?{)
	  (setq type 'postfix token (cons nil nil))
	  ;; extract first number from repetition operator
	  (while (if (null regexp)
		     (error "Syntax error in regexp:\
 malformed \\{...\\}")
		   (not (or (eq (car regexp) ?,)
			    (eq (car regexp) ?\\))))
	    (setcar token
		    (concat (car token) (char-to-string (pop regexp)))))
	  (if (null (car token))
	      (setcar token 0)
	    (unless (string-match "[0-9]+" (car token))
	      (error "Syntax error in regexp: malformed \\{...\\}"))
	    (setcar token (string-to-number (car token))))
	  (cond
	   ;; if next character is "\", we expect "}" to follow
	   ((eq (car regexp) ?\\)
	    (pop regexp)
	    (unless (eq (car regexp) ?})
	      (error "Syntax error in regexp: expected \"}\""))
	    (pop regexp)
	    (unless (car token)
	      (error "Syntax error in regexp: malformed \\{...\\}"))
	    (setcdr token (car token)))
	   ;; if next character is ",", we expect a second number to
	   ;; follow
	   ((eq (car regexp) ?,)
	    (pop regexp)
	    (while (if (null regexp)
		       (error "Syntax error in regexp:\
 malformed \\{...\\}")
		     (not (eq (car regexp) ?\\)))
	      (setcdr token
		      (concat (cdr token)
			      (char-to-string (pop regexp)))))
	    (unless (null (cdr token))
	      (unless (string-match "[0-9]+" (cdr token))
	 	(error "Syntax error in regexp: malformed \\{...\\}"))
	      (setcdr token (string-to-number (cdr token))))
	    (pop regexp)
	    (unless (eq (car regexp) ?})
	      (error "Syntax error in regexp: expected \"}\""))
	    (pop regexp))))
	 ))
       )

      ;; return first token type, token, and remaining regexp
      (list type token regexp))))



;;; ================================================================
;;;                     tNFA evolution

(defun tNFA-next-state (tNFA chr pos)
  "Evolve tNFA according to CHR, which corresponds to position
POS in a string."
  (let (elem state)
    ;; if there is a transition for character CHR...
    (cond
     ((setq elem (tNFA--assoc chr (tNFA--DFA-state-transitions tNFA)
			       :test (tNFA--DFA-state-test tNFA)))
      ;; if next state has not already been computed, do so
      (unless (tNFA--DFA-state-p (setq state (cdr elem)))
	(setq state (tNFA--DFA-next-state tNFA chr pos nil))
	(setcdr elem state)))

     ;; if there's a wildcard transition...
     ((setq state (tNFA--DFA-state-wildcard tNFA))
      ;; if next state has not already been computed, do so
      (unless (tNFA--DFA-state-p state)
	(setq state (tNFA--DFA-next-state tNFA chr pos t))
	(setf (tNFA--DFA-state-wildcard tNFA) state))))
    state))



(defun tNFA--DFA-next-state (DFA-state chr pos wildcard)
  (let (state-list state)
    ;; add all states reached by a CHR transition from DFA-STATE to
    ;; state list
    (if wildcard
	(dolist (state (tNFA--DFA-state-list DFA-state))
	  (when (or (eq (tNFA--state-type state) 'wildcard)
		    (and (eq (tNFA--state-type state) 'neg-char-alt)
			 (not (memq chr (tNFA--state-label state)))))
	    (push (tNFA--state-create
		   (tNFA--state-next state)
		   (tNFA--tags-copy (tNFA--state-tags state)))
		  state-list)))
      (dolist (state (tNFA--DFA-state-list DFA-state))
	(when (or (and (eq (tNFA--state-type state) 'literal)
		       (eq chr (tNFA--state-label state)))
		  (and (eq (tNFA--state-type state) 'char-alt)
		       (memq chr (tNFA--state-label state)))
		  (and (eq (tNFA--state-type state) 'neg-char-alt)
		       (not (memq chr (tNFA--state-label state))))
		  (eq (tNFA--state-type state) 'wildcard))
	  (push (tNFA--state-create
		 (tNFA--state-next state)
		 (tNFA--tags-copy (tNFA--state-tags state)))
		state-list))))

    ;; if state list is empty, return empty, failure DFA state
    (when state-list
      ;; otherwise, construct new DFA state and add it to the pool if
      ;; it's not already there
      (setq state-list (tNFA--epsilon-boundary state-list (1+ pos)))
      (setq state
	    (or (gethash state-list (tNFA--DFA-state-pool DFA-state))
		(tNFA--DFA-state-create
		 state-list
		 (tNFA--DFA-state-pool DFA-state)
		 :test (tNFA--DFA-state-test DFA-state))))
      ;; return next state
      state)))



(defun tNFA--epsilon-boundary (state-set pos)
  ;; Return the tagged epsilon-boundary of the NFA states listed in
  ;; STATE-SET, that is the set of all states that can be reached via
  ;; epsilon transitions from some state in STATE-SET (not including
  ;; states in STATE-SET itself).
  (let ((queue (queue-create))
	(result '())
	(reset '())
	state next tags)
    ;; temporarily link the NFA states to their corresponding tNFA
    ;; states, and add them to the queue
    (dolist (t-state state-set)
      (setf state (tNFA--state-NFA-state t-state)
	    (tNFA--NFA-state-tNFA-state state) t-state)
      (push state reset)
      (queue-enqueue queue state))

    (while (setq state (queue-dequeue queue))
      (cond
       ;; branch or epsilon: add next states as necessary, copying tags
       ;; across
       ((or (eq (tNFA--NFA-state-type state) 'branch)
	    (eq (tNFA--NFA-state-type state) 'epsilon))
	(dolist (next (if (eq (tNFA--NFA-state-type state) 'epsilon)
			  (list (tNFA--NFA-state-next state))
			(tNFA--NFA-state-next state)))
	  (unless (tNFA--NFA-state-tNFA-state next)
	    (setf (tNFA--NFA-state-tNFA-state next)
		  (tNFA--state-create
		   next (tNFA--tags-copy (tNFA--NFA-state-tags state))))
	    (push next reset)
	    ;; if next state hasn't already been seen in-degree times,
	    ;; add it to the end of the queue
	    (if (/= (decf (tNFA--NFA-state-count next)) 0)
		(queue-enqueue queue next)
	      ;; if it has now been seen in-degree times, reset count
	      ;; and add it back to the front of the queue
	      (setf (tNFA--NFA-state-count next)
		    (tNFA--NFA-state-in-degree next))
	      (queue-prepend queue next)))))

       ;; tag: add next state if necessary, updating tags if necessary
       ((eq (tNFA--NFA-state-type state) 'tag)
	(setq next (tNFA--NFA-state-next state))
	;; if next state is not already in results list, or it is
	;; already in results but new tag value takes precedence...
	(when (or (not (tNFA--NFA-state-tNFA-state next))
		  (tNFA--tags< pos (tNFA--NFA-state-tag state)
			      (tNFA--NFA-state-tags next)))
	  ;; if next state is already in results, update tag value
	  (if (tNFA--NFA-state-tNFA-state next)
	      (tNFA--tags-set (tNFA--NFA-state-tags next)
			     (tNFA--NFA-state-tag state) pos)
	    ;; if state is not already in results, copy tags, updating
	    ;; tag value, and add next state to results list
	    (setq tags (tNFA--tags-copy (tNFA--NFA-state-tags state)))
	    (tNFA--tags-set tags (tNFA--NFA-state-tag state) pos)
	    (setf (tNFA--NFA-state-tNFA-state next)
		  (tNFA--state-create next tags))
	    (push next reset))
	  ;; if next state hasn't already been seen in-degree times, add
	  ;; it to the end of the queue
	  (if (/= (decf (tNFA--NFA-state-count next)) 0)
	      (queue-enqueue queue next)
	    ;; if it has now been seen in-degree times, reset count and
	    ;; add it back to the front of the queue
	    (setf (tNFA--NFA-state-count next)
		  (tNFA--NFA-state-in-degree next))
	    (queue-prepend queue next))))

       ;; anything else is a non-epsilon-transition state, so add it to
       ;; result
       (t (push (tNFA--NFA-state-tNFA-state state) result))
       ))

    ;; reset temporary NFA state link and count
    (dolist (state reset)
      (setf (tNFA--NFA-state-tNFA-state state) nil
	    (tNFA--NFA-state-count state)
	      (tNFA--NFA-state-in-degree state)))
    ;; sort result states
    (sort result
	  (lambda (a b) (< (tNFA--state-id a) (tNFA--state-id b))))
    ))



;;; ================================================================
;;;                       tNFA matching

;;;###autoload
(defun* tNFA-regexp-match (regexp string &key (test 'eq))
  "Return non-nil if STRING matches REGEXP, nil otherwise.
Sets the match data if there was a match; see `match-beginning',
`match-end' and `match-string'.

REGEXP and STRING can be any sequence type (vector, list, or
string); they need not be actual strings. Special characters in
REGEXP are still just that: elements of the sequence that are
characters which have a special meaning in regexps.

The :test keyword argument specifies how to test whether two
individual elements of STRING are identical. The default is `eq'.

Only a subset of the full Emacs regular expression syntax is
supported. There is no support for regexp constructs that are
only meaningful for strings (character ranges and character
classes inside character alternatives, and syntax-related
backslash constructs). Back-references and non-greedy postfix
operators are not supported, so `?' after a postfix operator
loses its special meaning. Also, matches are always anchored, so
`$' and `^' lose their special meanings (use `.*' at the
beginning and end of the regexp to get an unanchored match)."

  (let ((tNFA (tNFA-from-regexp regexp :test test))
	(i -1) tags match-data group-stack (grp 0))

    ;; evolve tNFA according to characters of STRING
    (catch 'fail
      (dolist (chr (append string nil))
	(unless (setq tNFA (tNFA-next-state tNFA chr (incf i)))
	  (throw 'fail nil)))

      ;; if REGEXP matched...
      (when (setq tags (tNFA--DFA-state-match tNFA))
	(setq match-data (make-list (+ (length tags) 2) nil))
	;; set match data
	(setf (nth 0 match-data) 0
	      (nth 1 match-data) (length string))
	;; set group match data if there were any groups
	(dotimes (i (length tags))
	  (if (eq (tNFA--tags-type tags i) 'max)
	      (unless (= (tNFA--tags-get tags i) -1)
		(setf (nth (1+ (* 2 (pop group-stack))) match-data)
		      (tNFA--tags-get tags i)))
	    (incf grp)
	    (unless (= (tNFA--tags-get tags i) -1)
	      (push grp group-stack)
	      (setf (nth (* 2 grp) match-data)
		    (tNFA--tags-get tags i)))))
	(set-match-data match-data)
	tags))))


(defun tNFA-group-data (tNFA)
  "Return the group match data associated with a tNFA state."
  (tNFA--tags-to-groups (tNFA--DFA-state-match tNFA)))



(provide 'tNFA)

;;; tNFA.el ends here
