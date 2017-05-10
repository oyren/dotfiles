
;;; predictive-auto-overlay-auto-dict.el --- automatic overlays with automatic
;;;                                          dictionary update


;; Copyright (C) 2008, 2012 Toby Cubitt

;; Author: Toby Cubitt <toby-predictive@dr-qubit.org>
;; Version: 0.3.2
;; Keywords: predictive, automatic, overlays, dictionary, auto-dict
;; URL: http://www.dr-qubit.org/emacs.php

;; This file is NOT part of Emacs.
;;
;; This file is free software: you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free
;; Software Foundation, either version 3 of the License, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
;; more details.
;;
;; You should have received a copy of the GNU General Public License along
;; with this program.  If not, see <http://www.gnu.org/licenses/>.


;;; Code:

(eval-when-compile (require 'cl))
(require 'auto-overlays)
(require 'auto-overlay-word)
(require 'dict-tree)
(require 'predictive)
(require 'completion-ui)  ; we use `completion--position' replacement for
			  ; cl function `positon'

(provide 'predictive-auto-overlay-auto-dict)


;; set auto-dict overlay parsing and suicide functions, and indicate class
;; requires separate start and end regexps
(put 'predictive-auto-dict 'auto-overlay-parse-function
     'predictive-parse-auto-dict-match)
(put 'predictive-auto-dict 'auto-overlay-suicide-function
     'predictive-auto-dict-suicide)



(defun predictive-parse-auto-dict-match (o-match)
  ;; Create a new word overlay, and add its contents to a dictionary

  ;; create new word overlay
  (let ((o-new (auto-o-parse-word-match o-match))
	word dict)
    ;; extract word and get dict
    (setq word (buffer-substring-no-properties
		(overlay-get o-match 'delim-start)
		(overlay-get o-match 'delim-end)))
    (setq dict (overlay-get o-new 'auto-dict))
    ;; save word and dict in overlay properties
    (overlay-put o-match 'word word)
    (overlay-put o-match 'auto-dict dict)
    ;; add change function to overlay modification hooks
    (overlay-put o-new 'modification-hooks
		 (cons 'predictive-schedule-auto-dict-update
		       (overlay-get o-new 'modification-hooks)))
    (overlay-put o-new 'insert-in-front-hooks
		 (cons 'predictive-schedule-auto-dict-update
		       (overlay-get o-new 'insert-in-front-hooks)))
    (overlay-put o-new 'insert-behind-hooks
		 (cons 'predictive-schedule-auto-dict-update
		       (overlay-get o-new 'insert-behind-hooks)))
    ;; add word to dictionary
    (unless (dictree-p dict) (setq dict (symbol-value dict)))
    (dictree-insert dict word 0
		    ;; no duplicate definition warning if starting
		    ;; predictive-mode (all auto-dict entries get re-added at
		    ;; that stage)
		    (when (and predictive-mode
			       (dictree-get-property dict word :definitions))
		      (lambda (new old)
			(message "Warning: duplicate definition of \"%s\""
				 word)
			(+ new old))))
    ;; store reference to this definition overlay
    (let ((defs (remove (buffer-file-name (overlay-buffer o-new))
			(dictree-get-property dict word :definitions))))
      (dictree-put-property dict word :definitions
			    (pushnew o-new defs :test 'eq)))
    ;; return the new overlay
    o-new))



(defun predictive-auto-dict-suicide (o-match)
  ;; Delete the word overlay, delete the definition, and delete the word from
  ;; the dictionary if there are no more definitions

  (let ((word (overlay-get o-match 'word))
	(dict (overlay-get o-match 'auto-dict))
	(parent (overlay-get o-match 'parent)))
    ;; delete the overlay
    (auto-o-delete-overlay parent)
    (unless (dictree-p dict) (setq dict (symbol-value dict)))
    ;; delete overlay from definition list
    (unless (dictree-put-property
	     dict word :definitions
	     (delq parent (dictree-get-property dict word :definitions)))
      ;; schedule word deletion
      (add-to-list 'auto-o-pending-post-update
		   (list 'predictive-auto-dict-delete-word dict word)))))



(defun predictive-schedule-auto-dict-update
  (o-self modified &rest unused)
  ;; All auto-dict overlay modification hooks are set to this function, which
  ;; schedules `predictive-auto-dict-update' to run after any suicide
  ;; functions have been called
  (unless modified
    (add-to-list 'auto-o-pending-post-suicide
		 (list 'predictive-auto-dict-update o-self))))


(defun predictive-auto-dict-delete-word (dict word)
  ;; delete WORD from DICT if its definition list is empty
  (unless (dictree-get-property dict word :definitions)
    (dictree-delete dict word)))



(defun predictive-auto-dict-update (o-self)
  ;; Update the auto-dict with new word. Runs after modifications.

  (let ((dict (overlay-get (overlay-get o-self 'start) 'auto-dict))
	(word (overlay-get (overlay-get o-self 'start) 'word))
	defs)
    (unless (dictree-p dict) (setq dict (symbol-value dict)))
    ;; delete definition overlay from :definitions property of word in
    ;; auto-dict, deleting word entirely if this was last definition overlay
    (when (null (dictree-put-property
		 dict word :definitions
		 (delq o-self (dictree-get-property dict word :definitions))))
      (dictree-delete dict word))

    ;; if overlay has not been deleted...
    (when (overlay-buffer o-self)
      ;; extract new word
      (setq word (buffer-substring-no-properties
		  (overlay-start o-self) (overlay-end o-self)))
      ;; save label in overlay property
      (overlay-put (overlay-get o-self 'start) 'word word)
      ;; add new word to auto-dict
      (dictree-insert dict word 0
		      (lambda (new old)
			(message "Warning: dupliacte definition of \"%s\""
				 word)
			(+ new old)))
      ;; store reference to this definition overlay
      (dictree-put-property
       dict word :definitions
       (cons o-self (remove (buffer-file-name (overlay-buffer o-self))
			    (dictree-get-property dict word :definitions)))))
    ))



;;; =================================================================
;;;       Functions for automatically generated dictionaries

(defmacro predictive-auto-dict-name (name &optional file)
  ;; Return a dictionary name constructed from NAME and the buffer name
  `(intern
    (concat "dict-" ,name "-"
	    (replace-regexp-in-string
	     "\\." "-" (file-name-nondirectory
			(or ,file (buffer-file-name)))))))



(defun predictive-auto-dict-plist-savefun (plist)
  ;; convert overlays to buffer file names in :definitions property
  (let ((defs (plist-get plist :definitions))
	strings)
    (mapc
     (lambda (o)
       (add-to-list 'strings
		    (if (overlayp o)
			(buffer-file-name (overlay-buffer o))
		      o)))
     defs)
    (setq plist (copy-sequence plist))
    (plist-put plist :definitions strings)
    plist))



(defun predictive-auto-dict-load (name &optional file)
  "Load/create a NAME dictionary for the current buffer."
  (let (dictname)
    (cond
     ;; if buffer is associated with a file...
     ((buffer-file-name)
      (setq dictname (predictive-auto-dict-name name file))
      (unless file
	(setq file
	      (concat (file-name-directory (buffer-file-name))
		      predictive-auxiliary-file-location
		      (symbol-name dictname))))
      ;; create directory for dictionary file if necessary
      (predictive-create-auxiliary-file-location)
      ;; if dictionary is already loaded, return it
      (if (condition-case
	      error (symbol-value (intern-soft dictname))
	    (void-variable nil))
	  (symbol-value (intern-soft dictname))
	;; otherwise, load or create it
	(if (load file t)
	    (setf (dictree-filename (symbol-value dictname)) file)
	  (dictree-create dictname file
			  predictive-dict-autosave nil
			  '< '+ 'predictive-dict-rank-function
			  nil nil nil
			  nil predictive-completion-speed
			  nil predictive-completion-speed
			  nil nil nil nil
			  'predictive-auto-dict-plist-savefun nil))
	(predictive-load-dict dictname)
	(symbol-value dictname)))  ; return the dictionary

     ;; if buffer is not associated with a file, create a temporary NAME
     ;; dictionary
     (t
      (dictree-create nil nil nil nil
		      '< '+ 'predictive-dict-rank-function
		      nil nil nil
		      nil predictive-completion-speed
		      nil predictive-completion-speed
		      nil nil nil nil
		      'predictive-auto-dict-plist-savefun nil))
     )))


(defun predictive-auto-dict-unload (name &optional file dont-save)
  "Unload and possibly save the current buffer's NAME dictionary."
  (when (buffer-file-name)
    (predictive-unload-dict
     (symbol-value (predictive-auto-dict-name name file)) dont-save)))


(defun predictive-auto-dict-save (name &optional file)
  "Save the current buffer's NAME dictionary if associated with a file."
  (when (buffer-file-name)
    (dictree-save (symbol-value (predictive-auto-dict-name name file)))))



;;; =================================================================
;;;   Navigation functions for automatically generated dictionaries

(defun predictive-auto-dict-jump-to-def (dict key &optional overlay)
  "Jump to definition of KEY from auto-dict DICT.
If OVERLAY is supplied and is a definition overlay for KEY, jump
to the next definition in the list after the one corresponding to
OVERLAY."

  (let ((defs (dictree-get-property dict key :definitions))
	i)
    (when defs
      ;; find index of first definition after OVERLAY in list if supplied
      (setq i (mod (or (and overlay
			    (1+ (or (completion--position overlay defs) -1)))
		       0)
		   (length defs)))
      ;; process list until we find a definition overlay
      (while (and defs
		  (not (and (overlayp (nth i defs))
			    (overlay-buffer (nth i defs)))))
	(cond
	 ;; if we find a filename in the list, open the file and enable
	 ;; predictive-mode (which will replace the filename with overlays)
	 ((stringp (nth i defs))
	  ;; if file doesn't exist, remove filename from list
	  (if (not (file-exists-p (nth i defs)))
	      (progn
		(setq defs (delq (nth i defs) defs))
		(setq i 0))
	    (save-window-excursion
	      (find-file (nth i defs))
	      (turn-on-predictive-mode))))
	 ;; if we find an obsolete overlay in the list, delete it from list
	 ;; (should never occur!)
	 ((overlayp (nth i defs))
	  (setq defs (delq (nth i defs) defs))
	  (setq i 0))))

      (when defs
	;; jump to definition, unless we're already at it
	(let ((o-def (nth i defs)))
	(unless (and (eq (overlay-buffer o-def) (current-buffer))
		     (>= (point) (overlay-start o-def))
		     (<= (point) (overlay-end o-def)))
	  (push-mark)
	  (switch-to-buffer (overlay-buffer (nth i defs)))
	  (goto-char (overlay-start (nth i defs))))))

      defs)))  ; return list of definition


;;; predictive-auto-overlay-auto-dict.el ends here
