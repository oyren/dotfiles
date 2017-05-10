
;;; predictive-latex-cleveref.el --- predictive mode LaTeX cleveref
;;;                                  package support


;; Copyright (C) 2004-2012 Toby Cubitt

;; Author: Toby Cubitt <toby-predictive@dr-qubit.org>
;; Version: 0.9.2
;; Keywords: predictive, latex, package, cleveref, cref
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
;; with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.


;;; Code:

(require 'predictive-latex)
(provide 'predictive-latex-cleveref)

;; add load and unload functions to alist
;;(assoc-delete-all "cleveref" predictive-latex-usepackage-functions)
(push '("cleveref" predictive-latex-load-cleveref
	predictive-latex-unload-cleveref)
      predictive-latex-usepackage-functions)


;; set up 'predictive-latex-cleveref-label-word to be a `thing-at-point'
;; symbol
(put 'predictive-latex-cleveref-label-word 'forward-op
     'predictive-latex-cleveref-label-forward-word)


;; variables used to hold old definitions of label regexps
(defvar predictive-latex-cleveref-restore-label-regexp nil)
(defvar predictive-latex-cleveref-restore-label-definition nil)
(defvar predictive-latex-cleveref-restore-vref-regexp nil)
(make-variable-buffer-local
 'predictive-latex-cleveref-restore-label-regexp)
(make-variable-buffer-local
 'predictive-latex-cleveref-restore-label-definition)
(make-variable-buffer-local
 'predictive-latex-cleveref-restore-vref-definition)



(defun predictive-latex-load-cleveref ()
  ;; load cleveref regexps
  (destructuring-bind (word-resolve word-complete word-insert
		       punct-resolve punct-complete punct-insert
		       whitesp-resolve whitesp-complete whitesp-insert)
      (append (auto-completion-lookup-behaviour nil ?w)
	      (auto-completion-lookup-behaviour nil ?.)
	      (auto-completion-lookup-behaviour nil ? ))

    ;; add new browser sub-menu definition
    (make-local-variable 'predictive-latex-browser-submenu-alist)
    (push (cons "\\\\[cC]ref\\(range\\|\\)" 'predictive-latex-label-dict)
	  predictive-latex-browser-submenu-alist)

    ;; load regexps
    ;; \cref and \vref
    (setq predictive-latex-cleveref-restore-vref-regexp
	  (auto-overlay-unload-regexp 'predictive 'brace 'vref))
    (auto-overlay-load-regexp
     'predictive 'brace
     `(("\\([^\\]\\|^\\)\\(\\\\\\\\\\)*\\(\\\\\\([cCvV]ref\\(\\|range\\)\\*?\\|\\(name\\|label\\)[cC]ref\\){\\)"
	. 3)
       :edge start
       :id cref
       (dict . predictive-latex-label-dict)
       (priority . 40)
       (completion-menu . predictive-latex-construct-browser-menu)
       (completion-word-thing . predictive-latex-cleveref-label-word)
       (auto-completion-syntax-alist
	. ((?w . ((lambda ()
		    (let ((label (bounds-of-thing-at-point
				  'predictive-latex-cleveref-label-word)))
		      (when (and label (= (point) (car label)))
			(delete-region (car label) (cdr label))))
		    'add)
		  ,word-complete t))
	   (?_ . (add ,word-complete))
	   (?  . (,whitesp-resolve none))
	   (?. . (add ,word-complete))
	   (t  . (reject none))))
       (auto-completion-override-syntax-alist
	. ((?: . ((lambda ()
		    (predictive-latex-completion-add-till-regexp ":"))
		  ,word-complete))
	   (?_ . ((lambda ()
		    (predictive-latex-completion-add-till-regexp "\\W"))
		  ,word-complete))
	   (?, . (,punct-resolve none))
	   (?} . (,punct-resolve none))))
       (face . (background-color . ,predictive-overlay-debug-color)))
     t)

    ;; \label with optional argument replaces normal \label regexps
    (setq predictive-latex-cleveref-restore-label-regexp
	  (auto-overlay-unload-regexp 'predictive 'brace 'label))
    (auto-overlay-load-regexp
     'predictive 'brace
     `(("\\([^\\]\\|^\\)\\(\\\\\\\\\\)*\\(\\\\label\\(\\[.*?\\]\\)?{\\)" . 3)
       :edge start
       :id label
       (dict . t)
       (priority . 40)
       (face . (background-color . ,predictive-overlay-debug-color)))
     t)
    (setq predictive-latex-cleveref-restore-label-definition
	  (auto-overlay-unload-definition 'predictive 'label))
    (auto-overlay-load-definition
     'predictive
     '(predictive-auto-dict
       :id label
       (("\\([^\\]\\|^\\)\\(\\\\\\\\\\)*\\\\label\\(\\[.*?\\]\\)?{\\(.*?\\)}"
	 . 4)
       (auto-dict . predictive-latex-label-dict)))
     t)))



(defun predictive-latex-unload-cleveref ()
  ;; remove browser sub-menu definition
  (setq predictive-latex-browser-submenu-alist
	(predictive-assoc-delete-all "\\\\[cC]ref\\(range\\|\\)"
				     predictive-latex-browser-submenu-alist))
  ;; Unload cleveref regexps
  (auto-overlay-unload-regexp 'predictive 'brace 'cref)
  (when predictive-latex-cleveref-restore-vref-regexp
    (auto-overlay-load-regexp
     'predictive 'brace predictive-latex-cleveref-restore-vref-regexp t))
  (auto-overlay-unload-regexp 'predictive 'brace 'label)
  (auto-overlay-unload-definition 'predictive 'label)
  (auto-overlay-load-regexp
   'predictive 'brace predictive-latex-cleveref-restore-label-regexp t)
  (auto-overlay-load-definition
   'predictive predictive-latex-cleveref-restore-label-definition)
  (kill-local-variable 'predictive-latex-cleveref-restore-vref-regexp)
  (kill-local-variable 'predictive-latex-cleveref-restore-label-regexp)
  (kill-local-variable 'predictive-latex-cleveref-restore-label-definition))



(defun predictive-latex-cleveref-label-forward-word (&optional n)
  ;; going backwards...
  (if (and n (< n 0))
      (unless (bobp)
	(setq n (- n))
	(when (= ?\\ (char-before))
	  (while (= ?\\ (char-before)) (backward-char))
	  (setq n (1- n)))
	(dotimes (i n)
	  (when (and (char-before) (= (char-syntax (char-before)) ?w))
	    (backward-word 1))  ; argument not optional in Emacs 21
	  (while (and (char-before)
		      (or (= (char-syntax (char-before)) ?w)
			  (= (char-syntax (char-before)) ?_)
			  (and (= (char-syntax (char-before)) ?.)
			       (/= (char-before) ?,)
			       (/= (char-before) ?{))))
	    (backward-char))))
    ;; going forwards...
    (unless (eobp)
      (setq n (if n n 1))
      (dotimes (i n)
	(when (and (char-after) (= (char-syntax (char-after)) ?w))
	  (forward-word 1))  ; argument not optional in Emacs 21
	(while (and (char-after)
		    (or (= (char-syntax (char-after)) ?w)
			(= (char-syntax (char-after)) ?_)
			(and (= (char-syntax (char-after)) ?.)
			     (/= (char-after) ?,)
			     (/= (char-after) ?}))))
	  (forward-char))))
;;; 	(if (re-search-forward "\\(\\w\\|\\s_\\|\\s.\\)+" nil t)
;;; 	    (when (= (char-before) ?,) (backward-char))
;;; 	  (goto-char (point-max)))))
    )
)

;;; predictive-latex-cleveref ends here
