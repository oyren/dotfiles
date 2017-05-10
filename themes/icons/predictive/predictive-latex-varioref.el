
;;; predictive-latex-varioref.el --- predictive mode LaTeX varioref
;;;                                  package support


;; Copyright (C) 2009, 2012 Toby Cubitt

;; Author: Toby Cubitt <toby-predictive@dr-qubit.org>
;; Version: 0.2.1
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
(provide 'predictive-latex-varioref)

;; add load and unload functions to alist
;;(assoc-delete-all "cleveref" predictive-latex-usepackage-functions)
(push '("varioref" predictive-latex-load-varioref
	predictive-latex-unload-varioref)
      predictive-latex-usepackage-functions)



(defun predictive-latex-load-varioref ()
  ;; load cleveref regexps
  (destructuring-bind (word-resolve word-complete word-insert
		       punct-resolve punct-complete punct-insert
		       whitesp-resolve whitesp-complete whitesp-insert)
      (append (auto-completion-lookup-behaviour nil ?w)
	      (auto-completion-lookup-behaviour nil ?.)
	      (auto-completion-lookup-behaviour nil ? ))

    ;; add new browser sub-menu definition
    (make-local-variable 'predictive-latex-browser-submenu-alist)
    (push (cons "\\\\[vV]\\(\\|page\\)ref\\(range\\|\\)"
		'predictive-latex-label-dict)
	  predictive-latex-browser-submenu-alist)
    (push (cons "\\\\fullref" 'predictive-latex-label-dict)
	  predictive-latex-browser-submenu-alist)

    ;; load regexps
    ;; \vref
    (auto-overlay-load-regexp
     'predictive 'brace
     `(("\\([^\\]\\|^\\)\\(\\\\\\\\\\)*\\(\\\\[vV]\\(\\|page\\)ref\\(\\|range\\)\\*?{\\)" . 3)
       :edge start
       :id vref
       (dict . predictive-latex-label-dict)
       (priority . 40)
       (completion-menu . predictive-latex-construct-browser-menu)
       (auto-completion-syntax-alist
	. ((?w . (add
		  (lambda ()
		    (let ((pos (point)))
		      (when (and
			     (re-search-forward
			      "[^}]*?}" (line-end-position) t)
			     (= (match-beginning 0) pos))
			(backward-char)
			(delete-region pos (point)))
		      (goto-char pos))
		    ',word-complete)
		  t))
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
    ;; \fullref
    (auto-overlay-load-regexp
     'predictive 'brace
     `(("\\([^\\]\\|^\\)\\(\\\\\\\\\\)*\\(\\\\fullref{\\)" . 3)
       :edge start
       :id fullref
       (dict . predictive-latex-label-dict)
       (priority . 40)
       (completion-menu . predictive-latex-construct-browser-menu)
       (auto-completion-syntax-alist . ((?w . (add ,word-complete))
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
    ))



(defun predictive-latex-unload-varioref ()
  ;; remove browser sub-menu definition
  (setq predictive-latex-browser-submenu-alist
	(predictive-assoc-delete-all "\\\\[vV]\\(\\|page\\)ref\\(range\\|\\)"
				     predictive-latex-browser-submenu-alist))
  (setq predictive-latex-browser-submenu-alist
	(predictive-assoc-delete-all "\\\\fullref"
				     predictive-latex-browser-submenu-alist))
  ;; Unload cleveref regexps
  (auto-overlay-unload-regexp 'predictive 'brace 'vref)
  (auto-overlay-unload-regexp 'predictive 'brace 'fullref))


;;; predictive-latex-varioref ends here
