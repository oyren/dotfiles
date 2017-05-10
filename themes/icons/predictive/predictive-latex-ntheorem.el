
;;; predictive-latex-ntheorem.el --- predictive mode LaTeX ntheorem
;;;                                  package support


;; Copyright (C) 2008, 2012 Toby Cubitt

;; Author: Toby Cubitt <toby-predictive@dr-qubit.org>
;; Version: 0.1.2
;; Keywords: predictive, latex, package, ntheorem
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
(provide 'predictive-latex-ntheorem)

;; add load and unload functions to alist
;;(assoc-delete-all "cleveref" predictive-latex-usepackage-functions)
(push '("ntheorem" predictive-latex-load-ntheorem
	predictive-latex-unload-ntheorem)
      predictive-latex-usepackage-functions)


(defun predictive-latex-load-ntheorem ()
  ;; load ntheorem regexps

  (destructuring-bind (word-resolve word-complete word-insert
		       punct-resolve punct-complete punct-insert
		       whitesp-resolve whitesp-complete whitesp-insert)
      (append (auto-completion-lookup-behaviour nil ?w)
	      (auto-completion-lookup-behaviour nil ?.)
	      (auto-completion-lookup-behaviour nil ? ))

    ;; \thref
    (auto-overlay-load-regexp
     'predictive 'brace
     `(("\\([^\\]\\|^\\)\\(\\\\\\\\\\)*\\(\\\\thref{\\)" . 3)
       :edge start
       :id thref
       (dict . predictive-latex-label-dict)
       (priority . 40)
       (completion-menu . predictive-latex-construct-browser-menu)
       (completion-word-thing . predictive-latex-cleveref-label-word)
       (auto-completion-syntax-alist . ((?w . (add ,word-complete))
       					(?_ . (add ,word-complete))
       					(?  . (whitesp-resolve none))
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

    ;; \newshadedtheorem
    (auto-overlay-load-definition
     'predictive
     `(word
       :id newshadedtheorem
       (("\\([^\\]\\|^\\)\\(\\\\\\\\\\)*\\\\newshadedtheorem{\\(.*?\\)}" . 3)
	(auto-dict . predictive-latex-local-env-dict))))

    ;; \newframedtheorem
    (auto-overlay-load-definition
     'predictive
     `(word
       :id newframedtheorem
       (("\\([^\\]\\|^\\)\\(\\\\\\\\\\)*\\\\newframedtheorem{\\(.*?\\)}" . 3)
	(auto-dict . predictive-latex-local-env-dict))))
    ))



(defun predictive-latex-unload-ntheorem ()
  ;; Unload cleveref regexps
  (auto-overlay-unload-regexp 'predictive 'brace 'thref)
  (auto-overlay-unload-definition 'predictive 'newshadedtheorem)
  (auto-overlay-unload-definition 'predictive 'newframedtheorem)
)

;;; predictive-latex-ntheorem ends here
