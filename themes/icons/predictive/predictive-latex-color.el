
;;; predictive-latex-color.el --- predictive mode LaTeX color package support


;; Copyright (C) 2008 Toby Cubitt

;; Author: Toby Cubitt <toby-predictive@dr-qubit.org>
;; Version: 0.2.1
;; Keywords: predictive, latex, package, color
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
(provide 'predictive-latex-color)

;; add load and unload functions to alist
(push '("color" predictive-latex-load-color
	predictive-latex-unload-color)
      predictive-latex-usepackage-functions)



(defun predictive-latex-load-color ()
  ;; Load colour dictionary
  (predictive-load-dict 'dict-latex-colours)
  ;; add new browser sub-menu definition
  (make-local-variable 'predictive-latex-browser-submenu-alist)
  (push (cons "\\\\\\(text\\|page\\|\\)color" 'dict-latex-colours)
	predictive-latex-browser-submenu-alist)
  ;; Load regexps
  (auto-overlay-load-regexp
   'predictive 'brace
   `(("\\([^\\]\\|^\\)\\(\\\\\\\\\\)*\\(\\\\\\(\\|text\\|page\\)color\\(\\[.*?\\]\\)?{\\)" . 3)
     :edge start
     :id color
     (dict . dict-latex-colours)
     (priority . 40)
     (face . (background-color . ,predictive-overlay-debug-color)))
   t))



(defun predictive-latex-unload-color ()
  ;; remove browser sub-menu definition
  (setq predictive-latex-browser-submenu-alist
	(predictive-assoc-delete-all "\\\\\\(text\\|page\\|\\)color"
				     predictive-latex-browser-submenu-alist))
  ;; unload regexps
  (auto-overlay-unload-regexp 'predictive 'brace 'color)
;;;   (auto-overlay-unload-regexp 'predictive 'brace 'textcolor)
;;;   (auto-overlay-unload-regexp 'predictive 'brace 'pagecolor)
  ;; unload colour dictionary
  (predictive-unload-dict 'dict-latex-colours))


;;; predictive-latex-color ends here
