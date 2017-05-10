
;;; predictive-html.el --- predictive mode HTML setup function


;; Copyright (C) 2005 2008-2010, 2012 Toby Cubitt

;; Author: Toby Cubitt
;; Version: 0.5.3
;; Keywords: predictive, setup function, html

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

(require 'predictive)
(require 'auto-overlay-nested)
(require 'auto-overlay-flat)

(provide 'predictive-html)
(add-to-list 'predictive-major-mode-alist
	     '(html-mode . predictive-setup-html))



;; variables used to restore local settings of variables when predictive mode
;; is disabled in a Html buffer
(defvar predictive-restore-override-syntax-alist nil)
(make-variable-buffer-local 'predictive-restore-override-syntax-alist)

;; set up 'predictive-latex-word to be a `thing-at-point' symbol
(put 'predictive-html-word 'forward-op 'predictive-html-forward-word)


;; html dictionaries
(defconst predictive-html-dicts
  '(dict-html dict-html-char-entity dict-html-common
	      dict-html-core dict-html-events
	      dict-html-international dict-html-a dict-html-area
	      dict-html-base dict-html-quote dict-html-body
	      dict-html-button dict-html-col dict-html-del
	      dict-html-form dict-html-head dict-html-img
	      dict-html-input dict-html-ins dict-html-label
	      dict-html-legend dict-html-link dict-html-map
	      dict-html-meta dict-html-object dict-html-optgroup
	      dict-html-option dict-html-param dict-html-script
	      dict-html-select dict-html-style dict-html-table
	      dict-html-td dict-html-textarea dict-html-tr))


;; background color for certain auto-overlays to aid debugging
(defvar predictive-overlay-debug-color nil)



(defun predictive-setup-html (arg)
  "With a positive ARG, set up predictive mode for use with html major modes.
With a negative ARG, undo these changes. Called when predictive
mode is enabled via entry in `predictive-major-mode-alist'."

  (cond
   ;; ----- enabling html setup -----
   ((> arg 0)
    (catch 'load-fail

      ;; load the dictionaries
      (mapc (lambda (dic)
	      (unless (predictive-load-dict dic)
		(message "Failed to load dictionary %s" dic)
		(throw 'load-fail nil)))
	    predictive-html-dicts)

      ;; set html dictionaries to be used alongside main dictionary
      (setq predictive-auxiliary-dict '(dict-html dict-html-char-entity))

      ;; save overlays along with buffer
      (add-hook 'after-save-hook 'predictive-html-after-save nil t)
      (add-hook 'kill-buffer-hook 'predictive-html-kill-buffer nil t)

      ;; use html browser menu if first character of prefix is "<" or "&"
      (set (make-local-variable 'predictive-menu-function)
	   (lambda (overlay)
	     (if (or (string= (substring (overlay-get overlay 'prefix) 0 1)
			      "<")
		     (string= (substring (overlay-get overlay 'prefix) 0 1)
			      "&"))
		 (predictive-html-construct-browser-menu overlay)
	       (completion-construct-menu overlay))))

      ;; delete any existing predictive auto-overlay regexps and load html
      ;; auto-overlay regexps
      (auto-overlay-unload-set 'predictive)
      (predictive-html-load-regexps)
      (auto-overlay-start 'predictive)

      ;; load the keybindings and related settings
      (predictive-html-load-keybindings)
      ;; consider \ as start of a word
      (setq predictive-word-thing 'predictive-html-word)

      t))  ; indicate successful setup


   ;; ----- disabling html setup -----
   ((< arg 0)
        ;; stop predictive auto overlays
    (auto-overlay-stop 'predictive nil 'save)
    (auto-overlay-unload-set 'predictive)
    ;; restore predictive-main-dict to saved setting
    (kill-local-variable 'predictive-auxiliary-dict)
    ;; restore completion-dynamic-override-syntax-alist to saved setting
    (kill-local-variable 'completion-dynamic-override-syntax-alist)
    (setq auto-completion-override-syntax-alist
	  predictive-restore-override-syntax-alist)
    (kill-local-variable 'predictive-restore-override-syntax-alist)
    ;; remove other local variable settings
    (kill-local-variable 'predictive-menu-function)
    ;; remove hook functions that save overlays
    (remove-hook 'after-save-hook 'predictive-html-after-save t)
    (remove-hook 'kill-buffer-hook 'predictive-html-kill-buffer t)

    t)))  ; indicate successful reversion of changes



(defun predictive-html-load-regexps ()
  "Load the predictive mode html auto-overlay regexp decinitions."

  ;; "<!--" and "-->" delimit comments
  (auto-overlay-load-definition
   'predictive
   `(flat :id comment
	  ("<!--"
	   :edge start
	   (dict . predictive-main-dict)
	   (priority . 20)
	   (face . (background-color . ,predictive-overlay-debug-color)))
	  ("-->"
	   :edge end
	   (dict . predictive-main-dict)
	   (priority . 20)
	   (face . (background-color . ,predictive-overlay-debug-color)))
	  ))

  ;; "<..." starts various tags, ended by ">". "<" makes sure all other ">"s
  ;; are matched
  (auto-overlay-load-definition
   'predictive
   `(nested :id tag
	    ("<a\\( \\|$\\)"
	     :edge start
	     (dict . (dict-html-a dict-html-common))
	     (priority . 10)
	     (face . (background-color . ,predictive-overlay-debug-color)))
	    ("<area\\( \\|$\\)"
	     :edge start
	     (dict . (dict-html-area dict-html-common))
	     (priority . 10)
	     (face . (background-color . ,predictive-overlay-debug-color)))
	    ("<base\\( \\|$\\)"
	     :edge start
	     (dict . dict-html-base)
	     (priority . 10)
	     (face . (background-color . ,predictive-overlay-debug-color)))
	    ("<bdo\\( \\|$\\)"
	     :edge start
	     (dict . (dict-html-international dict-html-core))
	     (priority . 10)
	     (face . (background-color . ,predictive-overlay-debug-color)))
	    ("<\\(blockquote\\|q\\)\\( \\|$\\)"
	     :edge start
	     (dict . (dict-html-quote dict-html-common))
	     (priority . 10)
	     (face . (background-color . ,predictive-overlay-debug-color)))
	    ("<body\\( \\|$\\)"
	     :edge start
	     (dict . (dict-html-body dict-html-common))
	     (priority . 10)
	     (face . (background-color . ,predictive-overlay-debug-color)))
	    ("<br\\( \\|$\\)"
	     :edge start
	     (dict . dict-html-core)
	     (priority . 10)
	     (face . (background-color . ,predictive-overlay-debug-color)))
	    ("<button\\( \\|$\\)"
	     :edge start
	     (dict . (dict-html-button dict-html-common))
	     (priority . 10)
	     (face . (background-color . ,predictive-overlay-debug-color)))
	    ("<col\\( \\|$\\)"
	     :edge start
	     (dict . (dict-html-col dict-html-common))
	     (priority . 10)
	     (face . (background-color . ,predictive-overlay-debug-color)))
	    ("<colgroup\\( \\|$\\)"
	     :edge start
	     (dict . (dict-html-col dict-html-common))
	     (priority . 10)
	     (face . (background-color . ,predictive-overlay-debug-color)))
	    ("<del\\( \\|$\\)"
	     :edge start
	     (dict . (dict-html-del dict-html-common))
	     (priority . 10)
	     (face . (background-color . ,predictive-overlay-debug-color)))
	    ("<form\\( \\|$\\)"
	     :edge start
	     (dict . (dict-html-form dict-html-common))
	     (priority . 10)
	     (face . (background-color . ,predictive-overlay-debug-color)))
	    ("<head\\( \\|$\\)"
	     :edge start
	     (dict . (dict-html-head dict-html-international))
	     (priority . 10)
	     (face . (background-color . ,predictive-overlay-debug-color)))
	    ("<hr\\( \\|$\\)"
	     :edge start
	     (dict . (dict-html-core dict-html-events))
	     (priority . 10)
	     (face . (background-color . ,predictive-overlay-debug-color)))
	    ("<html\\( \\|$\\)"
	     :edge start
	     (dict . dict-html-international)
	     (priority . 10)
	     (face . (background-color . ,predictive-overlay-debug-color)))
	    ("<img\\( \\|$\\)"
	     :edge start
	     (dict . (dict-html-img dict-html-common))
	     (priority . 10)
	     (face . (background-color . ,predictive-overlay-debug-color)))
	    ("<input\\( \\|$\\)"
	     :edge start
	     (dict . (dict-html-input dict-html-common))
	     (priority . 10)
	     (face . (background-color . ,predictive-overlay-debug-color)))
	    ("<ins\\( \\|$\\)"
	     :edge start
	     (dict . (dict-html-ins dict-html-common))
	     (priority . 10)
	     (face . (background-color . ,predictive-overlay-debug-color)))
	    ("<label\\( \\|$\\)"
	     :edge start
	     (dict . (dict-html-label dict-html-common))
	     (priority . 10)
	     (face . (background-color . ,predictive-overlay-debug-color)))
	    ("<legend\\( \\|$\\)"
	     :edge start
	     (dict . (dict-html-legend dict-html-common))
	     (priority . 10)
	     (face . (background-color . ,predictive-overlay-debug-color)))
	    ("<link\\( \\|$\\)"
	     :edge start
	     (dict . (dict-html-link dict-html-common))
	     (priority . 10)
	     (face . (background-color . ,predictive-overlay-debug-color)))
	    ("<map\\( \\|$\\)"
	     :edge start
	     (dict . (dict-html-map dict-html-common))
	     (priority . 10)
	     (face . (background-color . ,predictive-overlay-debug-color)))
	    ("<meta\\( \\|$\\)"
	     :edge start
	     (dict . (dict-html-meta dict-html-international))
	     (priority . 10)
	     (face . (background-color . ,predictive-overlay-debug-color)))
	    ("<object\\( \\|$\\)"
	     :edge start
	     (dict . (dict-html-object dict-html-common))
	     (priority . 10)
	     (face . (background-color . ,predictive-overlay-debug-color)))
	    ("<optgroup\\( \\|$\\)"
	     :edge start
	     (dict . (dict-html-optgroup dict-html-common))
	     (priority . 10)
	     (face . (background-color . ,predictive-overlay-debug-color)))
	    ("<option\\( \\|$\\)"
	     :edge start
	     (dict . (dict-html-option dict-html-common))
	     (priority . 10)
	     (face . (background-color . ,predictive-overlay-debug-color)))
	    ("<param\\( \\|$\\)"
	     :edge start
	     (dict . dict-html-param)
	     (priority . 10)
	     (face . (background-color . ,predictive-overlay-debug-color)))
	    ("<script\\( \\|$\\)"
	     :edge start
	     (dict . dict-html-script)
	     (priority . 10)
	     (face . (background-color . ,predictive-overlay-debug-color)))
	    ("<select\\( \\|$\\)"
	     :edge start
	     (dict . (dict-html-select dict-html-common))
	     (priority . 10)
	     (face . (background-color . ,predictive-overlay-debug-color)))
	    ("<style\\( \\|$\\)"
	     :edge start
	     (dict . (dict-html-style dict-html-international))
	     (priority . 10)
	     (face . (background-color . ,predictive-overlay-debug-color)))
	    ("<table\\( \\|$\\)"
	     :edge start
	     (dict . (dict-html-table dict-html-common))
	     (priority . 10)
	     (face . (background-color . ,predictive-overlay-debug-color)))
	    ("<t\\(r\\|body\\|head\\|foot\\)\\( \\|$\\)"
	     :edge start
	     (dict . (dict-html-tr dict-html-common))
	     (priority . 10)
	     (face . (background-color . ,predictive-overlay-debug-color)))
	    ("<t[dh]\\( \\|$\\)"
	     :edge start
	     (dict . (dict-html-td dict-html-common))
	     (priority . 10)
	     (face . (background-color . ,predictive-overlay-debug-color)))
	    ("<textarea\\( \\|$\\)"
	     :edge start
	     (dict . (dict-html-textarea dict-html-common))
	     (priority . 10)
	     (face . (background-color . ,predictive-overlay-debug-color)))
	    ("<title\\( \\|$\\)"
	     :edge start
	     (dict . dict-html-international)
	     (priority . 10)
	     (face . (background-color . ,predictive-overlay-debug-color)))
	    ("<[[:alnum:]]+?\\( \\|$\\)"
	     :edge start
	     (dict . dict-html-common)
	     (priority . 10)
	     (face . (background-color . ,predictive-overlay-debug-color)))
	    (">"
	     :edge end
	     (priority . 10))
	    )))



(defun predictive-html-load-keybindings ()
  "Load the predictive mode html key bindings."
  ;; make "<", ">", and "&" do the right thing
  (setq predictive-restore-override-syntax-alist
	auto-completion-override-syntax-alist)
  (make-local-variable 'completion-dynamic-override-syntax-alist)
  (setq auto-completion-override-syntax-alist
	(append '((?< . (accept word))
		  (?> . (accept none))
		  (?& . (accept word)))
		auto-completion-override-syntax-alist)))



(defun predictive-html-kill-buffer ()
  ;; Function called from `kill-buffer-hook' to tidy things up
  ;; save overlays if buffer was saved
  (unless (buffer-modified-p)
    (auto-overlay-save-overlays 'predictive nil
				predictive-auxiliary-file-location)))



(defun predictive-html-after-save ()
  ;; Function called from `after-save-hook'
  (auto-overlay-save-overlays 'predictive nil
			      predictive-auxiliary-file-location))



(defun predictive-html-reparse-buffer ()
  "Clear all auto-overlays, then reparse buffer from scratch."
  (interactive)

  ;; restart the predictive auto-overlays without saving to file
  (auto-overlay-stop 'predictive)
  (auto-overlay-start 'predictive nil 'ignore-save-file))



(defun predictive-html-construct-browser-menu (overlay)
  "Construct the html browser menu keymap."

  ;; construct menu, dropping the last two entries which are a separator and a
  ;; link back to the basic completion menu (would just redisplay this menu,
  ;; since we're using the browser as the default menu)
  (let ((menu (completion-construct-browser-menu
	       overlay 'completion-browser-menu-item)))
    (setq menu (butlast menu 2))))



(defun predictive-html-forward-word (&optional n)
  ;; going backwards...
  (if (and n (< n 0))
      (unless (bobp)
	(dotimes (i (- n))
	  ;; make sure we're at the end of a word
	  (when (re-search-backward "<\\|\\w\\|>" nil t)
	    (forward-char))
	  (when (= (char-before) ?>) (backward-char))
	  (when (= (char-syntax (char-before)) ?w) (backward-word 1))
	  (when (= (char-before) ?<) (backward-char))
	  ))
    ;; going forwards...
    (unless (eobp)
      (re-search-forward "<\\w*>\\|<\\w*\\|\\w*>\\|\\w+" nil t (or n 1))))
)


;;; predictive-setup-html.el ends here
