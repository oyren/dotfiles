;;; predictive-texinfo.el --- predictive mode Texinfo setup function


;; Copyright (C) 2008, 2010-2012 Toby Cubitt

;; Author: Toby Cubitt <toby-predictive@dr-qubit.org>
;; Version: 0.3.5
;; Keywords: predictive, setup function, texinfo
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

(eval-when-compile (require 'cl))
(require 'predictive)
(require 'auto-overlay-word)
(require 'auto-overlay-line)
(require 'auto-overlay-self)
(require 'auto-overlay-nested)
(require 'predictive-latex)  ; we use dict-latex-math etc. for equations

(provide 'predictive-texinfo)
(add-to-list 'predictive-major-mode-alist
	     '(Texinfo-mode . predictive-setup-texinfo))
(add-to-list 'predictive-major-mode-alist
	     '(texinfo-mode . predictive-setup-texinfo))



;;;============================================================
;;;                  Customization Options

(defgroup predictive-texinfo nil
  "Predictive completion mode Texinfo support."
  :group 'predictive)


(defcustom predictive-texinfo-electric-environments nil
  "When enabled, environment names are automatically synchronized
between \\begin{...} and \\end{...} commands."
  :group 'predictive-texinfo
  :type 'boolean)




;;;============================================================
;;;                       Variables

;; set `thing-at-point' symbols
(put 'predictive-texinfo-word 'forward-op 'predictive-texinfo-forward-word)
(put 'predictive-texinfo-node-word 'forward-op
     'predictive-texinfo-node-forward-word)
(put 'predictive-texinfo-node-def-word 'forward-op
     'predictive-texinfo-node-def-forward-word)


;; variables holding dictionaries
(defvar predictive-texinfo-node-dict nil)
(make-variable-buffer-local 'predictive-texinfo-node-dict)
(defvar predictive-texinfo-local-texinfo-dict nil)
(make-variable-buffer-local 'predictive-texinfo-local-texinfo-dict)
(defvar predictive-texinfo-local-flag-dict nil)
(make-variable-buffer-local 'predictive-texinfo-local-flag-dict)


;; variable used to enable `predictive-texinfo-map' minor-mode keymap
;; (we don't use `define-minor-mode' because we explicitly don't want an
;; interactive minor-mode toggling command)
(defvar predictive-texinfo-mode nil)
(make-variable-buffer-local 'predictive-texinfo-mode)

;; keymap used for texinfo-specific `predictive-mode' key bindings
(defvar predictive-texinfo-map (make-sparse-keymap))

(push (cons 'predictive-texinfo-mode predictive-texinfo-map)
      minor-mode-map-alist)

;; override AUCTeX bindings so completion works
(define-key predictive-texinfo-map [?$]  'completion-self-insert)
(define-key predictive-texinfo-map [?\"] 'completion-self-insert)
(define-key predictive-texinfo-map [?_]  'completion-self-insert)
(define-key predictive-texinfo-map [?^]  'completion-self-insert)
(define-key predictive-texinfo-map [?\\] 'completion-self-insert)
(define-key predictive-texinfo-map [?-]  'completion-self-insert)



;; variables used to restore local settings of variables when predictive mode
;; is disabled in a Texinfo buffer
(defvar predictive-restore-override-syntax-alist nil)
(make-variable-buffer-local 'predictive-restore-override-syntax-alist)

;; variable storing filename before saving, to detect renaming
(defvar predictive-texinfo-previous-filename nil)
(make-variable-buffer-local 'predictive-texinfo-previous-filename)


;; variable storing filename before saving, to detect renaming (see
;; `predictive-texinfo-after-save')
(defvar predictive-texinfo-previous-filename nil)
(make-variable-buffer-local 'predictive-texinfo-previous-filename)


;; prevent bogus compiler warnings
(eval-when-compile
  (defvar dict-texinfo)
  (defvar dict-texinfo-env)
  (defvar dict-texinfo-indicating))


;; background color for certain auto-overlays to aid debugging
(defvar predictive-overlay-debug-colour nil)
(defvaralias 'predictive-overlay-debug-color 'predictive-overlay-debug-colour)


;; background color for certain auto-overlays to aid debugging
(defvar predictive-overlay-debug-colour nil)
(defvaralias 'predictive-overlay-debug-color 'predictive-overlay-debug-colour)




;;;=========================================================
;;;                  Setup function

(defun predictive-setup-texinfo (arg)
  "With a positive ARG, set up predictive mode for use with Texinfo major modes.
With a negative ARG, undo these changes. Called when predictive
mode is enabled via entry in `predictive-major-mode-alist'."

  (cond
   ;; ----- enabling Texinfo setup -----
   ((> arg 0)
    (catch 'load-fail

      ;; enable `predictive-texinfo-map' keymap
      (setq predictive-texinfo-mode t)

      ;; save overlays and dictionaries along with buffer
      (add-hook 'after-save-hook 'predictive-texinfo-after-save nil t)
      (add-hook 'kill-buffer-hook 'predictive-texinfo-kill-buffer nil t)

      ;; use Texinfo browser menu if first character of prefix is "@"
      (set (make-local-variable 'predictive-menu-function)
	   (lambda (overlay)
	     (if (string= (substring (overlay-get overlay 'prefix) 0 1) "@")
		 (predictive-texinfo-construct-browser-menu overlay)
	       (completion-construct-menu overlay))))

      ;; load the Texinfo dictionaries
      (mapc (lambda (dic)
	      (unless (predictive-load-dict dic)
		(message "Failed to load %s" dic)
		(throw 'load-fail nil)))
	    ;; FIXME: should create and use separate dict-tex-math instead of
	    ;;        using dict-latex-math
	    '(dict-texinfo dict-texinfo-env dict-texinfo-indicating
			   dict-texinfo-math dict-latex-math))
      ;; load/create the node and local Texinfo dictionaries
      (setq predictive-texinfo-node-dict
	    (predictive-auto-dict-load "texinfo-node")
	    predictive-texinfo-local-texinfo-dict
	    (predictive-auto-dict-load "texinfo-local-texinfo")
	    predictive-texinfo-local-flag-dict
	    (predictive-auto-dict-load "texinfo-local-flag"))

      ;; set Texinfo dictionary to be used alongside main dictionary
      (setq predictive-auxiliary-dict
	    '(dict-texinfo predictive-texinfo-local-texinfo-dict))

      ;; delete any existing predictive auto-overlay regexps and load Texinfo
      ;; auto-overlay regexps
      (auto-overlay-unload-set 'predictive)
      (predictive-texinfo-load-regexps)

      ;; start the auto-overlays
      (auto-overlay-start 'predictive nil predictive-auxiliary-file-location)

      ;; load the keybindings and related settings
      (predictive-texinfo-load-keybindings)
      ;; consider @ as start of a word
      (setq predictive-word-thing 'predictive-texinfo-word)

      t))  ; indicate successful setup


   ((< arg 0)
    ;; disable `predictive-texinfo-map' keymap
    (setq predictive-texinfo-mode nil)
    ;; stop predictive auto overlays
    (auto-overlay-stop 'predictive nil (when (buffer-file-name)
					 predictive-auxiliary-file-location))
    (auto-overlay-unload-set 'predictive)
    ;; restore predictive-main-dict
    (kill-local-variable 'predictive-auxiliary-dict)
    ;; restore `auto-completion-override-syntax-alist' to saved setting
    (kill-local-variable 'auto-completion-override-syntax-alist)
    (setq auto-completion-override-syntax-alist
	  predictive-restore-override-syntax-alist)
    (kill-local-variable 'predictive-restore-override-syntax-alist)
    ;; remove other local variable settings
    (kill-local-variable 'predictive-texinfo-dict)
    (kill-local-variable 'predictive-texinfo-local-texinfo-dict)
    (kill-local-variable 'predictive-menu-function)
    ;; remove hook functions that save overlays etc.
    (remove-hook 'after-save-hook 'predictive-texinfo-after-save t)
    (remove-hook 'kill-buffer-hook 'predictive-texinfo-kill-buffer t)

    t)))  ; indicate successful reversion of changes



(defun predictive-texinfo-load-regexps ()
  "Load the predictive mode Texinfo auto-overlay regexp definitions."

  (destructuring-bind (word-resolve word-complete word-insert
		       punct-resolve punct-complete punct-insert
		       whitesp-resolve whitesp-complete whitesp-insert)
      (append (auto-completion-lookup-behaviour nil ?w)
	      (auto-completion-lookup-behaviour nil ?.)
	      (auto-completion-lookup-behaviour nil ? ))
  ;; (let* ((word-behaviour (completion-lookup-behaviour nil ?w))
  ;; 	 (word-complete (completion-get-completion-behaviour word-behaviour))
  ;; 	 (word-resolve (completion-get-resolve-behaviour word-behaviour))
  ;; 	 (punct-behaviour (completion-lookup-behaviour nil ?.))
  ;; 	 (punct-complete (completion-get-completion-behaviour punct-behaviour))
  ;; 	 (punct-resolve (completion-get-resolve-behaviour punct-behaviour))
  ;; 	 (whitesp-behaviour (completion-lookup-behaviour nil ? ))
  ;; 	 (whitesp-complete (completion-get-completion-behaviour
  ;; 			    whitesp-behaviour))
  ;; 	 (whitesp-resolve (completion-get-resolve-behaviour
  ;; 			   whitesp-behaviour)))

    ;; @c starts comments that last till end of line
    (auto-overlay-load-definition
     'predictive
     `(line :id short-comment
	    ("@c \\|@comment "
	     (dict . predictive-main-dict)
	     (priority . 50)
	     (exclusive . t)
	     (face . (background-color . ,predictive-overlay-debug-colour)))))

    ;; "@ignore ... @end ignore" defines an extended comment
    (auto-overlay-load-definition
     'predictive
     `(flat :id long-comment
	      ("@ignore[[:blank:]]*$"
	       :edge start
	       (dict . predictive-main-dict)
	       (priority . 50)
	       (exclusive . t)
	       (face . (background-color . ,predictive-overlay-debug-colour)))
	      ("@end ignore$"
	       :edge end
	       (dict . predictive-main-dict)
	       (priority . 50)
	       (exclusive . t)
	       (face . (background-color . ,predictive-overlay-debug-colour)))
	      ))


    ;; \<command>{'s do various things in Texinfo. All are ended by } but not
    ;; by \}. The { is included to ensure all { and } match, but @{ and @} are
    ;; excluded
    (auto-overlay-load-definition
     'predictive
     `(nested :id brace
	      (("\\([^@]\\|^\\)\\(@@\\)*\\(@\\(x\\|px\\|info\\)?ref{\\)" . 3)
	       :edge start
	       (dict . predictive-texinfo-node-dict)
	       (priority . 40)
	       (completion-menu-function
		. predictive-texinfo-construct-browser-menu)
	       (completion-word-thing . predictive-texinfo-node-word)
	       (auto-completion-syntax-alist . ((?w . (add ,word-complete))
						(?_ . (add ,word-complete))
						(?  . (,whitesp-resolve none))
						(?. . (add ,word-complete))
						(t  . (reject none))))
	       (auto-completion-override-syntax-alist
		. ((?} . (,punct-resolve none))))
	       (face . (background-color . ,predictive-overlay-debug-colour)))
;;; 	      ("^@\\(x\\|px\\|info\\)?ref{"
;;; 	       :edge start
;;; 	       (dict . predictive-texinfo-node-dict)
;;; 	       (priority . 40)
;;; 	       (completion-menu-function
;;; 		. predictive-texinfo-construct-browser-menu)
;;; 	       (completion-word-thing . predictive-texinfo-node-word)
;;; 	       (auto-completion-syntax-alist . ((?w . (add ,word-complete))
;;; 						(?_ . (add ,word-complete))
;;; 						(?  . (,whitesp-resolve none))
;;; 						(?. . (add ,word-complete))
;;; 						(t  . (reject none))))
;;; 	       (auto-completion-override-syntax-alist
;;; 		. ((?} . (,punct-resolve none))))
;;; 	       (face . (background-color . ,predictive-overlay-debug-colour)))

	      (("\\([^@]\\|^\\)\\(@@\\)*\\(@math{\\)" . 3)
	       :edge start
	       (dict . (dict-texinfo-math dict-latex-math))
	       (priority . 40)
	       (completion-word-thing . predictive-latex-word)
	       (auto-completion-override-syntax-alist
		. ((?\\ . ((lambda ()
			     (if (and (char-before) (= (char-before) ?\\)
				      (or (not (char-before (1- (point))))
					  (not (= (char-before (1- (point)))
						  ?\\))))
				 'add ',punct-resolve))
			   ,word-complete))
		   (?_ . (,punct-resolve none))
		   (?^ . (,punct-resolve none))
		   (?{ . ((lambda ()
			    (if (and (char-before) (= (char-before) ?\\))
				'add ',punct-resolve))
			  (lambda ()
			    (if (and (char-before) (= (char-before) ?\\))
				',punct-complete 'none))))
		   (?} . ((lambda ()
			    (if (and (char-before) (= (char-before) ?\\))
				'add ',punct-resolve))
			  (lambda ()
			    (if (and (char-before) (= (char-before) ?\\))
				',punct-complete 'none))))
		   (?\; . ((lambda ()
			     (if (and (char-before) (= (char-before) ?\\))
				 'add ',punct-resolve))
			   (lambda ()
			     (if (and (char-before (1- (point)))
				      (= (char-before (1- (point))) ?\\))
				 ',punct-complete 'none))))
		   (?! . ((lambda ()
			    (if (and (char-before) (= (char-before) ?\\))
				'add ',punct-resolve))
			  (lambda ()
			    (if (and (char-before (1- (point)))
				     (= (char-before (1- (point))) ?\\))
				',punct-complete 'none))))))
	       (face . (background-color . ,predictive-overlay-debug-colour)))

;;; 	      ("^@math{"
;;; 	       :edge start
;;; 	       (dict . (dict-texinfo-math dict-latex-math))
;;; 	       (priority . 40)
;;; 	       (completion-word-thing . predictive-latex-word)
;;; 	       (auto-completion-override-syntax-alist
;;; 		. ((?\\ . ((lambda ()
;;; 			     (if (and (char-before) (= (char-before) ?\\)
;;; 				      (or (not (char-before (1- (point))))
;;; 					  (not (= (char-before (1- (point)))
;;; 						  ?\\))))
;;; 				 'add ',punct-resolve))
;;; 			   ,word-complete))
;;; 		   (?_ . (,punct-resolve none))
;;; 		   (?^ . (,punct-resolve none))
;;; 		   (?{ . ((lambda ()
;;; 			    (if (and (char-before) (= (char-before) ?\\))
;;; 				'add ',punct-resolve))
;;; 			  (lambda ()
;;; 			    (if (and (char-before) (= (char-before) ?\\))
;;; 				',punct-complete 'none))))
;;; 		   (?} . ((lambda ()
;;; 			    (if (and (char-before) (= (char-before) ?\\))
;;; 				'add ',punct-resolve))
;;; 			  (lambda ()
;;; 			    (if (and (char-before) (= (char-before) ?\\))
;;; 				',punct-complete 'none))))
;;; 		   (?\; . ((lambda ()
;;; 			     (if (and (char-before) (= (char-before) ?\\))
;;; 				 'add ',punct-resolve))
;;; 			   (lambda ()
;;; 			     (if (and (char-before (1- (point)))
;;; 				      (= (char-before (1- (point))) ?\\))
;;; 				 ',punct-complete 'none))))
;;; 		   (?! . ((lambda ()
;;; 			    (if (and (char-before) (= (char-before) ?\\))
;;; 				'add ',punct-resolve))
;;; 			  (lambda ()
;;; 			    (if (and (char-before (1- (point)))
;;; 				     (= (char-before (1- (point))) ?\\))
;;; 				',punct-complete 'none))))))
;;; 	       (face . (background-color . ,predictive-overlay-debug-colour)))

	      (("\\([^@]\\|^\\)\\(@@\\)*\\(@value{\\)" . 3)
	       :edge start
	       (dict . dict-texinfo-flag) (priority . 40)
	       (face . (background-color . ,predictive-overlay-debug-colour)))
;;; 	      ("^@value{"
;;; 	       :edge start
;;; 	       (dict . dict-texinfo-flag) (priority . 40)
;;; 	       (face . (background-color . ,predictive-overlay-debug-colour)))

	      ;; Note: the following regexps are complicated because they have
	      ;; to check whether number of @'s in front of { is even or
	      ;; odd.
	      (("\\([^@]\\|^\\)\\(@@\\)*\\({\\)" . 3)
	       :edge start
	       (priority . 40)
	       (face . (background-color . ,predictive-overlay-debug-colour)))
;;; 	      (("^\\({\\)" . 1)
;;; 	       :edge start
;;; 	       (priority . 40)
;;; 	       (face . (background-color . ,predictive-overlay-debug-colour)))
	      (("\\([^@]\\|^\\)\\(@@\\)*\\(}\\)" . 3)
	       :edge end
	       (priority . 40)
	       (face . (background-color . ,predictive-overlay-debug-colour)))
;;; 	      (("^\\(}\\)" . 1)
;;; 	       :edge end
;;; 	       (priority . 40)
;;; 	       (face . (background-color . ,predictive-overlay-debug-colour)))
	      ))


    ;; @end ends a Texinfo environment (the second definition deals with
    ;; overlays that have to extend to the end of the line)
    (auto-overlay-load-definition
     'predictive
     `(line
       :id environment
       ("@end "
	(priority . 10)
	(dict . dict-texinfo-env)
	(face . (background-color . ,predictive-overlay-debug-colour)))))
;;;     (auto-overlay-load-definition
;;;      'predictive
;;;      `(word
;;;        :id environment
;;;        (("@end \\([[:alpha:]]*\\)[^[:alpha:]]" . 1)
;;; 	(priority . 10)
;;; 	(dict . dict-texinfo-env)
;;; 	(face . (background-color . ,predictive-overlay-debug-colour)))))
;;;     (auto-overlay-load-definition
;;;      'predictive
;;;      `(word
;;;        :id environment-eol
;;;        (("@end \\([[:alpha:]]*\\)$" . 1)
;;; 	(priority . 10)
;;; 	(dict . dict-texinfo-env)
;;; 	(face . (background-color . ,predictive-overlay-debug-colour)))))


    ;; @table, @vtable and @ftable define two-column tables, and should be
    ;; followed by a Texinfo "indicating" command (the second definition deals
    ;; with overlays that have to extend to the end of the line)
    (auto-overlay-load-definition
     'predictive
     `(word
       :id table
       (("@[vf]?table \\([[:alpha:]]*\\)[^[:alpha:]]" . 1)
	(priority . 10)
	(dict . dict-texinfo-indicating)
	(face . (background-color . ,predictive-overlay-debug-colour)))))
    (auto-overlay-load-definition
     'predictive
     `(word
       :id table-eol
       (("@[vf]?table \\([[:alpha:]]*\\)$" . 1)
	(priority . 10)
	(dict . dict-texinfo-indicating)
	(face . (background-color . ,predictive-overlay-debug-colour)))))


    ;; @node and @anchor create cross-reference labels. Through the use of a
    ;; special "auto-dict" regexp class defined below, this automagically adds
    ;; the label to the label dictionary.
    (auto-overlay-load-definition
     'predictive
     '(predictive-auto-dict
       :id node
       (("@node \\(.*?\\)\\(,.*\\|$\\)" . 1)
	(auto-dict . predictive-texinfo-node-dict))))
    (auto-overlay-load-definition
     'predictive
     '(predictive-auto-dict
       :id anchor
       (("@anchor{\\(.*?\\)}" . 1)
	(auto-dict . predictive-texinfo-node-dict))))


    ;; the other optional arguments of @node are references to other nodes
    (auto-overlay-load-definition
     'predictive
     '(word
       :id node-args1
       (("@node [^,]*,\\([^,]*\\)\\($\\|,\\)" . 1)
	(dict . predictive-texinfo-node-dict))))
    (auto-overlay-load-definition
     'predictive
     '(word
       :id node-args2
       (("@node \\([^,]*,\\)\\{2\\}\\([^,]*\\)\\($\\|,\\)" . 2)
	(dict . predictive-texinfo-node-dict))))
    (auto-overlay-load-definition
     'predictive
     '(word
       :id node-args3
       (("@node \\([^,]*,\\)\\{3\\}\\([^,]*\\)\\($\\|,\\)" . 2)
	(dict . predictive-texinfo-node-dict))))


    ;; @macro and @rmacro define new Texinfo macros. Through the use of a
    ;; special "auto-dict" regexp class defined below, this automagically adds
    ;; the command to the Texinfo dictionary.
    (auto-overlay-load-definition
     'predictive
     '(predictive-auto-dict
       :id macro
       (("@r?macro \\(.*?\\)[[:blank:]]*\\({\\|$\\)" . 1)
	(auto-dict . predictive-texinfo-local-texinfo-dict))))

    ;; @alias defines a new alias to a Texinfo macro
    (auto-overlay-load-definition
     'predictive
     '(predictive-auto-dict
       :id alias
       (("@alias \\(.*?\\)[[:blank:]]*=" . 1)
	(auto-dict . predictive-texinfo-local-texinfo-dict))))


    ;; @set defines and/or sets a flag. Through the use of a special "auto-dict"
    ;; regexp class defined below, this automagically adds the command to the
    ;; flags dictionary.
    (auto-overlay-load-definition
     'predictive
     '(predictive-auto-dict
       :id set
       (("@set \\(.*\\)\\([^ ]\\|$\\)" . 1)
	(auto-dict . predictive-texinfo-flag-dict))))


    ;; @clear, @ifset and @ifclear refer to flags defined by @set
    (auto-overlay-load-definition
     'predictive
     '(word
       :id clear
       (("@clear \\(.*\\)\\([^ ]\\|$\\)" . 1)
	(dict . predictive-texinfo-flag-dict))))
    (auto-overlay-load-definition
     'predictive
     '(word
       :id ifset
       (("@ifset \\(.*\\)\\([^ ]\\|$\\)" . 1)
	(dict . predictive-texinfo-flag-dict))))
    (auto-overlay-load-definition
     'predictive
     '(word
       :id ifclear
       (("@ifclear \\(.*\\)\\([^ ]\\|$\\)" . 1)
	(dict . predictive-texinfo-flag-dict))))
    ))



(defun predictive-texinfo-load-keybindings ()
  "Load the predictive mode Texinfo key bindings."
  (setq predictive-restore-override-syntax-alist
	auto-completion-override-syntax-alist)
  (make-local-variable 'auto-completion-override-syntax-alist)

  ;; get behaviours defined in `auto-completion-syntax-alist'
  (destructuring-bind (word-resolve word-complete word-insert
		       punct-resolve punct-complete punct-insert
		       whitesp-resolve whitesp-complete whitesp-insert)
      (append (auto-completion-lookup-behaviour nil ?w 'predictive)
	      (auto-completion-lookup-behaviour nil ?. 'predictive)
	      (auto-completion-lookup-behaviour nil ?  'predictive))
  ;; (let* ((word-behaviour (completion-lookup-behaviour nil ?w))
  ;; 	 (word-complete (completion-get-completion-behaviour word-behaviour))
  ;; 	 (word-resolve (completion-get-resolve-behaviour word-behaviour))
  ;; 	 (punct-behaviour (completion-lookup-behaviour nil ?.))
  ;; 	 (punct-complete (completion-get-completion-behaviour punct-behaviour))
  ;; 	 (punct-resolve (completion-get-resolve-behaviour punct-behaviour))
  ;; 	 (whitesp-behaviour (completion-lookup-behaviour nil ? ))
  ;; 	 (whitesp-complete (completion-get-completion-behaviour
  ;; 			    whitesp-behaviour))
  ;; 	 (whitesp-resolve (completion-get-resolve-behaviour
  ;; 			   whitesp-behaviour)))

    ;; make "\", "$", "{" and "}" do the right thing
    (setq auto-completion-override-syntax-alist
	  (append
	   `((?@ . ((lambda ()
		      (if (and (char-before) (= (char-before) ?@)
			       (or (not (char-before (1- (point))))
				   (not (= (char-before (1- (point)))
					   ?@))))
			  'add ',punct-resolve))
		    ,word-complete))
	     (?  . ((lambda ()
		      (if (and (char-before) (= (char-before) ?\\))
			  'add ',whitesp-resolve))
		    (lambda ()
		      (cond
		       ((auto-overlays-at-point
			 nil '((lambda (dic) (eq dic 'dict-texinfo-env))
			       dict))
			(complete-in-buffer "" nil nil nil 'auto)
			'none)
		       ((and (char-before) (= (char-before) ?\\))
			',word-complete)
		       (t 'none)))))
	     (?{ . ((lambda ()
		      (if (and (char-before) (= (char-before) ?@))
			  'add ',punct-resolve))
		    (lambda ()
		      (if (and (char-before) (= (char-before) ?@))
			  ',punct-complete 'none))))
	     (?} . ((lambda ()
		      (if (and (char-before) (= (char-before) ?@))
			  'add ',punct-resolve))
		    (lambda ()
		      (if (and (char-before) (= (char-before) ?@))
			  ',punct-complete 'none))))
	     (?! . ((lambda ()
		      (if (and (char-before) (= (char-before) ?@))
			  'add ',punct-resolve))
		    (lambda ()
		      (if (and (char-before (1- (point)))
			       (= (char-before (1- (point))) ?@))
			  ',punct-complete 'none))))
	     (?\" . ((lambda ()
		       (if (and (char-before) (= (char-before) ?@))
			   'add ',punct-resolve))
		     (lambda ()
		       (if (and (char-before (1- (point)))
				(= (char-before (1- (point))) ?@))
			   ',punct-complete 'none))
		     (lambda ()
		       (if (or (and (char-before) (= (char-before) ?@))
			       (not (fboundp 'TeX-insert-quote)))
			   t
			 (TeX-insert-quote nil)
			 nil))))
	     (?' . ((lambda ()
		      (if (and (char-before) (= (char-before) ?@))
			  'add ',punct-resolve))
		    (lambda ()
		      (if (and (char-before (1- (point)))
			       (= (char-before (1- (point))) ?@))
			  ',punct-complete 'none))))
	     (?* . ((lambda ()
		      (if (and (char-before) (= (char-before) ?@))
			  'add ',punct-resolve))
		    (lambda ()
		      (if (and (char-before (1- (point)))
			       (= (char-before (1- (point))) ?@))
			  ',punct-complete 'none))))
	     (?, . ((lambda ()
		      (if (and (char-before) (= (char-before) ?@))
			  'add ',punct-resolve))
		    (lambda ()
		      (if (and (char-before (1- (point)))
			       (= (char-before (1- (point))) ?@))
			  ',punct-complete 'none))))
	     (?- . ((lambda ()
		      (if (and (char-before) (= (char-before) ?@))
			  'add ',punct-resolve))
		    (lambda ()
		      (if (and (char-before (1- (point)))
			       (= (char-before (1- (point))) ?@))
			  ',punct-complete 'none))))
	     (?. . ((lambda ()
		      (if (and (char-before) (= (char-before) ?@))
			  'add ',punct-resolve))
		    (lambda ()
		      (if (and (char-before (1- (point)))
			       (= (char-before (1- (point))) ?@))
			  ',punct-complete 'none))))
	     (?/ . ((lambda ()
		      (if (and (char-before) (= (char-before) ?@))
			  'add ',punct-resolve))
		    (lambda ()
		      (if (and (char-before (1- (point)))
			       (= (char-before (1- (point))) ?@))
			  ',punct-complete 'none))))
	     (?: . ((lambda ()
		      (if (and (char-before) (= (char-before) ?@))
			  'add ',punct-resolve))
		    (lambda ()
		      (if (and (char-before (1- (point)))
			       (= (char-before (1- (point))) ?@))
			  ',punct-complete 'none))))
	     (?= . ((lambda ()
		      (if (and (char-before) (= (char-before) ?@))
			  'add ',punct-resolve))
		    (lambda ()
		      (if (and (char-before (1- (point)))
			       (= (char-before (1- (point))) ?@))
			  ',punct-complete 'none))))
	     (?? . ((lambda ()
		      (if (and (char-before) (= (char-before) ?@))
			  'add ',punct-resolve))
		    (lambda ()
		      (if (and (char-before (1- (point)))
			       (= (char-before (1- (point))) ?@))
			  ',punct-complete 'none))))
	     (?^ . ((lambda ()
		      (if (and (char-before) (= (char-before) ?@))
			  'add ',punct-resolve))
		    (lambda ()
		      (if (and (char-before (1- (point)))
			       (= (char-before (1- (point))) ?@))
			  ',punct-complete 'none))))
	     (?` . ((lambda ()
		      (if (and (char-before) (= (char-before) ?@))
			  'add ',punct-resolve))
		    (lambda ()
		      (if (and (char-before (1- (point)))
			       (= (char-before (1- (point))) ?@))
			  ',punct-complete 'none))))
 	     )
	   auto-completion-override-syntax-alist) ; append target
	  )
    ))


;; FIXME: these probably need to go in a math auto-overlay
;;        'completion-override-syntax-alist property
;; (?\\ . ((lambda ()
;; 	  (if (and (char-before) (= (char-before) ?\\)
;; 		   (or (not (char-before (1- (point))))
;; 		       (not (= (char-before (1- (point)))
;; 			       ?\\))))
;; 	      'add ',punct-resolve))
;; 	,word-complete))
;; (?$ . (,punct-resolve none))
;; (?^ . (,punct-resolve none))



(defun predictive-texinfo-kill-buffer ()
  ;; Function called from `kill-buffer-hook' to tidy things up
  ;; save overlays if buffer was saved
  (unless (buffer-modified-p)
    (auto-overlay-save-overlays 'predictive nil
				predictive-auxiliary-file-location)
    ;; unload local dicts, without saving if buffer wasn't saved
    (predictive-auto-dict-unload "texinfo-node" (buffer-modified-p))
    (predictive-auto-dict-unload "texinfo-local-texinfo" (buffer-modified-p))
    (predictive-auto-dict-unload "texinfo-local-flag" (buffer-modified-p))
    (kill-local-variable 'predictive-texinfo-node-dict)
    (kill-local-variable 'predictive-texinfo-local-texinfo-dict)
    (kill-local-variable 'predictive-texinfo-local-flag-dict)))



(defun predictive-texinfo-after-save ()
  ;; Function called from `after-save-hook'
  (auto-overlay-save-overlays 'predictive nil
			      predictive-auxiliary-file-location)
  ;; if file has not been renamed, just save local dicts
  (if (or (and (null predictive-texinfo-previous-filename)
	       (null (buffer-file-name)))
	  (string= predictive-texinfo-previous-filename
		   (buffer-file-name)))
      (progn
	(predictive-auto-dict-save "texinfo-node")
	(predictive-auto-dict-save "texinfo-local-texinfo")
	(predictive-auto-dict-save "texinfo-local-flag"))
    ;; otherwise, restart predictive-mode to set everything up afresh
    (let ((restore (buffer-file-name)))
      (set-visited-file-name predictive-texinfo-previous-filename)
      (predictive-mode -1)
      (set-visited-file-name restore)
      (set-buffer-modified-p nil)
      (predictive-mode 1)))

  ;; store visited file name for comparison next time buffer is saved
  (setq predictive-texinfo-previous-filename (buffer-file-name))
  ;; repeat file save nessage (overwritten by overlay and dict save messages)
  (message "Wrote %s and saved predictive-mode state" (buffer-file-name)))



(defun predictive-texinfo-reparse-buffer ()
  "Clear all auto-overlays, then reparse buffer from scratch."
  (interactive)

  ;; stop the predictive auto-overlays without saving to file
  (auto-overlay-stop 'predictive)
  ;; revert to saved auto-dicts
  (predictive-auto-dict-unload "texinfo-node" (buffer-modified-p))
  (predictive-auto-dict-unload "texinfo-local-texinfo" (buffer-modified-p))
  (predictive-auto-dict-unload "texinfo-local-flag" (buffer-modified-p))
  (kill-local-variable 'predictive-texinfo-node-dict)
  (kill-local-variable 'predictive-texinfo-local-texinfo-dict)
  (kill-local-variable 'predictive-texinfo-local-flag-dict)
  (setq predictive-texinfo-node-dict
	(predictive-auto-dict-load "texinfo-node")
	predictive-texinfo-local-texinfo-dict
	(predictive-auto-dict-load "texinfo-local-texinfo")
	predictive-texinfo-local-flag-dict
	(predictive-auto-dict-load "texinfo-local-flag"))
  ;; clear and reload the overlay definitions (have to do this, otherwise some
  ;; auto-overlays try to add duplicate regexp definitions when reparsed)
  (setq auto-overlay-regexps nil)
  (predictive-texinfo-load-regexps)
  ;; restart the predictive auto-overlays; we let-bind predictive-mode to
  ;; prevent duplicate definition warnings
  (let ((predictive-mode nil))
    (auto-overlay-start 'predictive nil 'ignore-save-file)))




;;;=======================================================================
;;;                       Navigation commands

(defun predictive-texinfo-jump-to-definition ()
  "Jump to definition of whatever is at point.
\(Can be a label, or a command or environemtn defined in the
document's preamble\).

If point is already on a definition, cycle to next duplicate
definition of the same thing."
  (interactive)

  (let ((current-dict (predictive-current-dict))
	word dict o-def type)
    (when (dictree-p current-dict) (setq current-dict (list current-dict)))

    (or
     ;; when we're on either a cross-reference or a node definition...
     (and (or (member (setq dict (symbol-value (predictive-auto-dict-name
						"texinfo-node")))
		      current-dict)
	      (setq o-def (car (auto-overlays-at-point
				nil `((identity auto-overlay)
				      (eq set-id predictive)
				      (,(lambda (id)
					  (or (eq id 'node)
					      (eq id 'anchor)))
				       definition-id))))))
	  ;; look for node name at point
	  (setq word (or (thing-at-point 'predictive-texinfo-node-word)
			 (thing-at-point 'predictive-texinfo-node-def-word)))
	  (setq word (replace-regexp-in-string "\n+" " " word))
	  (set-text-properties 0 (length word) nil word)
	  (setq type "node"))

     ;; when we're on either a texinfo command or a definition thereof...
     (and (or (member (setq dict (symbol-value (predictive-auto-dict-name
						"texinfo-local-texinfo")))
		      current-dict)
	      (setq o-def (car (auto-overlays-at-point
				nil `((identity auto-overlay)
				      (eq set-id predictive)
				      (,(lambda (id)
					  (or (eq id 'alias)
					      (eq id 'macro)))
				       definition-id))))))
	  ;; look for command at point
	  (setq word (thing-at-point 'predictive-texinfo-word))
	  (set-text-properties 0 (length word) nil word)
	  ;; verify we're on a command by checking first character is "@"
	  (= (elt word 0) ?@)
	  (setq type "command"))

     ;; when we're on either a flag or definition thereof...
     (and (or (member (setq dict (symbol-value (predictive-auto-dict-name
						"texinfo-local-flag")))
		      current-dict)
	      (setq o-def (car (auto-overlays-at-point
				nil '((identity auto-overlay)
				      (eq set-id predictive)
				      (eq definition-id set))))))
	  ;; look for environment at point
	  (setq word (thing-at-point 'predictive-texinfo-word))
	  (set-text-properties 0 (length word) nil word)
	  (setq type "flag")))


    (if (null type)
	(message "Nothing to jump to")
      ;; jump to definition
      (setq o-def (predictive-auto-dict-jump-to-def dict word o-def))
      (cond
       ;; we only find out here whether a command or environment was defined
       ;; in the preamble or globally, so might have jumped no where
       ((null o-def) (message "Nothing to jump to"))
       ;; display warning if multiply defined
       ((> (length o-def) 1)
	(message "Texinfo %s \"%s\" multiply defined" type word))))
    ))



(defvar predictive-texinfo-node-history nil
  "History list for commands that read a Texinfo node name.")

(defun predictive-texinfo-jump-to-node-definition (&optional node)
  "Jump to the definition of NODE in the current Texinfo document.

Interactively, NODE is read from the mini-buffer, defaulting to
the node name at point (if any)."
  (interactive)

  (let ((dict (symbol-value (predictive-auto-dict-name "texinfo-node")))
	(current-dict (predictive-current-dict))
	o-def)
    (when (dictree-p current-dict) (setq current-dict (list current-dict)))

    ;; look for node name or definition at point
    (unless node
      (when (or (member dict current-dict)
		(setq o-def (car (auto-overlays-at-point
				  nil `((identity auto-overlay)
					(eq set-id predictive)
					(,(lambda (id)
					    (or (eq id 'node)
						(eq id 'anchor)))
					 definition-id))))))
	(when (setq node
		    (or (thing-at-point 'predictive-texinfo-node-word)
			(thing-at-point 'predictive-texinfo-node-def-word)))
	  (set-text-properties 0 (length node) nil node)
	  (setq node (replace-regexp-in-string "\n+" " " node)))))

    ;; interactively, read node from minibuffer, defaulting to what we've
    ;; found
    (when (called-interactively-p 'any)
      (let ((node-tmp
	     (completing-read
	      (if node
		  (format "Texinfo node (default \"%s\"): " node)
		"Texinfo node: ")
	      (lambda (string predicate all)
		(dictree-collection-function dict string predicate all))
	      nil t nil 'predictive-texinfo-node-history node t)))
	;; unless user accepted default, any overlay we found is no longer
	;; relevant
	(unless (string= node node-tmp)
	  (setq node node-tmp)
	  (setq o-def nil))))

    ;; jump to definition
    (unless (or (null node) (string= node ""))
      (setq o-def (predictive-auto-dict-jump-to-def dict node o-def))
      ;; display warning if multiply defined
      (when (> (length o-def) 1)
	(message "Texinfo node \"%s\" multiply defined" node))
      t)  ; return t to indicate we jumped somehwere
    ))



(defvar predictive-texinfo-command-history nil
  "History list for commands that read a Texinfo command.")

(defun predictive-texinfo-jump-to-command-definition (&optional command)
  "Jump to the definition of COMMAND in the current Texinfo document.

Interactively, COMMAND is read from the mini-buffer, defaulting to
the command at point (if any)."
  (interactive)

  (let ((dict (symbol-value (predictive-auto-dict-name "texinfo-local-texinfo")))
	(current-dict (predictive-current-dict))
	o-def)
    (when (dictree-p current-dict) (setq current-dict (list current-dict)))

    ;; look for command or definition thereof at point
    (unless command
      (when (or (member dict current-dict)
		(setq o-def (car (auto-overlays-at-point
				  nil `((identity auto-overlay)
					(eq set-id predictive)
					(,(lambda (id)
					    (or (eq id 'alias)
						(eq id 'macro)))
					 definition-id))))))
	(when (setq command (thing-at-point 'predictive-texinfo-word))
	  (if (= (elt command 0) ?@)
	      (set-text-properties 0 (length command) nil command)
	    (setq command nil)))))

    ;; interactively, read command from minibuffer, defaulting to what we've
    ;; found
    (when (called-interactively-p 'any)
      (let ((command-tmp
	     (completing-read
	      (if command
		  (format "Texinfo command (default \"%s\"): " command)
		"Texinfo command: ")
	      (lambda (string predicate all)
		(dictree-collection-function dict string predicate all))
	      nil t nil 'predictive-texinfo-command-history command t)))
	;; unless user accepted default, any overlay we found is no longer
	;; relevant
	(unless (string= command command-tmp)
	  (setq command command-tmp)
	  (setq o-def nil))))

    ;; jump to definition
    (unless (or (null command) (string= command ""))
      (setq o-def (predictive-auto-dict-jump-to-def dict command o-def))
      ;; display warning if multiply defined
      (when (> (length o-def) 1)
	(message "Texinfo command \"%s\" multiply defined" command))
      t)  ; return t to indicate we jumped somehwere
    ))



(defvar predictive-texinfo-flag-history nil
  "History list for commands that read a Texinfo flag.")

(defun predictive-texinfo-jump-to-flag-definition (&optional flag)
  "Jump to the definition of FLAG in the current Texinfo document.

Interactively, FLAG is read from the mini-buffer, defaulting to
the flag at point (if any)."
  (interactive)

  (let ((dict (symbol-value (predictive-auto-dict-name "texinfo-flag")))
	(current-dict (predictive-current-dict))
	o-def)
    (when (dictree-p current-dict) (setq current-dict (list current-dict)))

    ;; look for flag name or definition at point
    (unless flag
      (when (or (member dict current-dict)
		(setq o-def (car (auto-overlays-at-point
				  nil '((identity auto-overlay)
					(eq set-id predictive)
					(eq definition-id set))))))
	(when (setq flag (thing-at-point 'predictive-texinfo-word))
	  (set-text-properties 0 (length flag) nil flag))))

    ;; interactively, read flag from minibuffer, defaulting to what we've
    ;; found
    (when (called-interactively-p 'any)
      (let ((flag-tmp
	     (completing-read
	      (if flag
		  (format "Texinfo flag (default \"%s\"): " flag)
		"Texinfo flag: ")
	      (lambda (string predicate all)
		(dictree-collection-function dict string predicate all))
	      nil t nil 'predictive-texinfo-flag-history flag t)))
	;; unless user accepted default, any overlay we found is no longer
	;; relevant
	(unless (string= flag flag-tmp)
	  (setq flag flag-tmp)
	  (setq o-def nil))))

    ;; jump to definition
    (unless (or (null flag) (string= flag ""))
      (setq o-def (predictive-auto-dict-jump-to-def dict flag o-def))
      ;; display warning if multiply defined
      (when (> (length o-def) 1)
	(message "Texinfo flag \"%s\" multiply defined" flag))
      t)  ; return t to indicate we jumped somehwere
    ))




;;;=============================================================
;;;                Completion-browser functions

(defun predictive-texinfo-construct-browser-menu (overlay)
  "Construct the Texinfo browser menu keymap."
  ;; construct menu, dropping the last two entries which are a separator and a
  ;; link back to the basic completion menu (would just redisplay this menu,
  ;; since we're using the browser as the default menu)
  (let ((menu (completion-construct-browser-menu
	       overlay 'predictive-texinfo-browser-menu-item)))
    (setq menu (butlast menu 2))))



(defun predictive-texinfo-browser-menu-item (cmpl ignore1 ignore2 overlay)
  "Construct predictive Texinfo completion browser menu item."

  ;; if entry is @end, create sub-menu containing environment completions
  (if (string= (concat (overlay-get overlay 'prefix) cmpl) "@end")
      ;; find all Texinfo environments
      (let ((envs (dictree-complete dict-texinfo-env ""))
	    (menu (make-sparse-keymap)))
	(setq envs (mapcar (lambda (e) (concat cmpl " " (car e))) envs))
	;; create sub-menu keymap
	(setq menu (completion-browser-sub-menu
		    envs
		    'predictive-texinfo-browser-menu-item
		    'completion-browser-sub-menu
		    overlay))
	;; add completion itself (@end) to the menu
	(define-key menu [separator-item-sub-menu] '(menu-item "--"))
	(define-key menu [completion-insert-root]
	  (list 'menu-item (concat (overlay-get overlay 'prefix) cmpl)
		`(lambda ()
		   (list ,(if (stringp cmpl) cmpl (car cmpl))
			 ,(if (stringp cmpl)
			      (length (overlay-get overlay 'prefix))
			    (cdr cmpl))))))
	;; return the menu keymap
	menu)

    ;; otherwise, create a selectable completion item
    `(lambda ()
       (list ,(if (stringp cmpl) cmpl (car cmpl))
	     ,(if (stringp cmpl)
		  (length (overlay-get overlay 'prefix))
		(cdr cmpl))))))




;;;=============================================================
;;;               Miscelaneous utility functions

(defun predictive-texinfo-forward-word (&optional n)
  ;; going backwards...
  (if (and n (< n 0))
      (unless (bobp)
	(dotimes (i (- n))
	  ;; make sure we're at the end of a word
	  (when (re-search-backward "@\\|\\w" nil t)
	    (forward-char))
	  ;; if point is within or just after a sequence of @'s, go
	  ;; backwards for the correct number of @'s
	  (if (= (char-before) ?@)
	      (let ((pos (point)))
		(save-excursion
		  (while (= (char-before) ?@) (backward-char))
		  (setq pos (- pos (point))))
		(if (= (mod pos 2) 1) (backward-char) (backward-char 2)))
	    ;; otherwise, go back one word, plus one @ if there's an odd
	    ;; number of them before it
	    (backward-word 1)  ; argument not optional in Emacs 21
	    (when (and (not (bobp)) (= ?@ (char-before)))
	      (let ((pos (point)))
		(save-excursion
		  (while (= (char-before) ?@) (backward-char))
		  (setq pos (- pos (point))))
		(when (= (mod pos 2) 1) (backward-char))))
	    )))

    ;; going forwards...
    (unless (eobp)
      ;; deal with point within sequence of @'s
      (when (= (char-after) ?@)
	(let ((pos (point)))
	  (save-excursion
	    (while (= (char-before) ?@) (backward-char))
	    (setq pos (- pos (point))))
	  (when (= (mod pos 2) 1) (backward-char))))
      ;; go forward, counting @ as part of word, @@ as entire word
      (dotimes (i (or n 1))
	(when (re-search-forward "@\\|\\w" nil t)
	  (backward-char))
	(re-search-forward "@\\W\\|@\\w+\\|\\w+" nil t)
	(when (= (char-before) ?\n) (backward-char))))
    ))



(defun predictive-texinfo-node-forward-word (&optional n)
  ;; going backwards...
  (if (and n (< n 0))
      (unless (bobp)
	(setq n (- n))
	(dotimes (i n)
	  (while (and (char-before) (/= (char-before) ?{)
		      (or (and (= (char-syntax (char-before)) ?w)
			       (backward-word 1))
			  (or (backward-char) t))))))
    ;; going forwards...
    (unless (eobp)
      (setq n (if n n 1))
      (dotimes (i n)
	(while (and (char-after) (/= (char-after) ?})
		    (or (and (= (char-syntax (char-after)) ?w)
			     (forward-word 1))
			(or (forward-char) t))))))))



(defun predictive-texinfo-node-def-forward-word (&optional n)
  ;; going backwards...
  (if (and n (< n 0))
      (unless (bobp)
	(setq n (- n))
	(when (re-search-backward "^@node[[:space:]]*" nil t n)
	  (forward-char (- (match-end 0) (match-beginning 0)))))
    ;; going forwards...
    (goto-char (line-end-position))))



;;; predictive-texinfo.el ends here
