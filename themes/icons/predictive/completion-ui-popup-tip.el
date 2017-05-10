
;;; completion-ui-popup-tip.el --- popup-tip user-interface for Completion-UI


;; Copyright (C) 2012 Toby Cubitt

;; Author: Toby Cubitt <toby-predictive@dr-qubit.org>
;; Version: 0.1.2
;; Keywords: completion, user interface, popup-tip
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
(require 'completion-ui)
(require 'popup)



;;; ============================================================
;;;                    Customization variables

(defgroup completion-ui-popup-tip nil
  "Completion-UI popup-tip user interface."
  :group 'completion-ui)


(completion-ui-defcustom-per-source completion-ui-use-popup-tip t
  "When non-nil, enable the popup-tip interface."
  :group 'completion-ui-popup-tip
  :type 'boolean)


(defcustom completion-ui-popup-tip-margin 1
  "Width (in characters) of left and right margins in a popup-tip."
  :group 'completion-ui-popup-tip
  :type 'integer)


(defcustom completion-ui-popup-tip-under-point nil
  "If non-nil, position popup-tip directly under point.
Otherwise, it is positioned under the word being completed."
  :group 'completion-ui-popup-tip
  :type 'boolean)


(defface completion-popup-tip-face
  '((t . (:inherit popup-tip-face)))
  "Face used in popup-tip."
  :group 'completion-ui-popup-tip)



;;; ============================================================
;;;                 Other configuration variables

(defvar completion-popup-tip-function nil
  "Function to call to construct the popup-tip text.

The function is called with three arguments, the prefix, a list
of completions, and the index of the currently active
completion. It should return a string containing the text to be
displayed in the popup-tip.

Note: this can be overridden by an \"overlay local\" binding (see
`auto-overlay-local-binding').")


(defvar completion-popup-tip-map nil
  "Keymap used when a popup-tip is displayed.
These key bindings also get added to the completion overlay keymap.")


(defvar completion-popup-tip-active-map nil
  "Keymap used when a popup-tip is being displayed.")



;;; ============================================================
;;;                   Setup default keymaps

;; Set default key bindings for the keymap used when a popup-tip is displayed,
;; unless it's already been set (most likely in an init file).
(unless completion-popup-tip-active-map
  (let ((map (make-sparse-keymap)))
    ;; <up> and <down> cycle completions, which appears to move selection
    ;; up and down popup-tip entries
    (define-key map [down] 'completion-popup-tip-cycle)
    (define-key map [up] 'completion-popup-tip-cycle-backwards)
    (setq completion-popup-tip-active-map map)))


;; Set default key bindings for the keymap whose key bindings are added to the
;; overlay keymap when the popup-tip interface is enabled
(unless completion-popup-tip-map
  (setq completion-popup-tip-map (make-sparse-keymap))
  ;; S-<down> displays the completion popup-tip
  (define-key completion-popup-tip-map [S-down] 'completion-show-popup-tip)
  ;; S-<up> cancels it
  (define-key completion-popup-tip-map [S-up] 'completion-cancel-popup-tip))


;; ;; <up> and <down> cycle the popup-tip
;; ;; Note: it shouldn't be necessary to bind them here, but the bindings in
;; ;;       completion-popup-tip-active-map don't work when the popup-tip is
;; ;;       auto-displayed, for mysterious reasons (note that we can't use
;; ;;       `completion-run-if-condition' in `completion-overlay-map', hence
;; ;;       binding them here)
;; (define-key completion-map [down]
;;   (lambda () (interactive)
;;     (completion--run-if-condition
;;      'completion-popup-tip-cycle
;;      'completion-ui--activated
;;      completion-popup-tip-active)))
;; (define-key completion-map [down]
;;   (lambda () (interactive)
;;     (completion--run-if-condition
;;      'completion-popup-tip-cycle-backwards
;;      'completion-ui--activated
;;      completion-popup-tip-active)))



;;; ============================================================
;;;                 Interface functions

(defun completion-activate-popup-tip (&optional overlay)
  "Show the completion popup-tip.

If no overlay is supplied, tries to find one at point.
The point had better be within OVERLAY or you'll have bad luck
in all your flower-arranging endevours for thirteen years."
    (interactive)
    ;; if no overlay was supplied, try to find one at point
    (unless overlay (setq overlay (completion-ui-overlay-at-point)))
    ;; activate popup-tip key bindings
    (completion-activate-overlay-keys overlay completion-popup-tip-map)
    ;; if popup-tip has been displayed manually, re-display it
    (when (overlay-get overlay 'completion-interactive-popup-tip)
      (completion-show-popup-tip overlay)))



(defun completion-show-popup-tip (&optional overlay interactive)
  "Show completion popup-tip for completion OVERLAY.
The point had better be within OVERLAY or you'll have bad luck
in all your flower-arranging endevours for fourteen years.

If OVERLAY is not supplied, try to find one at point. If
INTERACTIVE is supplied, pretend we were called interactively."
  (interactive)

  ;; if no overlay was supplied, try to find one at point
  (unless overlay (setq overlay (completion-ui-overlay-at-point)))
  ;; deactivate other auto-show interfaces
  (completion-ui-deactivate-auto-show-interface overlay)
  ;; if we can display a popup-tip and
  (when overlay
    (completion-cancel-popup-tip overlay)
    ;; if there are completions to display...
    (when (overlay-get overlay 'completions)
      ;; if called manually, flag this in overlay property and call
      ;; auto-show-helpers, since they won't have been called by
      ;; `completion-ui-auto-show'
      (when (or (called-interactively-p 'any) interactive)
	(overlay-put overlay 'completion-interactive-popup-tip t)
	(completion-ui-call-auto-show-interface-helpers overlay))

      ;; construct the popup-tip text
      (let ((text (funcall
		   (completion-ui-source-popup-tip-function nil overlay)
		   overlay)))
	(when (string= (substring text -1) "\n")
	  (setq text (substring text 0 -1)))

	;; show popup-tip
	(let ((popup (popup-tip
		      text
		      :point (+ (overlay-start overlay)
				(if completion-ui-popup-tip-under-point
				    completion-ui-popup-tip-margin
				  (- (length (overlay-get overlay 'prefix)))))
		      :nowait t
		      :face 'completion-popup-tip-face
		      :selection-face 'completion-highlight-face
		      :margin-left completion-ui-popup-tip-margin
		      :margin-right completion-ui-popup-tip-margin))
	      (i (overlay-get overlay 'completion-num)))
	  (overlay-put overlay 'completion-popup-tip popup)
	  (when i (popup-select popup i)))

	;; activate popup-tip keys
	(completion-activate-overlay-keys
	 overlay completion-popup-tip-active-map)
	))))


(defun completion-cancel-popup-tip (&optional overlay)
  "Hide the completion popup-tip and cancel timers."
  (interactive)
  ;; unset manually displayed popup-tip flag
  (when (or overlay (setq overlay (completion-ui-overlay-at-point)))
    (overlay-put overlay 'completion-interactive-popup-tip nil))
  ;; cancel timer
  (completion-ui-cancel-auto-show)
  ;; cancel popup-tip
  (popup-delete (overlay-get overlay 'completion-popup-tip))
  (overlay-put overlay 'completion-popup-tip nil)
  ;; disable popup-tip keys
  (completion-deactivate-overlay-keys
   overlay completion-popup-tip-active-map))


(defun completion-popup-tip-cycle (&optional n overlay)
  "Cycle forwards through N completions and redisplay the popup-tip.
A negative argument cycles backwards.

If OVERLAY is supplied, use that instead of finding one. The
point had better be within OVERLAY or you'll be attacked by a mad
cow."
  (interactive)
  (completion-cycle n overlay)
  (completion-show-popup-tip nil t))


(defun completion-popup-tip-cycle-backwards (&optional n overlay)
  "Cycle backwards through N completions and redisplay the popup-tip.
A negative argument cycles backwards.

If OVERLAY is supplied, use that instead of finding one. The
point had better be within OVERLAY or you'll be attacked by a mad
sheep."
  (interactive)
  (completion-cycle (if n (- n) -1) overlay)
  (completion-show-popup-tip nil t))



;;; =================================================================
;;;                    Register user-interface

(completion-ui-register-interface popup-tip
 :variable   completion-ui-use-popup-tip
 :activate   completion-activate-popup-tip
 :deactivate completion-cancel-popup-tip
 :auto-show  completion-show-popup-tip)



(provide 'completion-ui-popup-tip)


;; Local Variables:
;; eval: (font-lock-add-keywords nil '(("(\\(completion-ui-defcustom-per-source\\)\\>[ \t]*\\(\\sw+\\)?" (1 font-lock-keyword-face) (2 font-lock-variable-name-face))))
;; End:

;;; completion-ui-popup-tip.el ends here
