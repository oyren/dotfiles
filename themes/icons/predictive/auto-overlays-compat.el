
;;; auto-overlays-compat.el --- compatability functions for auto-overlays package


;; Copyright (C) 2006, 2012 Toby Cubitt

;; Author: Toby Cubitt <toby-predictive@dr-qubit.org>
;; Version: 0.3.2
;; Keywords: auto-overlay, automatic, overlays, compatability
;; URL: http://www.dr-qubit.org/emacs.php

;; This file is NOT part of the Emacs.
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

(provide 'auto-overlays-compat)


(defun auto-overlays-compat-line-number-at-pos (&optional pos)
  "Return (narrowed) buffer line number at position POS.
\(Defaults to the point.\)"
  (unless pos (setq pos (point)))
  ;; note: need to add 1 if at beginning of line
  (+ (count-lines (point-min) pos)
     (if (save-excursion (goto-char pos) (bolp)) 1 0))
)


(defun auto-overlays-compat-replace-regexp-in-string (regexp rep string)
  "Return a new string with all matches for REGEXP in STRING replaced
with REP."
  (let ((str string))
    (while (string-match regexp str)
      (setq str (replace-match rep nil nil str)))
    str)
)

;;; auto-overlays-compat.el ends here
