
;;; predictive-compat.el --- compatability functions for predictive package


;; Copyright (C) 2004-2006, 2012 Toby Cubitt

;; Author: Toby Cubitt <toby-predictive@dr-qubit.org>
;; Version: 0.12.1
;; Keywords: predictive, completion, compatability
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


(defun predictive-compat-replace-regexp-in-string (regexp rep string)
  "Return a new string with all matches for REGEXP in STRING replaced
with REP."
  (let ((str string))
    (while (string-match regexp str)
      (setq str (replace-match rep nil nil str)))
    str)
)
