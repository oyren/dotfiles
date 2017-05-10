
(require 'dict-tree)

(eval-when-compile (defvar dict-english nil))


(defun crossword-search (pattern)
  "Search for words matching a pattern of letters in a crossword.
Use \".\" (without quotes) in place of unknown letters.

When called interactively, matching words are displayed in the
\"*Crossword matches\" buffer.

Searches for matches in the `dict-english' English
dictionary. Returns a list of words that match the pattern."
  (interactive "sCrossword pattern (use \".\" in place of unknown letters): ")
  (unless (dictree-p dict-english) (dictree-load "dict-english"))
  (setq pattern
	(replace-regexp-in-string "\\\\\\." "." (regexp-quote pattern)))
  (let ((words (dictree-regexp-search
		dict-english pattern nil nil nil nil nil
		(lambda (key data) key))))
    (when (called-interactively-p 'any)
      (switch-to-buffer-other-window "*Crossword matches*")
      (erase-buffer)
      (dolist (word words) (insert word "\n"))
      (other-window -1))
    words))
