
(require 'predictive)

(defun predictive-setup-numpad ()
  "Configure current buffer for predictive text entry via the numpad.
\(Similar to the predictive text feature found on many mobile
phones.\)"
  (interactive)
  (set (make-local-variable 'predictive-equivalent-characters)
       '("2abc" "3def" "4ghi" "5jkl" "6mno" "7pqrs" "8tuv" "9wxyz" "0 "))
  (set (make-local-variable 'auto-completion-override-syntax-alist) nil)
  (set (make-local-variable 'completion-ui-use-hotkeys) nil))


(provide 'predictive-numpad)
