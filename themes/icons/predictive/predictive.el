
;;; predictive.el --- predictive completion minor mode


;; Copyright (C) 2004-2012 Toby Cubitt

;; Author: Toby Cubitt <toby-predictive@dr-qubit.org>
;; Package-Version: 0.24
;; Version: 0.19.8
;; Keywords: convenience, abbrev, tex, predictive, completion
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


;;; Install:
;;
;; Put the Predictive Completion package files in your load-path, and add the
;; following to your .emacs:
;;
;;     (require 'predictive)
;;
;; Alternatively, you can use autoload instead to save memory:
;;
;;     (autoload 'predictive-mode "/path/to/predictive.elc" t)
;;
;; Full instructions are given in the INSTALL file that should come with the
;; Predictive Completion package.



;;; Code:

(eval-when-compile (require 'cl))
(require 'completion-ui)
(require 'dict-tree)
(require 'auto-overlays)
(require 'timerfunctions)
(require 'ispell)

;; use dynamic byte compilation to save memory
;;(eval-when-compile (setq byte-compile-dynamic t))




;;; ================================================================
;;;          Customization variables controling predictive mode

(defgroup predictive nil
  "Predictive completion."
  :group 'convenience)


(defcustom predictive-main-dict 'dict-english
  "Main dictionary to use in a predictive mode buffer.

It should be the symbol of a loaded dictionary. It can also be a
list of such symbols, in which case predictive mode searches for
completions in all of them, as though they were one large
dictionary.

Note that using lists of dictionaries can lead to unexpected effets when
auto-learn or auto-add-to-dict are used. If auto-learn is enabled, weights
will be updated in the first dictionary in the list that contains the word
being updated \(see `predictive-auto-learn'\). Similarly, if auto-add-to-dict
is set to t, words will be added to the first dictionary in the list \(see
`predictive-auto-add-to-dict'\)."
  :group 'predictive
  :type 'symbol)


(defcustom predictive-dict-lock-loaded-list nil
  "List of dictionaries that should never be automatically unloaded,
in addition to `predictive-main-dict'."
  :group 'predictive
  :type '(repeat symbol))


(defcustom predictive-completion-speed 0.1
  "Default completion speed for new predictive mode dictionaries
created using `predictive-create-dict'.

The completion speed is a desired upper limit on the time it
takes to find completions, in seconds. However, there is no
guarantee it will be achieved!  Lower values result in faster
completion, at the expense of dictionaries taking up more
memory."
  :group 'predictive
  :type 'number)


(defcustom predictive-dict-autosave t
  "Default autosave flag for new predictive mode dictionaries.

A value of t means modified dictionaries will be saved
automatically when unloaded (or when quitting Emacs). The symbol
'ask' means you will be prompted to save modified dictionaries. A
value of nil means dictionaries will not be saved automatically,
and unless you save the dictionary manually all changes will be
lost when the dictionary is unloaded. See also
`predictive-save-dict'."
  :group 'predictive
  :type 'boolean)


(defcustom predictive-dict-autosave-on-kill-buffer t
  "Whether to save dictionaries when a buffer is killed.

If non-nil, modifications to dictionaries that are used by a
buffer will automatically be saved when the buffer is killed,
for dictionaries that have their autosave flag set (see
`predictive-dict-autosave')."
  :group 'predictive
  :type 'boolean)


(defcustom predictive-dict-autosave-on-mode-disable t
  "Whether to save dictionaries when predictive mode is disabled.

If non-nil, modifications to dictionaries that are used by a
buffer will automatically be saved when predictive mode is
disabled in that buffer, for dictionaries that have their
autosave flag set (see `predictive-dict-autosave')."
  :group 'predictive
  :type 'boolean)


(defcustom predictive-dict-compilation nil
  "Whether to save dictionaries in compiled or uncompiled form.

The default is to save both compiled and uncompiled forms. If set
to 'compiled, only the compiled form is saved. If set to
'uncompiled, only the uncompiled form is saved. The compiled form
loads faster, but is not portable across different Emacs
versions."
  :group 'predictive
  :type '(choice (const :tag "both" nil)
		 (const compiled)
		 (const uncompiled)))


(defcustom predictive-ignore-initial-caps t
  "Whether to ignore initial capital letters when completing
words. If non-nil, completions for the uncapitalized string are
also found.

Note that only the *first* capital letter of a string is
ignored. Thus typing \"A\" would find \"and\", \"Alaska\" and
\"ANSI\", but typing \"AN\" would only find \"ANSI\", whilst
typing \"a\" would only find \"and\"."
  :group 'predictive
  :type 'boolean)


(defcustom predictive-equivalent-characters '(" -")
  "List of characters to be treated as equivalent.
Each element of the list should be a string, and all characters
appearing in the same string will be treated as equivalent when
completing words. Predictive mode will then not only find
completions for the prefix you typed, but also for all equivalent
prefixes. Note that case is significant.

A typical use of this is to define all variants of a particular
character, regardless of accents or other diacritics, to be
equivalent. See also `predictive-auto-correction-no-completion'
and `completion-dynamic-highlight-prefix-alterations'.

`predictive-equivalent-characters' works by substituting a
character alternative listing all the equivalent characters
whenever those characters appear in the prefix. It merely
provides a more convenient way of defining these commonly used
expansions, and is exactly the same as adding those expansions on
to the very *end* of `predictive-prefix-expansions' \(which
see\), in the same order in which the characters are listed in
the string."
  :group 'predictive
  :type '(choice (const :tag "off" nil)
		 (const :tag "English" (" -"))
		 (const :tag "French"
			("aâà" "eéêè" "iî" "oô" "uû" "cç"
			 "AÂÀ" "EÉÊÈ" "IÎ" "OÔ" "UÛ" "CÇ"))
		 (const :tag "German"
			("aä" "oö" "uü" "AÄ" "OÖ" "UÜ"))
		 (const :tag "keypad"
			("2abc" "3def" "4ghi" "5jkl" "6mno"
			 "7pqrs" "8tuv" "9wxyz" "0 "))
		 (repeat :tag "custom" string)))



(defcustom predictive-prefix-expansions nil
  "Alist of expansions to apply to a prefix before completing it.
The alist should associate regexps with their replacements. The
expansions are applied in-order to the completion
prefix. Characters matching a regexp are only expanded once,
i.e. later expansions are *not* applied to the replacement text
of previous expansions. Case is always significant.

The expansions themselves can be regexp fragments, and can
include regexp special characters. However, only a subset of the
full Emacs regular expression syntax is supported. There is no
support for regexp constructs that are only meaningful for
strings (character ranges and character classes inside character
alternatives, and syntax-related backslash
constructs). Back-references and non-greedy postfix operators are
not supported, so `?' after a postfix operator loses its special
meaning. Also, matches are always anchored, so `$' and `^' lose
their special meanings (use `.*' at the beginning and end of the
regexp to get an unanchored match).

If the original prefix contains any regexp special characters,
they are quoted using `\\' *before* the prefix expansions are
applied.

Expansions produced by `predictive-equivalent-characters' are
effectively added on to the end of
`predictive-prefix-expansions', so any expansions defined in the
latter take precedence."
  :group 'predictive
  :type '(choice (const :tag "off" nil)
		 (const :tag "German"
			(("ss" . "\\(?ss\\|ß\\)?")
			 ("SS" . "\\(?SS\\|ß\\)?")
			 ("ß"  . "\\(?ss\\|ß\\)?")))
		 (alist :tag "custom" :key-type regexp :value-type string)))


(defcustom predictive-auto-correction-no-completion nil
  "When non-nil, predictive mode will only auto-correct typed words,
not complete them. This is only useful when one or more
`predictive-prefix-expansions' or
`predictive-equivalent-characters' are defined.

Setting this option disables completion entirely. Predictive mode
will only auto-correct typed words, using the definitions in
`predictive-prefix-expansions' and
`predictive-equivalent-characters'. For example, if the latter
expands characters into character alternatives containing all
accented and unaccented variants of that character, then
predictive mode will auto-correct accents, but without offering
completions of the words."
  :group 'predictive
  :type 'boolean)


(defcustom predictive-auto-complete t
  "Enable and disable `auto-completion-mode'
along with predictive mode."
  :group 'predictive
  :type 'boolean)


(defcustom predictive-auto-learn nil
  "Enables predictive mode's automatic word frequency learning.

When non-nil, the frequency count for that word is incremented
each time a completion is accepted, making the word more likely
to be offered higher up the list of completions in the future."
  :group 'predictive
  :type 'boolean)


(defcustom predictive-auto-add-to-dict nil
  "Controls automatic adding of new words to dictionaries.

If nil, words are never automatically added to a dictionary. If
t, new words \(i.e. words that are not in the dictionary\) are
automatically added to the active dictionary. If set to a
dictionary name (a symbol), new words are automatically added to
that dictionary instead of the active one. If set to the symbol
'buffer, words will be added to the buffer-local dictionary
\(`predictive-use-buffer-local-dict' must be non-nil in this
case\). Note that this is subtly different from adding to the
active dictionary when `predictive-use-buffer-local-dict' is
enabled, which will add words to *both* the buffer-local and main
dictionaries.

If `predctive-add-to-dict-ask' is enabled, predictive mode will
ask before adding any word."
  :group 'predictive
  :type '(choice (const :tag "off" nil)
		 (const :tag "active" t)
		 (const :tag "buffer-local" buffer)
		 (symbol :tag "dictionary")))
(make-variable-buffer-local 'predictive-auto-add-to-dict)


(defcustom predictive-add-to-dict-ask t
  "If non-nil, predictive mode will ask before auto-adding a word
to a dictionary. Enabled by default. This has no effect unless
`predictive-auto-add-to-dict' is also enabled."
  :group 'predictive
  :type 'boolean)


(defcustom predictive-auto-add-min-chars nil
  "Minimum length of auto-added words.
Words shorter than this will not be automatically added to the
  dictionary when `predictive-auto-add-to-dict' is enabled."
  :group 'predictive
  :type '(choice (const :tag "Off" nil)
		 (integer :tag "On")))


(defcustom predictive-auto-define-prefixes t
  "Controls automatic prefix definitions in predictive mode.

If non-nil, whenever a word is added to a dictionary, it is
automatically defined to be a prefix for all words for which it
*is* a prefix. Predictive mode will then automatically ensure
that its weight is always at least as great as the weight of any
of those words, so that it takes precedence over them when
completing."
  :group 'predictive
  :type 'boolean)


(defcustom predictive-auxiliary-file-location ".predictive/"
  "Directory to which predictive mode auxiliary files are saved.

If this is a relative path, it is relative to the current
directory of a buffer using predictive mode. This means that
files located in different directories will use separate
auxiliary file subdirectories.

Setting an absolute path is possible, but discouraged. All
auxiliary files will be created in the same directory, and there
are no safe-guards to prevent two different auxiliary files that
happen to have the same name from clobbering one another. That
said, auxiliary filenames incorporate the buffer filename, so
only identically named files in different directories pose a
risk."
  :group 'predictive
  :type 'directory
  ;; ensure trailing directory separator
  :set (lambda (var val)
	 (unless (string= (file-name-directory val) val)
	   (setq val (concat val "/")))
	 (set-default var val)))


(defcustom predictive-use-buffer-local-dict nil
  "If non-nil, a buffer-local dictionary will be used
in conjunction with `predictive-main-dict'. Results from both
dictionaries are combined, as though they were one large
dictionary.

The buffer-local dictionary is saved to a file in the same
directory as the buffer's associated file, and is loaded from
there the next time predictive mode is enabled in the same
buffer.

The dictionary is initially empty. If `predictive-auto-learn' or
`predictive-auto-add-to-dict' are enabled, words will be added to
it as you type. The learning rate for the word weights is
`predictive-local-learn-multiplier' times higher than that for
`predictive-main-dict', so the buffer-local dictionary will
quickly adapt to the vocabulary used in specific buffers.

Note that all the words from `predictive-main-dict' will still be
available as completions, but their weights will be modified by
the buffer-local dictionary."
  :group 'predictive
  :type 'boolean)


(defcustom predictive-buffer-local-learn-multiplier 50
  "Multiplier for buffer-local learning rate.
When words are learnt or added to a buffer-local dictionary, the
weight increment is multiplied by this number. See also
`predictive-use-buffer-local-dict'."
  :group 'predictive
  :type 'integer)


(defcustom predictive-use-auto-learn-cache t
  "If non-nil, auto-learned and auto-added words will be cached
and only added to the dictionary when Emacs is idle.

This makes predictive mode more responsive, since on slower
machines learning or adding words can otherwise cause a small but
noticeable delay when typing. However, it also means that
dictionaries will not immediately reflect changes due to
auto-learning or auto-adding. In particular, auto-added words
will not appear as completions until Emacs has been idle long
enough for them to have been added.

This has no effect unless `predictive-auto-learn' or
`predictive-auto-add' are enabled. See also
`predictive-flush-auto-learn-delay'."
  :group 'predictive
  :type 'boolean)


(defcustom predictive-flush-auto-learn-delay 10
  "Time to wait before flushing auto-learn/add caches.
The caches will only be flushed after Emacs has been idle for
this many seconds. To take effect, this variable must be set
before predictive mode is enabled.

This has no effect unless `predictive-use-auto-learn-cache' is
enabled."
  :group 'predictive
  :type 'number)


(defcustom predictive-which-dict-mode t
  "If non-nil, display the predictive mode dictionary in the mode line."
  :group 'predictive
  :type 'boolean)


(defcustom predictive-guess-prefix-suffixes
  '("ability" "ibility" "ity" "ables" "able" "ibles" "ible" "ally" "ingly"
    "ings" "ing" "ately" "ates" "ate" "ly" "ations" "ation" "tions" "tion"
    "ions" "ion" "atives" "ative" "ives" "ive" "ments" "ment" "ances" "ance"
    "ancy" "ish" "ists" "ist" "isms" "ism" "eous" "ous" "ians" "ian" "als"
    "al" "ed" "es" "en" "ic" "ns" "s" "d" "n" "y")
  "List of possible suffixes. Earlier entries take precedence."
  :group 'predictive
  :type '(repeat string))




;;; ==================================================================
;;;          Aliases for completion-UI customization options

;; (when (fboundp 'defvaralias)
;;   (defvaralias 'predictive-completion-max-candidates
;;     'completion-max-candidates)
;;   (defvaralias 'predictive-completion-resolve-behaviour
;;     'completion-resolve-behaviour)
;;   (defvaralias 'predictive-auto-completion-min-chars
;;     'auto-completion-min-chars)
;;   (defvaralias 'predictive-auto-completion-delay
;;     'auto-completion-delay)
;;   (defvaralias 'predictive-auto-completion-backward-delete-delay
;;     'auto-completion-backward-delete-delay)
;;   (defvaralias 'predictive-completion-use-dynamic
;;     'completion-use-dynamic)
;;   (defvaralias 'predictive-completion-dynamic-syntax-alist
;;     'completion-dynamic-syntax-alist)
;;   (defvaralias 'predictive-completion-dynamic-override-syntax-alist
;;     'completion-dynamic-override-syntax-alist)
;;   (defvaralias 'predictive-completion-use-hotkeys
;;     'completion-use-hotkeys)
;;   (defvaralias 'predictive-completion-hotkey-list
;;     'completion-hotkey-list)
;;   (defvaralias 'predictive-completion-use-tooltip
;;     'completion-use-tooltip)
;;   (defvaralias 'predictive-completion-tooltip-delay
;;     'completion-tooltip-delay)
;;   (defvaralias 'predictive-completion-tooltip-timeout
;;     'completion-tooltip-timeout)
;;   (defvaralias 'predictive-completion-tooltip-offset
;;     'completion-tooltip-offset)
;;   (defvaralias 'predictive-completion-tooltip-face
;;     'completion-tooltip-face)
;;   (defvaralias 'predictive-completion-auto-show-menu
;;     'completion-auto-show-menu)
;;   (defvaralias 'predictive-completion-browser-max-items
;;     'completion-browser-max-items)
;;   (defvaralias 'predictive-completion-browser-buckets
;;     'completion-browser-buckets)
;;   (defvaralias 'predictive-completion-use-echo
;;     'completion-use-echo))




;;; ==================================================================
;;;     Non-customization variables controlling predictive mode

;; These variables can be set in major-mode setup functions, hooks, or init
;; files. They are not customization definitions since it makes no sense for a
;; user to customize them.
;;
;; Note: default values for some are set by code at the end of this file


(defvar predictive-mode-hook nil
  "Hook run after predictive mode is enabled.")

(defvar predictive-mode-disable-hook nil
  "Hook run after predictive mode is disabled.")


;; FIXME: should this be a customization option?
(defvar predictive-major-mode-alist nil
  "Alist associating major mode symols with functions.
The functions should take one argument. The alist is checked
whenever predictive mode is enabled or disabled in a buffer. If
the buffer's major made matches an entry in the alist, the
associated function is called, with a positive argument if
predictive mode is being enabled or a negative one if it is being
disabled. This makes it easier to customize predictive mode for
different major modes.")


(defvar predictive-accept-functions '(predictive-auto-learn)
  "Hook run after a predictive completion is accepted.
The functions are called with two or three arguments: the prefix,
the accepted completion, and any prefix argument supplied by the
user when the completion was accepted interactively.")


(defvar predictive-reject-functions
  '((lambda (prefix word &optional arg)
      (when arg (predictive-auto-learn nil prefix))))
  "Hook run after a predictive completion is rejected.
The functions are called with two or three arguments: the prefix,
the rejected completion, and any prefix argument supplied by the
user when the completion was rejected interactively.")


(defvar predictive-menu-function 'completion-construct-menu
  "Function used to construct the predictive completion menu.")


(defvar predictive-browser-function 'completion-construct-browser-menu
  "Function used to construct the predictive completion browser.")


(defvar predictive-word-thing 'word
  "Thing-at-point symbol used to determine the prefix to complete.")


(defvar predictive-completion-filter nil
  "Function that returns a filter function for completions.
When set, this function is called with one argument: the prefix
that is being completed (a string). The function it returns
should take two arguments: a word from a dictionary and the value
stored for that word.

Note: this can be overridden by an \"overlay local\" binding (see
`auto-overlay-local-binding').")


(defvar predictive-auto-add-filter nil
  "Function called to decide whether to auto-add a word to a dictionary.
When set, this function is called with two arguments: the word
potentially being added (a string), and the dictionary it would
be added to. It should return non-nil if the word should be added
to the dictionary, nil if it should not. Only used when
`predictive-auto-add-to-dict' is enabled.")


(defvar predictive-map nil
  "Keymap used in predictive mode.")



;;; ================================================================
;;;                Setup default key bindings

(unless predictive-map
  (setq predictive-map (make-sparse-keymap))
  ;; M-<tab> and M-/ cycle word at point
  (define-key predictive-map [(meta tab)] 'complete-predictive)
  (define-key predictive-map "\M-/" 'complete-predictive)
  ;; M-<shift>-<tab> and M-? (usually M-<shift>-/) cycle backwards
  (define-key predictive-map [(meta shift iso-lefttab)] 'complete-predictive)
  (define-key predictive-map "\M-?" 'complete-predictive))



;;; ================================================================
;;;             Internal variables to do with completion

;; variables storing auto-learn and auto-add caches
(defvar predictive-auto-learn-cache nil)
(make-variable-buffer-local 'predictive-auto-learn-cache)

(defvar predictive-auto-add-cache nil)
(make-variable-buffer-local 'predictive-auto-add-cache)

;; permanent timer for flushing auto-learn and auto-add caches
(defvar predictive-flush-auto-learn-timer nil)
(make-variable-buffer-local 'predictive-flush-auto-learn-timer)

;; flag used to indicate failed major-mode setup function
(defvar predictive-disable-major-mode-setup nil)
(make-variable-buffer-local 'predictive-disable-major-mode-setup)



;;; ==============================================================
;;;          Internal variables to do with dictionaries

;; when set, overrides `predictive-main-dict' in a buffer
(defvar predictive-buffer-dict nil)
(make-variable-buffer-local 'predictive-buffer-dict)

;; when set, used in addition to `predictive-main-dict' or
;; `predictive-buffer-dict'
(defvar predictive-auxiliary-dict nil)
(make-variable-buffer-local 'predictive-auxiliary-dict)

;; variables storing lists of used dictionaries
(defvar predictive-global-used-dict-list nil)
(defvar predictive-used-dict-list nil)
(make-variable-buffer-local 'predictive-used-dict-list)




;;; ================================================================
;;;                  Convenience macros and functions

(defun predictive-capitalized-p (string)
  ;; Return t if string is capitalized (only first letter upper case), nil
  ;; otherwise.
  (and (> (length string) 0)
       (= (aref string 0) (upcase (aref string 0)))
       (not (= (aref string 0) (downcase (aref string 0))))
       (or (= 1 (length string))
	   (string= (substring string 1) (downcase (substring string 1))))))


(defmacro predictive-create-auxiliary-file-location ()
  ;; Create directory specified by `predictive-auxiliary-file-locaion' for
  ;; current buffer, if necessary.
  '(make-directory predictive-auxiliary-file-location t))


(defmacro predictive-buffer-local-dict-name ()
  ;; Return the buffer-local dictionary name
  '(intern
    (concat "dict-"
	    (replace-regexp-in-string
	     "\\." "-"
	     (file-name-nondirectory
	      (or (buffer-file-name) (buffer-name)))))))


(defmacro predictive-buffer-local-meta-dict-name ()
  ;; Return the buffer-local meta-dictionary name
  '(intern
    (concat "dict-meta-"
	    (replace-regexp-in-string
	     "\\." "-"
	     (file-name-nondirectory
	      (or (buffer-file-name)
		  (buffer-name)))))))


(defun predictive-assoc-delete-all (key alist)
  "Delete from ALIST all elements whose car is `equal' to KEY.
Return modified alist."
  (while (and (consp (car alist))
	      (equal (car (car alist)) key))
    (setq alist (cdr alist)))
  (let ((tail alist) tail-cdr)
    (while (setq tail-cdr (cdr tail))
      (if (and (consp (car tail-cdr))
	       (equal (car (car tail-cdr)) key))
	  (setcdr tail (cdr tail-cdr))
	(setq tail tail-cdr))))
  alist)



(defun predictive-lookup-word-p (word &optional ignored)
  "Return non-nil if WORD is found by `lookup-words', nil otherwise.
Potentially useful as a `predictive-auto-add-filter' (hence the
ignored second argument)."
  (member word (lookup-words word)))


(defun predictive-ispell-word-p (word &optional ignored)
  "Return non-nil if WORD is accepted by `ispell', nil otherwise.
Potentially useful as a `predictive-auto-add-filter' (hence the
ignored second argument).

WARNING! `predictive-ispell-word-p' can sometimes cause Emacs to
hang if it is used as a `predictive-auto-add-filter' and
`predictive-use-auto-learn-cache' is enabled. If this happens,
use C-g to terminate `predictive-ispell-word-p', then consider
disabling `predictive-use-auto-learn-cache'."
  ;; --- Code copied and adapted from `ispell-word' in ispell.el ---
  (let (poss)
    (ispell-set-spellchecker-params)    ; Initialize variables and dicts alists
    (ispell-accept-buffer-local-defs)   ; use the correct dictionary
    (ispell-send-string "%\n")	      ; put in verbose mode
    (ispell-send-string (concat "^" word "\n"))
    ;; wait until ispell has processed word
    (with-local-quit
      (while (progn
	       (ispell-accept-output)
	       (not (string= "" (car ispell-filter))))))
    ;;(ispell-send-string "!\n") ;back to terse mode.
    (setq ispell-filter (cdr ispell-filter)) ; remove extra \n
    (and ispell-filter (listp ispell-filter)
	 (<= (length ispell-filter) 1)
	 (setq poss (ispell-parse-output (car ispell-filter))))
    ;; return t if word is correct
    ;;(when (null poss) (message "Error in ispell process"))
    (or (eq poss t) (stringp poss))))




;;; ===============================================================
;;;                   The minor mode definition

;; the mode variable
(defcustom predictive-mode nil
  "Non-nil if Predictive Completion mode is enabled.
Setting this variable directly will have no effect. Use
`predictive-mode' command instead."
  :group 'predictive
  :type 'boolean
  :set (lambda (symbol value) (predictive-mode (or value 0)))
  :initialize 'custom-initialize-default)

(make-variable-buffer-local 'predictive-mode)


;; setup the mode-line indicator
(add-to-list 'minor-mode-alist
	     '(predictive-mode
	       (" Predict"
		(predictive-which-dict-mode
		 ("["
		  (:eval
		   (let ((name (predictive-which-dict-name)))
		     (add-text-properties
		      0 (length (car name)) `(help-echo ,(cdr name))
		      (car name))
		     (car name)))
		  "]")))))


;; add the minor mode keymap to the list
(let ((existing (assq 'predictive-mode minor-mode-map-alist)))
  (if existing
      (setcdr existing predictive-map)
    (push (cons 'predictive-mode predictive-map) minor-mode-map-alist)))



(defun predictive-mode (&optional arg)
  "Toggle Predictive Completion mode.
With no argument, this command toggles the mode.
A positive prefix argument turns the mode on.
A negative prefix argument turns it off.

Note that simply setting the minor-mode variable
`predictive-mode' is not sufficient to enable predictive
mode. Use the command `predictive-mode' instead.

Predictive Completion mode implements predictive text completion,
in an attempt to save on typing. It looks up completions for the
word currently being typed in a dictionary.

Although the English dictionary supplied with the Predictive
Completion Mode package gives quite good results \"out of the
box\", for best results you're strongly encouraged to create your
own dictionaries and train them on your own text. See the Predictive
Completion Manual for more information.

See the `predictive' and `completion-ui' customization groups for
documentation on the many and various configuration options, and
the Predictive Completion Manual for fuller information.

When `predictive-auto-complete' is enabled, words will
automatically be completed as you type. Otherwise, to complete
the word at or next to the point, the following keys can be used:

\\{predictive-map}\

When completing a word, the following keys are available:

\\{completion-overlay-map}\

To show/hide the completion popup-tip:

\\{completion-popup-tip-map}\

When the popup-tip is displayed:

\\{completion-popup-tip-active-map}\

To bring up the completion menu:

\\{completion-menu-map}\

To display the completion popup frame:

\\{completion-popup-frame-map}\

When within a pop-up frame:

\\{completion-popup-frame-mode-map}"

  (interactive "P")

  (cond
   ;; do nothing if enabling/disabling predictive mode and it is already
   ;; enabled/disabled
   ((and arg (eq predictive-mode (> (prefix-numeric-value arg) 0))))


   ;; ----- enabling predictive mode -----
   ((not predictive-mode)
    ;; make sure main dictionaries are loaded
    (mapc 'predictive-load-dict (predictive-main-dict))
    ;; make sure modified dictionaries used in the buffer are saved when the
    ;; bufer is killed
    (when predictive-dict-autosave-on-kill-buffer
      (add-hook 'kill-buffer-query-functions
		'predictive-save-used-dicts nil 'local))
    ;; load/create the buffer-local dictionary if using it, and make sure it's
    ;; saved and unloaded when buffer is killed
    (when predictive-use-buffer-local-dict
      (predictive-load-buffer-local-dict)
      (add-hook 'kill-buffer-hook 'predictive-unload-buffer-local-dict
		nil 'local))
    ;; make sure auto-learn/add caches are flushed if buffer is killed
    (add-hook 'kill-buffer-hook 'predictive-flush-auto-learn-caches
	      nil 'local)

    ;; look up major mode in major-mode-alist and call any matching function
    ;; with a positive argument to indicate enabling
    (let ((modefunc (assq major-mode predictive-major-mode-alist)))
      (when modefunc
	(if (functionp (cdr modefunc))
	    (unless (funcall (cdr modefunc) 1)
	      (warn (concat "Predictive major-mode setup function %s "
			    "failed; %s support disabled")
		       (cdr modefunc) major-mode)
	      (setq predictive-disable-major-mode-setup t))
	  (error "Wrong type in `predictive-major-mode-alist': functionp, %s"
		 (prin1-to-string (cdr modefunc))))))

    ;; turn on auto-completion mode if necessary
    (set (make-local-variable 'auto-completion-source) 'predictive)
    (when predictive-auto-complete (auto-completion-mode 1))
    ;; setup idle-timer to flush auto-learn and auto-add caches
    (setq predictive-flush-auto-learn-timer
	  (tf-run-with-idle-timer predictive-flush-auto-learn-delay t
				  0.1 t nil
				  'predictive-flush-auto-learn-caches 'idle))
    ;; set the mode variable and run the hook
    (setq predictive-mode t)
    (run-hooks 'predictive-mode-hook)
    (message "Predictive mode enabled"))


   ;; ----- disabling predictive mode -----
   (predictive-mode
    ;; turn off auto-completion mode if necessary
    (kill-local-variable 'auto-completion-source)
    (when predictive-auto-complete (auto-completion-mode -1))
    ;; cancel auto-learn timer and flush the caches
    (cancel-timer predictive-flush-auto-learn-timer)
    (predictive-flush-auto-learn-caches)

    ;; save the dictionaries
    (when predictive-dict-autosave-on-mode-disable
      (predictive-save-used-dicts 'no-save-query))
    (when predictive-use-buffer-local-dict
      (predictive-unload-buffer-local-dict))

    ;; if major-mode setup function failed to load, just reset the flag
    (if predictive-disable-major-mode-setup
	(setq predictive-disable-major-mode-setup nil)
      ;; otherwise, look up major mode in major-mode-alist and call any
      ;; matching function with a negative argument to indicate disabling
      (let ((modefunc (assq major-mode predictive-major-mode-alist)))
	(when modefunc
	  (condition-case nil
	      (if (functionp (cdr modefunc))
		  (funcall (cdr modefunc) -1)
		(error (concat "Wrong type in `predictive-major-mode-alist': "
			       "functionp, %s"
			       (prin1-to-string (cdr modefunc)))))
	    (error
	     (warn (concat "Predictive major-mode setup function failed "
			   "whilst disabling: %s")
		   (prin1-to-string (cdr modefunc))))))))

    ;; remove hooks
    (remove-hook 'kill-buffer-hook 'predictive-flush-auto-learn-caches 'local)
    (remove-hook 'kill-buffer-query-functions
		 'predictive-save-used-dicts 'local)
    (remove-hook 'kill-buffer-hook 'predictive-unload-buffer-local-dict 'local)

    ;; delete local variable bindings
    (kill-local-variable 'predictive-used-dict-list)
    ;; reset the mode variable and run the hook
    (setq predictive-mode nil)
    (run-hooks 'predictive-mode-disable-hook)
    (message "Predictive mode disabled"))
   ))



(defun turn-on-predictive-mode ()
  "Turn on predictive mode. Useful for adding to hooks."
  (unless predictive-mode (predictive-mode)))




;;; ================================================================
;;;       Public functions for predictive mode dictionaries

(defun predictive-set-main-dict (dict)
  "Set the main dictionary for the current buffer.
To set the default main dictionary, you should customize
`predictive-main-dict' instead."
  (interactive (list (read-dict "Dictionary: " nil nil 'allow-unloaded)))
  (cond
   ((symbolp dict) (setq dict (symbol-value dict)))
   ;; if DICT is a string, load DICT
   ((stringp dict)
    (let ((dic (predictive-load-dict dict)))
      (if (dictree-p dic)
	  (setq dict dic)
	(error "Dictionary %s could not be loaded" dict)))))
  ;; unload previous main dictionary
  (mapc 'predictive-unload-dict
	(if predictive-buffer-dict
	    (if (listp predictive-main-dict)
		predictive-main-dict
	      (list predictive-main-dict))
	  (if (listp predictive-buffer-dict)
	      predictive-buffer-dict
	    (list predictive-buffer-dict))))
  ;; set new main dictionary
  (setq dict (intern-soft (dictree-name dict)))
  (setq predictive-buffer-dict dict))



(defun predictive-load-dict (dict)
  "Load the dictionary DICT into the current buffer.

DICT must either be the name of a loaded dictionary, or the name
of a dictionary to be found somewhere in the load path. Returns
the dictionary, or nil if dictionary fails to
load. Interactively, it is read from the mini-buffer."

  (interactive (list (read-dict "Dictionary: " nil nil 'allow-unloaded)))
  ;; sort out argument
  (when (symbolp dict) (setq dict (symbol-name dict)))
  (cond
   ;; DICT is already a dictionary
   ((dictree-p dict))
   ;; DICT is the name of a loaded dictionary
   ((condition-case nil
	(symbol-value (intern-soft dict))
      (void-variable nil))
    (let ((dic (symbol-value (intern-soft dict))))
      (if (dictree-p dic)
	  (setq dict dic)
	(error "%s is not a dictionary" dict))))
   ;; DICT is the name of an unloaded dictionary
   (t
    (let ((dic (dictree-load dict)))
      (if (dictree-p dic)
	  (setq dict dic)
	(if (called-interactively-p 'any)
	    (error "Dictionary %s could not be loaded" dict)
	  (setq dict nil))))))

  (when dict
    ;; if we successfully loaded the dictionary, add it to global and buffer's
    ;; used dictionary lists (note: can't use add-to-list because we want
    ;; comparison with eq, not equal)
    (unless (memq dict predictive-used-dict-list)
      (push dict predictive-used-dict-list))
    (let ((entry (assq dict predictive-global-used-dict-list)))
      (if entry
	  (unless (memq (current-buffer) (cdr entry))
	    (push (current-buffer) (cdr entry)))
	(push (cons dict (list (current-buffer)))
	      predictive-global-used-dict-list)))

    ;; add buffer-local hook to unload dictionaries before killing the buffer
    (add-hook 'kill-buffer-hook 'predictive-kill-buffer-unload-dicts nil t)

    ;; indicate successful loading and return loaded dict
    (message "Dictionary %s loaded in buffer %s"
	     (dictree-name dict) (buffer-name (current-buffer)))
    dict))



(defun predictive-unload-dict (dict &optional dont-save)
  "Unload dictionary DICT from the current buffer.

If the dictionary is not in use by any other buffers, this will
also unload the dictionary from Emacs."
  (interactive (list (read-dict "Dictionary: "
				nil predictive-used-dict-list)))
  ;; sort out argument
  (and (symbolp dict) (setq dict (symbol-value dict)))

  ;; remove dict from buffer's used dictionary list
  (setq predictive-used-dict-list (delq dict predictive-used-dict-list))
  (message "Dictionary %s unloaded from buffer %s"
	   (dictree-name dict) (buffer-name (current-buffer)))

  ;; remove buffer from dict's buffer list in global used dictionary list
  (let ((entry (assq dict predictive-global-used-dict-list)))
    (when entry
      (setq entry (setcdr entry (delq (current-buffer) (cdr entry)))))

    ;; unload dictionary if it's not longer used by any buffer and isn't the
    ;; main dict or listed in `predictive-dict-lock-loaded-list'
    (unless (or entry
		(memq (intern-soft (dictree-name dict))
		      (if (listp (default-value 'predictive-main-dict))
			  (default-value 'predictive-main-dict)
			(list (default-value 'predictive-main-dict))))
		(memq (intern-soft (dictree-name dict))
		      predictive-dict-lock-loaded-list))
      (setq predictive-global-used-dict-list
	    (assq-delete-all dict predictive-global-used-dict-list))
      (dictree-unload dict dont-save))))



(defun predictive-kill-buffer-unload-dicts ()
  "Called when a buffer with loaded dictionaries is killed,
to clean up `predictive-global-used-dict-list'."
  (mapc 'predictive-unload-dict predictive-used-dict-list))



(defun predictive-save-dict (dict)
  "Save dictionary DICT to its associated file.
Use `predictive-write-dict' to save to a different file.

See also `predictive-dict-compilation'."
  (interactive (list (read-dict "Dictionary to save: ")))
  (and (symbolp dict) (setq dict (symbol-value dict)))
  (when dict (dictree-save dict predictive-dict-compilation)))



(defun predictive-write-dict (dict filename &optional overwrite)
  "Write dictionary DICT to file FILENAME.

If optional argument OVERWRITE is non-nil, no confirmation will
be asked for before overwriting an existing file.

See also `predictive-dict-compilation'."
  (interactive (list (read-dict "Dictionary to write: ")
		     (read-file-name "File to write to: ")
		     current-prefix-arg))
  (and (symbolp dict) (setq dict (symbol-value dict)))
  (dictree-write dict filename overwrite predictive-dict-compilation))



(defun predictive-create-dict
  (&optional dictname file populate autosave speed no-prefixes)
  "Create a new predictive mode dictionary called DICTNAME.

The optional argument FILE specifies a file to associate with the
dictionary. The dictionary will be saved to this file by default
\(similar to the way a file is associated with a buffer).

If POPULATE is not specified, create an empty dictionary. If
POPULATE is specified, populate the dictionary from that file
\(see `dict-populate-from-file').

If the optional argument AUTOSAVE is t, the dictionary will
automatically be saved when it is unloaded. If it is any other
value, all unsaved changes are lost when it is unloaded. Defaults
to `predictive-dict-autosave'.

The optional argument SPEED sets the desired speed with which
string should be completed using the dictionary, in seconds. It
defaults to `predictive-completion-speed'.

Prefix relationships are automatically defined if
`predictive-auto-define-prefixes' is enabled, unless NO-PREFIXES
is non-nil.

Interactively, DICTNAME and FILE are read from the
minibuffer. SPEED and AUTOSAVE use the defaults provided by
`predictive-completion-speed' and `predictive-dict-autosave'
respectively."

  (interactive (list
		(read-string "Dictionary name: ")
		(read-file-name "File to save to \(optional): " nil "")
		(read-file-name
		 "File to populate from \(leave blank for empty dictionary\): "
		 nil "")))

  ;; sort out arguments
  (and (stringp populate) (string= populate "") (setq populate nil))
  (unless (or (null populate) (file-regular-p populate))
    (setq populate nil)
    (message "File %s does not exist; creating empty dictionary" populate))
  (and dictname (symbolp dictname) (setq dictname (symbol-name dictname)))

  ;; confirm if overwriting existing dict, then unload existing one
  ;; (Note: the condition-case is there to work around bug in intern-soft. It
  ;;        should return nil when the symbol isn't interned, but seems to
  ;;        return the symbol instead in some Emacs versions)
  (when (or (null dictname)
	    (and (null (dictree-p (condition-case nil
				      (symbol-value (intern-soft dictname))
				    (void-variable nil))))
		 (setq dictname (intern dictname)))
	    (and (or (null (called-interactively-p 'any))
		     (and (y-or-n-p
			   (format (concat
				    "Dictionary %s already exists. Replace "
				    "it? (you'll be prompted to save any "
				    "unsaved changes first) ")
				   dictname))
			  (dictree-unload (symbol-value (intern-soft dictname)))))
		 (setq dictname (intern dictname))))

    (let (dict
	  (complete-speed (if speed speed predictive-completion-speed))
	  (autosave (if autosave (eq autosave t) predictive-dict-autosave)))

      ;; create the new dictionary
      (setq dict (dictree-create dictname file autosave nil
				 '< '+ 'predictive-dict-rank-function nil nil
				 nil nil complete-speed nil complete-speed))
      ;; populate it
      (if (null populate)
	  (when (called-interactively-p 'interactive)
	    (message "Created dictionary %s" dictname))
	(dictree-populate-from-file dict populate nil nil
				    (lambda (data) (or data 0)))
	;; define prefixes when `predictive-auto-define-prefixes' is enabled
	(when (and predictive-auto-define-prefixes (not no-prefixes))
	  (predictive-define-all-prefixes dict nil nil 'interactive))
	(when (called-interactively-p 'interactive)
	  (message "Created dictionary %s and populated it from file %s"
		   dictname populate)))

      ;; return the new dictionary
      dict)))



(defun predictive-create-meta-dict
  (name dictlist &optional filename autosave speed)
  "Create a new predictive mode meta-dictionary called NAME,
based on the dictionaries in DICTLIST.

The other arguments are as for `predictive-create-dict'."

  (interactive
   (list (read-string "Dictionary name: ")
	 (let ((dic (read-dict
		     "Constituent dictionary (blank to end): " nil))
	       diclist)
	   (while dic
	     (setq diclist (nconc diclist (list dic)))
	     (setq dic
		   (read-dict "Constituent dictionary (blank to end): " nil)))
	   diclist)
	 (read-file-name "File to save to \(optional): " nil "")))

  ;; sort out arguments
  (when (called-interactively-p 'any)
    (when (< (length dictlist) 2)
      (error "Can't see any point in creating a meta-dictionary\
 based on less than two dictionaries"))
    (when (symbolp name) (setq name (symbol-name name)))
    (when (string= filename "") (setq filename nil)))
  (setq dictlist
	(mapcar (lambda (d) (if (symbolp d) (symbol-value d) d)) dictlist))

  ;; confirm if overwriting existing dict, then unload existing one
  ;; (Note: we need the condition-case to work around bug in intern-soft. It
  ;;        should return nil when the symbol isn't interned, but seems to
  ;;        return the symbol instead)
  (when (or (and (null (dictree-p (condition-case nil
				      (symbol-value (intern-soft name))
				    (void-variable nil))))
		 (setq name (intern name)))
	    (or (null (called-interactively-p 'any))
		(and (y-or-n-p
		      (format "Dictionary %s already exists. Replace it? "
			      name))
		     (dictree-unload (symbol-value (intern-soft name)))
		     (setq name (intern name)))))

    (or speed (setq speed predictive-completion-speed))
    (or autosave (setq autosave autosave predictive-dict-autosave))

    ;; create and return the new dictionary
    (dictree-create-meta-dict dictlist name filename autosave nil
			      '+ nil nil nil nil speed)))



(defun predictive-dump-dict-to-buffer (dict &optional buffer)
  "Dump words and their associated weights
from dictionary DICT to BUFFER. If BUFFER exists, data will be
appended to the end of it. Otherwise, a new buffer will be
created. If BUFFER is omitted, the current buffer is used.

If saved to a file, the dumped data can be used to populate a
dictionary when creating it using `predictive-create-dict'. See
also `predictive-dump-dict-to-file'."
  (interactive (list (read-dict
		      (let ((dic (car (predictive-current-dict))))
			(if dic
			    (concat "Dictionary to dump (default "
				    (dictree-name dic) "): ")
			  "Dictionary to dump: "))
		      (car (predictive-current-dict)))
		     (read-buffer "Buffer to dump to: "
				  (buffer-name (current-buffer)))))
  (and (symbolp dict) (setq dict (symbol-value dict)))
  (dictree-dump-to-buffer dict buffer 'string))



(defun predictive-dump-dict-to-file (dict &optional filename overwrite)
  "Dump words and their associated weights
from dictionary DICT to a text file FILENAME. If BUFFER exists,
data will be appended to the end of it. Otherwise, a new buffer
will be created.

If OVERWRITE is non-nil, FILENAME will be overwritten *without*
prompting if it already exists. Interactively, OVERWRITE is set
by supplying a prefix arg.

The dumped data can be used to populate a dictionary when
creating it using `predictive-create-dict'. See also
`predictive-dump-dict-to-buffer'."
  (interactive (list (read-dict
		      (let ((dic (car (predictive-current-dict))))
			(if dic
			    (concat "Dictionary to dump (default "
				    (dictree-name dic) "): ")
			  "Dictionary to dump: "))
		      (car (predictive-current-dict)))
		     (read-file-name "File to dump to: ")
		     current-prefix-arg))
  (and (symbolp dict) (setq dict (symbol-value dict)))
  (dictree-dump-to-file dict filename 'string overwrite))



(defun predictive-add-to-dict (dict word &optional weight)
  "Insert WORD into predictive mode dictionary DICT.

Optional argument WEIGHT sets the weight. If the word is not in the
dictionary, it will be added to the dictionary with initial weight WEIGHT \(or
0 if none is supplied\). If the word is already in the dictionary, its weight
will be incremented by WEIGHT \(or by 1 if WEIGHT is not supplied).

Interactively, WORD and DICT are read from the minibuffer, and WEIGHT is
specified by the prefix argument."
  (interactive (list (read-dict
		      (let ((dic (car (predictive-current-dict))))
			(if dic
			    (concat "Dictionary to add to (default "
				    (dictree-name dic) "): ")
			  "Dictionary to add to: "))
		      (car (predictive-current-dict)))
		     (read-from-minibuffer
		      (concat "Word to add"
			      (let ((str (thing-at-point 'word)))
				(when str (concat " (default \"" str "\")")))
			      ": "))
		     current-prefix-arg))
  ;; sort out arguments
  (and (symbolp dict) (setq dict (symbol-value dict)))
  (when (called-interactively-p 'any)
    (when (string= word "")
      (let ((str (thing-at-point 'word)))
	(if (null str)
	    (error "No word supplied")
	  (setq word str))))
    ;; sort out weight argument
    (unless (null weight) (setq weight (prefix-numeric-value weight))))
  ;; strip any text properties from word
  (set-text-properties 0 (length word) nil word)

  ;; insert word
  (let* ((defpref (and predictive-auto-define-prefixes
		       (not (dictree-meta-dict-p dict))
		       (not (dictree-member-p dict word))))
	 (newweight (dictree-insert dict word (or weight 0)
				    (when (null weight)
				      (lambda (a b) (1+ b)))))
	 pweight)

    ;; if adding a new word, and we're automatically defining prefixes...
    (when defpref
      ;; define new word to be a prefix of all its completions
      (dolist (cmpl (cdr (dictree-complete dict word nil nil nil nil nil
					   (lambda (key data) key))))
	(predictive-define-prefix dict cmpl word))
      ;; define all prefixes of new word (note: `predictive-define-prefix'
      ;; does nothing if prefix isn't in dict, so no need to check that here)
      (dotimes (i (1- (length word)))
	(predictive-define-prefix dict word (substring word 0 (1+ i)))))

    ;; if word has associated prefixes, make sure weight of each prefix is at
    ;; least as great as word's new weight
    (unless (dictree-meta-dict-p dict)
      (let ((prefixes (dictree-get-property dict word :prefixes)))
	(dolist (prefix prefixes)
	  (setq pweight (dictree-lookup dict prefix))
	  (cond
	   ;; prefix has been deleted from dictionary
	   ((null pweight)
	    ;; confirm prefix really has been deleted
	    (unless (dictree-member-p dict prefix)
	      (dictree-put-property dict word :prefixes
				    (delete prefix prefixes))))
	   ;; prefix weight needs incrementing
	   ((< pweight newweight)
	    (dictree-insert dict prefix newweight (lambda (a b) a))))))))

  (when (called-interactively-p 'interactive)
    (message "\"%s\" added to dictionary %s" word (dictree-name dict))))



(defun predictive-remove-from-dict (dict word)
  "Delete WORD from predictive mode dictionary DICT.
Interactively, WORD and DICT are read from the minibuffer."
  (interactive (list (read-dict
		      (let ((dic (car (predictive-current-dict))))
			(if dic
			    (concat "Dictionary to delete from (default "
				    (dictree-name dic) "): ")
			  "Dictionary to delete from: "))
		      (car (predictive-current-dict)))
		     (read-from-minibuffer
		      (concat "Word to delete"
			      (let ((str (thing-at-point 'word)))
				(when str (concat " (default \"" str "\")")))
			      ": "))))
  ;; sort out arguments
  (and (symbolp dict) (setq dict (symbol-value dict)))
  (and (called-interactively-p 'any) (string= word "")
       (let ((str (thing-at-point 'word)))
	 (if (null str)
	     (error "No word supplied")
	   (set-text-properties 0 (length str) nil str)
	   (setq word str))))
  ;; delete word
  (if (dictree-delete dict word)
      (when (called-interactively-p 'any)
	(message "\"%s\" deleted from dictionary %s" word
		 (dictree-name dict)))
    (when (called-interactively-p 'any)
      (message "\"%s\" not found in dictionary %s" word
	       (dictree-name dict)))))



(defun predictive-reset-weight (dict word &optional weight)
  "Reset the weight of WORD in dictionary DICT to 0.

If WORD is null, reset weights of all words in the
dictionary (prompting for confirmation first if this is called
interactively).

If WEIGHT is supplied, reset to that value instead of
0. Interactively, WEIGHT is the numerical prefix argument."

  (interactive (list (read-dict
		      (let ((dic (car (predictive-current-dict))))
			(if dic
			    (concat "Dictionary (default "
				    (dictree-name dic) "): ")
			  "Dictionary: "))
		      (car (predictive-current-dict)))
		     (read-string
		      "Word to reset (leave blank to reset all words): ")
		     current-prefix-arg))

  ;; sort out arguments
  (and (symbolp dict) (setq dict (symbol-value dict)))
  (and (stringp word) (string= word "") (setq word nil))
  (cond
   ((null weight) (setq weight 0))
   ((called-interactively-p 'any)
    (setq weight (prefix-numeric-value weight))))

  ;; confirm interactive reset of all weights
  (when (or word
	    (not (called-interactively-p 'any))
	    (yes-or-no-p
	     (format "Really reset weights of all words in dictionary %s? "
		     (dictree-name dict))))
    ;; if a word was specified, reset its weight to 0
    (if word
	(and (dictree-insert dict word weight (lambda (a b) a))
	     (called-interactively-p 'interactive)
	     (message "Weight of \"%s\" in %s reset to %d"
		      word (dictree-name dict) weight))
      ;; if no word was specified, reset all weights to 0
      (let ((i 0)
	    (count (when (called-interactively-p 'interactive)
		     (dictree-size dict))))
	(when (called-interactively-p 'interactive)
	  (message "Resetting word weights in %s...(word 1 of %d)"
		   (dictree-name dict) count))
	(dictree-mapc
	 (if (called-interactively-p 'interactive)
	     (lambda (word ignored)
	       (setq i (1+ i))
	       (when (= (mod i 10) 0)
		 (message "Resetting word weights in %s...(word %d of %d)"
			  (dictree-name dict) i count))
	       (dictree-insert dict word weight (lambda (a b) a)))
	   (lambda (word ignored)
	     (dictree-insert dict word weight (lambda (a b) a))))
	 dict)
	(when (called-interactively-p 'interactive)
	  (message "Resetting word weights in %s...done" (dictree-name dict)))
	))))



(defun predictive-define-prefix (dict word prefix)
  "Add PREFIX to the list of prefixes for WORD in dictionary DICT.
The weight of PREFIX will automatically be kept at least as large
as the weight of WORD."
  (interactive (list (read-dict
		      (let ((dic (car (predictive-current-dict))))
			(if dic
			    (concat "Dictionary (default "
				    (dictree-name dic) "): ")
			  "Dictionary: "))
		      (car (predictive-current-dict)))
		     (setq word (read-string
				 (format "Word (default \"%s\"): "
					 (thing-at-point 'word))))
		     (let ((wrd (if (or (null word) (string= word ""))
				    (thing-at-point 'word)
				  word)))
		       (read-string
			(format "Prefix for \"%s\" (default \"%s\"): "
				wrd (or (predictive-guess-prefix wrd) ""))))
		     ))
  ;; sort out arguments
  (and (symbolp dict) (setq dict (symbol-value dict)))
  (when (called-interactively-p 'any)
    ;; default to word at point
    (when (or (null word) (string= word ""))
      (let ((str (thing-at-point 'word)))
	(if (null str)
	    (error "No word supplied")
	  (set-text-properties 0 (length str) nil str)
	  (setq word str))))
    ;; default to guessed prefix, throwing error if there is no guess
    (when (or (null prefix) (string= prefix ""))
      (setq prefix (predictive-guess-prefix word))
      (unless prefix (error "Prefix must be supplied for \"%s\"" word)))
    ;; throw error if word not in dict
    (when (not (dictree-member-p dict word))
      (error "\"%s\" not found in dictionary %s" word (dictree-name dict))))

    ;; prompt for confirmation if prefix isn't really a prefix for word
    (when (and (dictree-member-p dict word)
	       (dictree-member-p dict prefix)
	       (or (not (called-interactively-p 'any))
		   (and (> (length word) (length prefix))
			(string= (substring word 0 (length prefix)) prefix))
		   (y-or-n-p
		    (format
		     "\"%s\" is not a prefix of \"%s\". Continue anyway? "
		     prefix word))))

      (let ((prefixes (dictree-get-property dict word :prefixes)))
	;; unless prefix is already defined, define it
	(if (member prefix prefixes)
	    (when (called-interactively-p 'interactive)
	      (message "\"%s\" is already a prefix of \"%s\" in dictionary %s"
		       prefix word (dictree-name dict)))
	  (dictree-put-property dict word :prefixes (cons prefix prefixes))
	  (when (called-interactively-p 'interactive)
	    (message "Defined \"%s\" as prefix of \"%s\" in dictionary %s"
		     prefix word (dictree-name dict)))))

      ;; make sure prefix's weight is at least as large as word's
      (let ((weight (dictree-lookup dict word))
	    (pweight (dictree-lookup dict prefix)))
	(and weight (or (null pweight) (< pweight weight))
	     (dictree-insert dict prefix weight (lambda (a b) a))))))



(defun predictive-guess-prefix (word)
  "Guess a likely prefix for WORD.
See `predictive-define-prefix' and 'predictive-guess-prefix-suffixes'."
  (let ((suffix
	 (catch 'found
	   (dolist (sfx predictive-guess-prefix-suffixes)
	     (and (> (length word) (length sfx))
		  (string= (substring word
				      (- (length word) (length sfx))
				      (length word))
			   sfx)
		  (throw 'found sfx))))))
    (when suffix (substring word 0 (- (length word) (length suffix))))))



(defun predictive-undefine-prefix (dict word prefix)
  "Remove PREFIX from list of prefixes for WORD in dictionary DICT.
The weight of PREFIX will no longer automatically be kept at
least as large as the weight of WORD."
  (interactive (list (read-dict
		      (let ((dic (car (predictive-current-dict))))
			(if dic
			    (concat "Dictionary (default "
				    (dictree-name dic) "): ")
			  "Dictionary: "))
		      (car (predictive-current-dict)))
		     (setq word (read-string
				 (format "Word (default \"%s\"): "
					 (thing-at-point 'word))))
		     (let ((wrd (if (or (null word) (string= word ""))
				    (thing-at-point 'word)
				  word)))
		       (read-string
			(format "Remove prefix of \"%s\" (default \"%s\"): "
				wrd (or (predictive-guess-prefix wrd) ""))))
		     ))
  ;; sort out arguments
  (and (symbolp dict) (setq dict (symbol-value dict)))
  (when (called-interactively-p 'any)
    (when (or (null word) (string= word ""))
      (let ((str (thing-at-point 'word)))
	(if (null str)
	    (error "No word supplied")
	  (set-text-properties 0 (length str) nil str)
	  (setq word str))))
    (when (or (null prefix) (string= prefix ""))
      (setq prefix (predictive-guess-prefix word))))

  ;; delete prefix, displaying message if called interactively
  (let ((prefixes (dictree-get-property dict word :prefixes)))
    (if (and (called-interactively-p 'interactive)
	     (not (member prefix prefixes)))
	(message "\"%s\" is not a prefix of \"%s\" in dictionary %s"
		 prefix word (dictree-name dict))
      (dictree-put-property dict word :prefixes (delete prefix prefixes))
      (when (called-interactively-p 'interactive)
	(message "Prefix \"%s\" of \"%s\" removed from dictionary %s"
		 prefix word (dictree-name dict))))))



(defun predictive-define-all-prefixes
  (dict &optional prefix length interactive)
  "Define prefix relationships for PREFIX in dictionary DICT.
PREFIX will be added to the prefix list of any word for which it
is a prefix. Predictive mode will then automatically ensure that
the weight of PREFIX is always at least as great as the weight of
those words.

If PREFIX is null, it defines prefix relationships for *all*
words in DICT that are prefixes of other words. In this case,
optional argument LENGTH specifies a minimum length for a prefix
word\; prefix words shorter than this minimum will be ignored. If
it is zero or negative, all prefix words will be included.

Interactively, DICT and PREFIX are read from the minibuffer, and
LENGTH is the integer prefix argument.

Since defining all prefixes can take some time, progress messages
are displayed in the echo area when called interactively, or when
INTERACTIVE is non-nil."
  (interactive (list (read-dict
		      (let ((dic (car (predictive-current-dict))))
			(if dic
			    (concat "Dictionary (default "
				    (dictree-name dic) "): ")
			  "Dictionary: "))
		      (car (predictive-current-dict)))
		     (read-string "Prefix (leave blank for all): ")
		     (prefix-numeric-value current-prefix-arg)))
  ;; sort out arguments
  (and (symbolp dict) (setq dict (symbol-value dict)))
  (and (stringp prefix) (string= prefix "") (setq prefix nil))
  (and (called-interactively-p 'interactive) (setq interactive t))

  ;; create function for defining prefixes
  (let ((prefix-fun
	 (lambda (word)
	   (let (completion-list string)
	     ;; deal with capitalisation
	     (if (and predictive-ignore-initial-caps
		      (predictive-capitalized-p word))
		 (setq string (list word (downcase word)))
	       (setq string word))
	     ;; find completions of word, dropping first which is always the
	     ;; word itself
	     (setq completion-list
		   (cdr (dictree-complete dict string nil nil nil nil nil
					  (lambda (key data) key))))
	     ;; define the word to  be a prefix for all its completions
	     (dolist (cmpl completion-list)
	       (predictive-define-prefix dict cmpl word))
	     ))))

    ;; display informative messages if called interactively
    (when interactive
      (if prefix
	  (message "Defining prefix \"%s\" in %s..."
		   prefix (dictree-name dict))
      (message "Defining prefixes in %s..." (dictree-name dict))))
    (let ((i 0)
	  (count (when interactive (dictree-size dict))))
      (and interactive prefix
	   (message "Defining prefixes in %s...(word 1 of %d)"
		    (dictree-name dict) count))

      ;; define one prefix
      (if prefix (funcall prefix-fun prefix)

	;; define all prefixes
	(dictree-mapc
	 (lambda (word weight)
	   (and interactive
		(setq i (1+ i)) (= 0 (mod i 50))
		(message "Defining prefixes in %s...(word %d of %d)"
			 (dictree-name dict) i count))
	   ;; ignore word if it's too short
	   (unless (and length (< (length word) length))
	     (funcall prefix-fun word)))
	 dict 'string))

      (when interactive
	(if prefix
	    (message "Defining prefix \"%s\" in %s...done"
		     prefix (dictree-name dict))
	  (message "Defining prefixes in %s...done" (dictree-name dict)))))
    ))



(defun predictive-undefine-all-prefixes (dict &optional prefix)
  "Remove PREFIX from all prefix relationships in DICT.
Predictive mode will no longer ensure that the weight of PREFIX
is greater than that of other words.

If PREFIX is null, remove all prefix relationships (prompting for
confirmation first if called interactively)."
  (interactive (list (read-dict
		      (let ((dic (car (predictive-current-dict))))
			(if dic
			    (concat "Dictionary (default "
				    (dictree-name dic) "): ")
			  "Dictionary: "))
		      (car (predictive-current-dict)))
		     (read-string "Prefix (leave blank for all): ")))
  ;; sort out arguments
  (and (symbolp dict) (setq dict (symbol-value dict)))
  (and (stringp prefix) (string= prefix "") (setq prefix nil))
  ;; prompt for confirmation if called interactively to remove all prefixes
  (when (or (not (called-interactively-p 'any))
	    prefix
	    (y-or-n-p
	     (format
	      "Really remove all prefix relationships from dictionary %s? "
	      (dictree-name dict))))

    ;; display informative message so people have something to look at whilst
    ;; calculating dictionary size
    (if prefix
	(message "Undefining prefix \"%s\" in %s..."
		 prefix (dictree-name dict))
      (message "Undefining prefixes in %s..." (dictree-name dict)))

    (let ((count (when (called-interactively-p 'interactive)
		   (dictree-size dict)))
	  (interactive (called-interactively-p 'interactive))
	  (i 0) prefix-fun prefix-list)

      ;; define function to be mapped over dictionary words, for removing
      ;; single prefix or all prefixes, as appropriate
      (if prefix
	  (setq prefix-fun
		(lambda (word dummy)
		  (and interactive (setq i (1+ i)) (= 0 (mod i 50))
		       (message
			"Undefining prefix \"%s\" in %s...(word %d of %d)"
			prefix (dictree-name dict) i count))
		  ;; remove PREFIX if it appears in word's prefix list
		  (when (member prefix
				(setq prefix-list
				      (dictree-get-property
				       dict word :prefixes)))
		    (dictree-put-property dict word :prefixes
					  (delete prefix prefix-list)))))
	(setq prefix-fun
	      (lambda (word dummy)
		(and interactive (setq i (1+ i)) (= 0 (mod i 50))
		     (message
		      "Undefining all prefixes in %s...(word %d of %d)"
		      (dictree-name dict) i count))
		;; clear word's prefix list
		(dictree-put-property dict word :prefixes nil))))


      ;; do actual work...
      (when (called-interactively-p 'interactive)
	(if prefix
	    (message "Undefining prefix \"%s\" in %s...(word 1 of %d)"
		     prefix (dictree-name dict) count)
	  (message "Undefining prefixes in %s...(word 1 of %d)"
		   (dictree-name dict) count)))
      (dictree-mapc prefix-fun dict 'string)
      (when (called-interactively-p 'interactive)
	(if prefix
	    (message "Undefining prefix \"%s\" in %s...done"
		     prefix (dictree-name dict))
	  (message "Undefining prefixes in %s...done" (dictree-name dict))))
      )))



(defun predictive-learn-from-buffer (&optional buffer dict all)
  "Learn word weights from BUFFER (defaults to the current buffer).

The word weight of each word in dictionary DICT is incremented by
the number of occurences of that word in the buffer. DICT can
either be a dictionary, or a list of dictionaries. If DICT is not
supplied, it defaults to all dictionaries used by
BUFFER. However, DICT must be supplied if ALL is specified (see
below).

By default, only occurences of a word that occur in a region
where the dictionary is active are taken into account. If
optional argument ALL is non-nil, all occurences are taken into
account. In this case, a dictionary must be sprecified.

Interactively, BUFFER and DICT are read from the mini-buffer, and
ALL is specified by the presence of a prefix argument.

See also `predictive-fast-learn-or-add-from-buffer'."

  (interactive (list (read-buffer "Buffer to learn from: "
				  (buffer-name (current-buffer)) t)
		     (read-dict
		      "Dictionary to update (defaults to all in use): "
		      t)
		     current-prefix-arg))
  ;; sort out and sanity check arguments
  (and (called-interactively-p 'any) (eq dict t) (setq dict nil))
  (and (symbolp dict) (setq dict (symbol-value dict)))
  (and all (null dict)
       (error "Argument ALL supplied but no dictionary specified"))

  (let ((d 0) i dict-list numdicts dictsize dictname
	restore-mode regexp currdict)
    (save-excursion
      ;; switch on predictive mode in the buffer if necessary
      (when buffer (set-buffer buffer))
      (unless all
	(if predictive-mode (setq restore-mode t) (predictive-mode 1)))

      ;; either use list of dictionaries used in buffer, or bundle single
      ;; dictionary inside list so dolist can handle it
      (if (null dict)
	  (setq dict-list predictive-used-dict-list)
	(setq dict-list (list dict)))
      (setq numdicts (length dict-list))

      ;; loop over all dictionaries in dictionary list
      (dolist (dict dict-list)
	;; initialise dictionary counters etc. for messages
	(setq dictname (dictree-name dict))
	(setq d (1+ d))  ; counts dictionaries
	(if (> numdicts 1)
	    (message "Learning words for dictionary %s...(dict %d of %d)"
		     dictname d numdicts)
	  (message "Learning words for dictionary %s..." dictname))
	;; initialise word counters etc. for messages
	(setq dictsize (dictree-size dict))
	(setq i 0)       ; counts words
	(if (> numdicts 1)
	    (message "Learning words for dictionary %s...(dict %d of %d,\
 word 1 of %d)" dictname d numdicts dictsize)
	  (message "Learning words for dictionary %s...(word 1 of %d)"
		   dictname dictsize))

	;; map over all words in dictionary
	(dictree-mapc
	 (lambda (word weight)   ; (value passed to weight is ignored)
	   ;; construct regexp for word
	   (setq regexp (regexp-quote word))
	   (when (= ?w (char-syntax (aref word 0)))
	     (setq regexp (concat "\\b" regexp)))
	   (when (= ?w (char-syntax (aref word (1- (length word)))))
	     (setq regexp (concat regexp "\\b")))
	   ;; count occurences of current word
	   (setq weight 0)
	   (goto-char (point-min))
	   (while (re-search-forward regexp nil t)
	     (if all
		 (setq weight (1+ weight))
	       ;; if ALL is nil, only count occurence if the active
	       ;; dictionary at that location matches the dictionary we're
	       ;; working on
	       (setq currdict (predictive-current-dict))
	       (when (or (and (listp currdict) (memq dict currdict))
			 (eq dict currdict))
		 (setq weight (1+ weight)))))
	   ;; increment word's weight
	   (predictive-add-to-dict dict word weight)
	   (when (= 0 (mod (setq i (1+ i)) 10))
	     (if (> numdicts 1)
		 (message "Learning words for dictionary %s...(dict %d of %d,\
 word %d of %d)" dictname d numdicts i dictsize)
	       (message "Learning words for dictionary %s...(word %d of %d)"
			dictname i dictsize))))
	 dict 'string)   ; map over all words in dictionary

	(message "Learning words for dictionary %s...done" dictname))

      ;; restore predictive-mode state
      (unless (or all restore-mode) (predictive-mode -1))
      )))



(defun predictive-learn-from-file (file &optional dict all)
  "Learn word weights from FILE.

The word weight of each word in dictionary DICT is incremented by
the number of occurences of that word in the file. DICT can
either be a dictionary, or a list of dictionaries. If DICT is not
supplied, it defaults to all dictionaries used by FILE. However,
DICT must be supplied if ALL is specified, see below.

By default, only occurences of a word that occur in a region
where the dictionary is active are taken into account. If
optional argument ALL is non-nil, all occurences are taken into
account. In this case, a dictionary must be specified.

Interactively, FILE and DICT are read from the mini-buffer, and
ALL is specified by the presence of a prefix argument."
  (interactive (list (read-file-name "File to learn from: " nil nil t)
		     (read-dict
		      "Dictionary to update (defaults to all in use): "
		      t)
		     current-prefix-arg))
  ;; sort out arguments
  (and (called-interactively-p 'any) (eq dict t) (setq dict nil))
  (and (symbolp dict) (setq dict (symbol-value dict)))
  (save-excursion
    ;; open file in a buffer
    (let (visiting buff)
      (if (setq buff (get-file-buffer file))
	  (setq visiting t)
	(find-file file))
      ;; learn from the buffer
      (predictive-learn-from-buffer buff dict all)
      (unless visiting (kill-buffer buff)))))



(defun predictive-fast-learn-or-add-from-buffer (&optional buffer dict all)
  "Learn word weights from BUFFER (defaults to the current buffer).

The word weight of each word in dictionary DICT is incremented by
the number of occurences of that word in the buffer. DICT can
either be a dictionary, or a list of dictionaries. If DICT is not
supplied, it defaults to all dictionaries used by
BUFFER. However, DICT must be supplied if ALL is specified, see
below.

By default, only occurences of a word that occur in a region
where the dictionary is active are taken into account. If
optional argument ALL is non-nil, all occurences are taken into
account. In this case, a dictionary must be sprecified.

Note that this function takes the setting of
`predictive-auto-add-to-dict' and related options into
account. If an explicit dictionary is supplied, new words will be
added to that dictionary if `predictive-auto-add-to-dict' has any
non-nil value. If DICT is not supplied, the
`predictive-auto-add-to-dict' setting has the usual effect
\(which see\). If `predictive-add-to-dict-ask' is non-nil, you
will be prompted to confirm each and every word before it is
added \(so you may well wish to temporarily set
`predictive-add-to-dict-ask to nil before using this
function\). The `predictive-auto-add-min-chars' and
`predictive-auto-add-filter' variables also have their usual
effect.

Interactively, BUFFER and DICT are read from the mini-buffer, and
ALL is specified by the presence of a prefix argument.

This function is faster then `predictive-learn-from-buffer' for
large dictionaries, but will miss any words not consisting
entirely of word- or symbol-constituent characters according to
the buffer's syntax table."

  (interactive (list (read-buffer "Buffer to learn from: "
				  (buffer-name (current-buffer)) t)
		     (read-dict
		      "Dictionary to update (defaults to all in use): "
		      t)
		     current-prefix-arg))
  ;; sort out and sanity check arguments
  (and (called-interactively-p 'any) (eq dict t) (setq dict nil))
  (and (symbolp dict) (setq dict (symbol-value dict)))
  (and all (null dict)
       (error "Argument ALL supplied but no dictionary specified"))


  (let (restore-mode currdict word percent)
    (save-excursion
      ;; switch on predictive mode in the buffer if necessary
      (when buffer (set-buffer buffer))
      (unless all
	(if predictive-mode (setq restore-mode t) (predictive-mode 1)))

      ;; step through each word in buffer...
      (goto-char (point-min))
      (setq percent 0)
      (if dict
	  (message "Learning words for dictionary %s...(0%%)"
		   (dictree-name dict))
	(message "Learning words...(0%%)"))
      (while (re-search-forward "\\b\\(\\sw\\|\\s_\\)+\\b" nil t)
	(setq word (match-string-no-properties 0))
	(and predictive-ignore-initial-caps
	     (predictive-capitalized-p word)
	     (setq word (downcase word)))
	(cond

	 ;; if ALL was specified, learn current word
	 (all
	  (when (or (dictree-member-p dict word)
		    (and predictive-auto-add-to-dict
			 (or (not predictive-add-to-dict-ask)
			     (y-or-n-p
			      (format "Add word \"%s\" to dictionary? " word))
			     )))
	    (predictive-add-to-dict dict word)))

	 ;; if ALL was not specified and a dictionary has been specified, only
	 ;; increment the current word's weight if dictionary is active there
	 (dict
	  (setq currdict (predictive-current-dict))
	  (and (or (and (listp currdict) (memq dict currdict))
		   (eq dict currdict))
	       (or (dictree-member-p dict word)
		   (and predictive-auto-add-to-dict
			(or (not predictive-add-to-dict-ask)
			    (y-or-n-p
			     (format "Add word \"%s\" to dictionary? "
				     word))
			    )))
	       (predictive-add-to-dict dict word)))


	 ;; if ALL is not specified and no dictionary was specified, increment
	 ;; its weight in first dictionary active there that contains the word
	 (t
	  (setq currdict (predictive-current-dict))
	  (when currdict
	    (when (dictree-p currdict) (setq currdict (list currdict)))
	    (unless (catch 'learned
		      (dotimes (i (length currdict))
			(when (dictree-member-p (nth i currdict) word)
			  (predictive-add-to-dict (nth i currdict) word)
			  (throw 'learned t))))

	      ;; if word wasn't in any dictionary but auto-add is enabled, add
	      ;; the word to the appropriate dictionary
	      (when (and predictive-auto-add-to-dict
			 (or (not predictive-add-to-dict-ask)
			     (y-or-n-p
			      (format "Add word \"%s\" to dictionary? "
				      word)))
			 (or (null predictive-auto-add-min-chars)
			     (>= (length word)
				 predictive-auto-add-min-chars)))
		(cond

		 ;; if adding to the current dictionary, or first dictionary
		 ;; in the list if former is a list of dictionaries, then do
		 ;; so if the word passes the filter (if any)
		 ((eq predictive-auto-add-to-dict t)
		  (when (or (null predictive-auto-add-filter)
			    (funcall predictive-auto-add-filter
				     word (car currdict)))
		    (predictive-add-to-dict (car currdict) word)))

		 ;; if adding to the buffer-local dictionary, do so
		 ((eq predictive-auto-add-to-dict 'buffer)
		  ;; if buffer-local dictionaries are not enabled, display an
		  ;; error message
		  (if (null predictive-use-buffer-local-dict)
		      (message "The setting of `predictive-auto-add-to-dict'\
 specifies adding to the buffer-local dictionary, but buffer-local\
 dictionaries are not enabled by `predictive-use-buffer-local-dict'")
		    ;; if word passes the filter (if any), add it to the
		    ;; buffer-local dictionary
		    (when (or (null predictive-auto-add-filter)
			      (funcall
			       predictive-auto-add-filter
			       word
			       (symbol-value
				(predictive-buffer-local-dict-name))))
		      (predictive-add-to-dict
		       (symbol-value (predictive-buffer-local-dict-name))
		       word))))

		 ;; anything else specifies an explicit dictionary to add to
		 (t
		  (setq currdict (symbol-value predictive-auto-add-to-dict))
		  ;; check `predictive-auto-add-to-dict' is a dictionary
		  (if (dictree-p dict)
		      (when (or (null predictive-auto-add-filter)
				(funcall predictive-auto-add-filter
					 word dict))
			(predictive-add-to-dict dict word))
		    ;; display error message if not a dictionary
		    (beep)
		    (message
		     "Wrong type in `predictive-auto-add-to-dict': dictp")))
		 ))))))


	(when (> (- (/ (float (point)) (point-max)) percent) 0.0001)
	  (setq percent (/ (float (point)) (point-max)))
	  (if dict
	      (message "Learning words for dictionary %s...(%s%%)"
		       (dictree-name dict)
		       (progn
			 (string-match ".*\\..?.?"
				       (prin1-to-string (* 100 percent)))
			 (match-string 0 (prin1-to-string (* 100 percent)))))
	    (message "Learning words...(%s%%)"
		     (progn
		       (string-match ".*\\..?.?"
				     (prin1-to-string (* 100 percent)))
		       (match-string 0 (prin1-to-string (* 100 percent)))))))
	)  ; end while loop

      (unless (or all restore-mode) (predictive-mode -1))
      (if dict
	  (message "Learning words for dictionary %s...done"
		   (dictree-name dict))
	(message "Learning words...done"))
      )))



(defun predictive-fast-learn-or-add-from-file (file &optional dict all)
  "Learn word weights from FILE.

The word weight of each word in dictionary DICT is incremented by
the number of occurences of that word in the file. DICT can
either be a dictionary, or a list of dictionaries. If DICT is not
supplied, it defaults to all dictionaries used by FILE. However,
DICT must be supplied if ALL is specified, see below.

By default, only occurences of a word that occur in a region
where the dictionary is active are taken into account. If
optional argument ALL is non-nil, all occurences are taken into
account. In this case, a dictionary must be specified.

Note that this function takes the setting of
`predictive-auto-add-to-dict' and related options into
account. If an explicit dictionary is supplied, new words will be
added to that dictionary if `predictive-auto-add-to-dict' has any
non-nil value. If DICT is not supplied, the
`predictive-auto-add-to-dict' setting has the usual effect
\(which see\). If `predictive-add-to-dict-ask' is non-nil, you
will be prompted to confirm each and every word before it is
added \(so you may well wish to temporarily set
`predictive-add-to-dict-ask to nil before using this
function\). The `predictive-auto-add-min-chars' and
`predictive-auto-add-filter' variables also have their usual
effect.

Interactively, FILE and DICT are read from the mini-buffer, and
ALL is specified by the presence of a prefix argument.

This function is faster then `predictive-learn-from-file' for
large dictionaries, but will miss any words not consisting
entirely of word- or symbol-constituent characters."
  (interactive (list (read-file-name "File to learn from: " nil nil t)
		     (read-dict
		      "Dictionary to update (defaults to all in use): "
		      t)
		     current-prefix-arg))
  ;; sort out arguments
  (and (called-interactively-p 'any) (eq dict t) (setq dict nil))
  (and (symbolp dict) (setq dict (symbol-value dict)))
  (save-excursion
    ;; open file in a buffer
    (let (visiting buff)
      (if (setq buff (get-file-buffer file))
	  (setq visiting t)
	(find-file file))
      ;; learn from the buffer
      (predictive-fast-learn-or-add-from-buffer buff dict all)
      (unless visiting (kill-buffer buff)))))



(defun predictive-display-help (&optional dummy1 word &rest ignored)
  "Display any help for WORD from the current dictionary.

If WORD is not supplied, display any help for word at point if
the last command was a point motion command (this is so that the
function can be used in `post-command-hook').

Remaining arguments are ignored (they are there to allow
`predictive-display-help' to be used in the
`completion-accept-functions' hook). "
  (interactive)

  (and (not word)
       (or (eq last-command 'previous-line)
	   (eq last-command 'next-line)
	   (eq last-command 'backward-char)
	   (eq last-command 'forward-char)
	   (eq last-command 'left-char)
	   (eq last-command 'right-char))
       (setq word (thing-at-point
		   (completion-ui-source-word-thing 'predictive)))
       (set-text-properties 0 (length word) nil word))

  (when word
    (let ((dict (predictive-current-dict))
	  help)
      (when (catch 'found-help
	      (dolist (dic dict)
		(when (setq help (dictree-get-property dic word :help))
		  (throw 'found-help t))))
	(message help)))))




;;; ================================================================
;;;           Internal functions to do with completion

(defun predictive-complete (prefix &optional maxnum)
  "Try to complete string PREFIX, usually the string before the point,
returning at most MAXNUM completion candidates, ordered by
their weighting.

If MAXNUM is null, all possible completion candidates are
returned in alphabetical order, rather than by weight.

If `predictive-ignore-initial-caps' is enabled and first
character of string is capitalized, also search for completions
for uncapitalized version."

  (let ((dict (predictive-current-dict))
	pfx filter completions)

    ;; construct the completion filter
    (let ((completion-filter predictive-completion-filter))
      (setq filter (auto-overlay-local-binding 'completion-filter)))
    (when filter
      (unless (functionp filter)
	(error "Wrong type in completion-filter: functionp %s"
	       (prin1-to-string filter)))
      (setq filter (funcall filter prefix)))

    ;; if there is a current dictionary...
    (when dict
      ;; expand prefix
      (setq pfx (predictive-expand-prefix prefix))
      ;; if expanded prefix is a regexp, do a regexp search, using RESULTFUN
      ;; argument of `dictree-regexp-search' to convert dump word weights and
      ;; convert regexp group data into prefix length
      (if (eq (car pfx) 'regexp)
	  (setq completions
	  	(dictree-regexp-search
	  	 dict (cdr pfx) (if maxnum t nil) maxnum nil nil filter
	  	 (unless predictive-auto-correction-no-completion
	  	   (lambda (key data)
	  	     (cons (car key)
	  		   (- (cdr (cadr key)) (car (cadr key))))))))

	;; otherwise, complete the prefix
	(setq completions
	      (dictree-complete dict (cdr pfx) (if maxnum t nil) maxnum
				nil nil filter (lambda (key data) key))))
      ;; sort out capitalization of completions
      (and predictive-ignore-initial-caps
	   (predictive-capitalized-p prefix)
	   (setq completions
		 (mapcar (if (eq (car pfx) 'regexp)
			     (lambda (cmpl)
			       (cons
				(concat
				 (char-to-string (upcase (aref (car cmpl) 0)))
				 (substring (car cmpl) 1))
				(cdr cmpl)))
			   (lambda (cmpl)
			     (concat (char-to-string (upcase (aref cmpl 0)))
				     (substring cmpl 1))))
			 completions)))
      ;; return the completions
      completions)))



(defun predictive-expand-prefix (prefix)
  ;; Return expanded list of prefixes to complete, based on settings of
  ;; `predictive-ignore-initial-caps' and `predictive-prefix-expansions'

  ;; if there are no prefix expansions...
  (if (and (null predictive-prefix-expansions)
	   (null predictive-equivalent-characters)
	   (not predictive-auto-correction-no-completion))
      ;; if ignoring initial caps, expand a capitalized prefix into a list of
      ;; lower-case and capitalized prefixes
      (if (and predictive-ignore-initial-caps
	       (predictive-capitalized-p prefix))
	  (cons 'complete
		(list prefix
		      (concat (downcase (char-to-string (aref prefix 0)))
			      (substring prefix 1))))
	(cons 'complete prefix))

    ;; if there are prefix expansions, or
    ;; `predictive-auto-correction-no-completion' is set...
    (let ((expanded-prefix prefix)
	  expansion-list expanded-flag)
      ;; quote any special characters in prefix
      (dolist (expansion '(("\\." . "\\\\.") ("\\*" . "\\\\*")
			   ("\\+" . "\\\\+") ("\\?" . "\\\\?")
			   ("\\[" . "\\\\[") ("\\]" . "\\\\]")))
	(setq expanded-prefix
	      (replace-regexp-in-string
	       (car expansion) (cdr expansion) expanded-prefix)))
      (setq expanded-prefix
	    (replace-regexp-in-string
	     "\\(\\\\\\)\\([^][*?\\]\\|$\\)" "\\\\\\\\" prefix nil nil 1))

      ;; convert `predictive-equivalent-characters' into expansions
      (dolist (equiv (reverse predictive-equivalent-characters))
	(dotimes (i (length equiv))
	  (push (cons (char-to-string (aref equiv (- (length equiv) i 1)))
		      (concat "[" equiv "]"))
		expansion-list)))
      (setq expansion-list
	    (append predictive-prefix-expansions expansion-list))

      ;; apply `predictive-prefix-expansions' and
      ;; `predictive-equivalent-characters' expansions
      (let ((case-fold-search nil)
	    (chars (mapcar 'char-to-string (append expanded-prefix nil)))
	    (expanded (make-vector (length expanded-prefix) nil))
	    i)
	(dolist (expansion expansion-list)
	  (setq i (string-match (car expansion) expanded-prefix 0))
	  (while (and i (not (aref expanded i)))
	    (setf (nth i chars) (cdr expansion)
		  (aref expanded i) t
		  expanded-flag t)
	    (dotimes (i (- (match-end 0) (match-beginning 0) 1))
	      (setf (nth (+ (match-beginning 0) i 1) chars) nil)
	      (aset expanded (+ (match-beginning 0) i 1) t))
	    (setq i (string-match
		     (car expansion) expanded-prefix (match-end 0)))))
	(setq expanded-prefix (apply 'concat chars)))

      ;; if ignoring initial caps...
      (when (and expanded-flag
		 predictive-ignore-initial-caps
		 (predictive-capitalized-p prefix))
	(let ((c (aref expanded-prefix 0)))
	  (cond
	   ;; if initial character is a non-negated character alternative, add
	   ;; lower-case letters to the alternative
	   ((and (eq c ?\[) (not (eq (aref expanded-prefix 1) ?^)))
	    (let ((case-fold-search nil)
		  char-alt)
	      (string-match "^\\[.*?\\]" expanded-prefix)
	      (setq char-alt
		    (substring (match-string 0 expanded-prefix) 1 -1))
	      (dolist (c (append char-alt nil))
		(unless (eq c (downcase c))
		  (setq char-alt
			(concat char-alt (char-to-string (downcase c))))))
	      (setq expanded-prefix
		    (concat "[" char-alt "]"
			    (replace-match "" nil nil prefix)))))
	   ;; if initial character is a literal upper-case character, expand
	   ;; it into a character alternative including lower-case version
	   ((not (or (eq c ?*) (eq c ??) (eq c ?\[) (eq c ?\]) (eq c ?\\)))
	    (setq expanded-prefix
		  (concat "[" (char-to-string c)
			  (char-to-string (downcase c)) "]"
			  (substring prefix 1))))
	   )))

      ;; return expanded (or otherwise) prefix
      (if predictive-auto-correction-no-completion
	  (cons 'regexp expanded-prefix)
	(if expanded-flag
	    (cons 'regexp (concat "\\(" expanded-prefix "\\).*"))
	  (if (and predictive-ignore-initial-caps
		   (predictive-capitalized-p prefix))
	      (cons 'complete
		    (list prefix
			  (concat (vector (downcase (aref prefix 0)))
				  (substring prefix 1))))
	    (cons 'complete prefix)))))))



(defun predictive-auto-learn (ignored1 word &optional ignored2)
  "Function to deal with auto-learning WORD.
Usually called after a completion is accepted."

  (let ((dict (predictive-current-dict))
	found dic)

    ;; if there is a current dict...
    (unless (eq dict t)
      (when (dictree-p dict) (setq dict (list dict)))
      ;; if ignoring initial caps, look for uncapitalized word too
      (let (wordlist)
	(if (and predictive-ignore-initial-caps
		 (predictive-capitalized-p word))
	    (setq wordlist (list (downcase word) word))
	  (setq wordlist (list word)))
	;; look for word in all dictionaries in list
	(setq found
	      (catch 'found
		(dolist (d dict)
		  (setq dic d)
		  (dolist (wrd wordlist)
		    (when (dictree-member-p dic wrd) (throw 'found wrd)))))))


      ;; if the completion was not in the dictionary,
      ;; `predictive-auto-add-to-dict' is enabled, and either
      ;; `predictive-add-to-dict-ask' is disabled or user responded "y" when
      ;; asked, then add the new word to the appropriate dictionary
      (if (null found)
	  (when (and predictive-auto-add-to-dict
		     (or (not predictive-add-to-dict-ask)
			 (y-or-n-p
			  (format "Add word \"%s\" to dictionary? " word))))
	    ;; if adding to the currently active dictionary, then do just that,
	    ;; adding to the first in the list if there are a list of
	    ;; dictionaries
	    (cond
	     ((eq predictive-auto-add-to-dict t)
	      ;; if caching auto-added words, do so
	      (if predictive-use-auto-learn-cache
		  (push (cons word (car dict)) predictive-auto-add-cache)
		;; otherwise, check it pases the filter (if there is one),
		;; then add it to the dictionary
		(and (or (null predictive-auto-add-min-chars)
			 (>= (length word)
			     predictive-auto-add-min-chars))
		     (or (null predictive-auto-add-filter)
			 (funcall predictive-auto-add-filter
				  word (car dict)))
		     (predictive-add-to-dict (car dict) word))))

	     ;; if adding to the buffer-local dictionary...
	     ((eq predictive-auto-add-to-dict 'buffer)
	      ;; if buffer-local dictionaries are not enabled, display an
	      ;; error message
	      (if (null predictive-use-buffer-local-dict)
		  (message "`predictive-auto-add-to-dict' trying to add to\
 buffer-local dictionary, but `predictive-use-buffer-local-dict' is disabled")
		;; if caching auto-added words, do so
		(if predictive-use-auto-learn-cache
		    (push (cons word
				(symbol-value (predictive-buffer-local-dict-name)))
			  predictive-auto-add-cache)
		  ;; otherwise, check it passes the filter (if there is one),
		  ;; then add it to the dictionary
		  (and (or (null predictive-auto-add-min-chars)
			   (>= (length word)
			       predictive-auto-add-min-chars))
		       (or (null predictive-auto-add-filter)
			   (funcall
			    predictive-auto-add-filter
			    word
			    (symbol-value (predictive-buffer-local-dict-name))))
		       (predictive-add-to-dict
			(symbol-value (predictive-buffer-local-dict-name))
			word)))))

	     ;; anything else specifies an explicit dictionary to add to
	     (t
	      (setq dict (symbol-value predictive-auto-add-to-dict))
	      ;; check `predictive-auto-add-to-dict' is a dictionary
	      (if (dictree-p dict)
		  ;; if caching auto-added words, do so
		  (if predictive-use-auto-learn-cache
		      (push (cons word dict) predictive-auto-add-cache)
		    ;; otherwise, check is passes the filter (if there is
		    ;; one), then add it to the dictionary
		    (and (or (null predictive-auto-add-min-chars)
			     (>= (length word)
				 predictive-auto-add-min-chars))
			 (or (null predictive-auto-add-filter)
			     (funcall predictive-auto-add-filter
				      word dict))
			 (predictive-add-to-dict dict word)))
		;; display error message if not a dictionary
		(beep)
		(message
		 "Wrong type in `predictive-auto-add-to-dict': dictree-p")))
	     ))


	;; if the completion was in the dictionary and auto-learn is set...
	(when predictive-auto-learn
	  ;; if caching auto-learned words, do so
	  (if predictive-use-auto-learn-cache
	      (push (cons found dic) predictive-auto-learn-cache)
	    ;; if not caching, increment its weight in the dictionary it was
	    ;; found in
	    (predictive-add-to-dict dic found)))
	))))



(defun predictive-flush-auto-learn-caches (&optional idle)
  ;; Flush entries from the auto-learn and auto-add caches, adding them to the
  ;; appropriate dictionary. If optional argument IDLE is supplied, no
  ;; informative messages are displayed, and flushing will only continue
  ;; whilst emacs is idle

  (let ((learn-count (length predictive-auto-learn-cache))
	(add-count (length predictive-auto-add-cache))
	entry word dict count)

    (unless idle
      ;; set variables used in messages
      (setq count (+ learn-count add-count))
      (message "Flushing predictive mode auto-learn caches...(word 1 of %d)"
	       count))

    ;; flush words from auto-learn cache
    (dotimes (i (if idle (min 1 learn-count) learn-count))
      (setq entry (pop predictive-auto-learn-cache))
      (setq word (car entry))
      (setq dict (cdr entry))
      (unless idle
	(message
	 "Flushing predictive mode auto-learn caches...(word %d of %d)"
	 i count))
      ;; add word to whichever dictionary it is found in
      (when (dictree-p dict) (setq dict (list dict)))
      (catch 'learned
	(dolist (dic dict)
	  (when (dictree-member-p dic word)
	    (predictive-add-to-dict dic word 1)
	    (throw 'learned t)))))

    ;; flush words from auto-add cache
    (dotimes (i (if idle (min 1 add-count) add-count))
      (setq entry (pop predictive-auto-add-cache))
      (setq word (car entry))
      (setq dict (cdr entry))
      (unless idle
	(message
	 "Flushing predictive mode auto-learn caches...(word %d of %d)"
	 i count))
      (and (or (null predictive-auto-add-min-chars)
	       (>= (length word) predictive-auto-add-min-chars))
	   (or (null predictive-auto-add-filter)
	       (funcall predictive-auto-add-filter word dict))
	   (predictive-add-to-dict dict word))))

  (unless idle (message "Flushing predictive mode auto-learn caches...done")))




;;; ===================================================================
;;;    Internal functions and variables to do with dictionaries

(defun predictive-dict-rank-function (a b)
  ;; The dictionary rank function compares by weight (larger is "better"),
  ;; failing that by string length (smaller is "better"), and failing that it
  ;; compares the strings alphabetically
  (if (= (cdr a) (cdr b))
      (if (= (length (car a)) (length (car b)))
	  (string< (car a) (car b))
	(< (length (car a)) (length (car b))))
    (> (cdr a) (cdr b))))


(defun predictive-main-dict ()
  ;; Return concatenation of `predictive-buffer-dict' or
  ;; `predictive-main-dict' with `predictive-auxiliary-dict'
  (append
   (if predictive-buffer-dict
       (if (listp predictive-buffer-dict)
	   predictive-buffer-dict
	 (list predictive-buffer-dict))
     (if (listp predictive-main-dict)
	 predictive-main-dict
       (list predictive-main-dict)))
   predictive-auxiliary-dict))


(defun predictive-current-dict (&optional point)
  "Return the currently active dictionary(ies) at POINT
\(defaults to the point\). Always returns a list of dictionaries, even if
there's only one."
  (when (null point) (setq point (point)))

  ;; get the active dictionary and the overlay that sets it, if any
  ;; note: can't use `auto-overlay-local-binding' here because we want the
  ;; overlay as well as the binding
  (let ((overlay (auto-overlay-highest-priority-at-point
		  point '(identity dict)))
	dict generate)
    (if (null overlay)
	(setq dict (predictive-main-dict))
      (setq dict (overlay-get overlay 'dict))
      (cond
       ((functionp dict) (setq dict (funcall dict)))
       ((symbolp dict) (setq dict (symbol-value dict)))))

    ;; t indicates no active dictionary, so return nil
    (if (eq dict t) nil
      ;; otherwise bundle the dictionary inside a list for mapcar
      (unless (listp dict) (setq dict (list dict)))

      (mapcar
       (lambda (dic)
	 ;; if element is a function or symbol, evaluate it
	 (cond
	  ((functionp dic) (setq dic (funcall dic)))
  	  ((symbolp dic) (setq dic (symbol-value dic))))

	 (cond
	  ;; if element is a dictionary, return it
	  ((dictree-p dic) dic)

	  ;; if element is a plist with a :generate property...
	  ((and (listp dic) (setq generate (plist-get dic :generate)))
	   (unless (functionp generate)
	     (error "Wrong type in dictionary's :generate property:\
 functionp %s" (prin1-to-string generate)))
	   ;; if plist has a :dict property, and it's :refresh function
	   ;; returns nil, use existing :dict property
	   (if (and (plist-get dict :dict)
		    (or (not (functionp (plist-get dict :refresh)))
			(not (funcall (plist-get dict :refresh) overlay))))
	       (plist-get dict :dict)
	     ;; otherwise, generate and return the dictionary, saving it in
	     ;; the :dict propery
	     (overlay-put overlay 'dict
			  (plist-put dict :dict (funcall generate overlay)))
	     (plist-get dict :dict)))

	  ;; throw error on anything else
	  (t (error "Wrong type in element of dictionary list: functionp,\
 symbolp, dict-p, plist (with :generate) or t at %d %s"
		    point (prin1-to-string dic)))
	  ))

       dict)  ; map over dict
      )))



(defun predictive-load-buffer-local-dict (&optional main-dict)
  "Load/create the buffer-local dictionary.

If MAIN-DICT is a dictionary or a list of dictionaries, it
specifies the dictionaries on which the buffer-local
meta-dictionary will be based, instead of
`predictive-main-dict'."

  (let (filename buffer-dict meta-dict dict-list insfun rankfun combfun)
    ;; The rank function compares by weight (larger is "better"), failing that
    ;; by string length (smaller is "better"), and failing that it compares
    ;; the strings alphabetically.
    (setq rankfun
	  (lambda (a b)
	    (if (= (cdr a) (cdr b))
		(if (= (length (car a)) (length (car b)))
		    (string< (car a) (car b))
		  (< (length (car a)) (length (car b))))
	      (> (cdr a) (cdr b)))))
    ;; construct list of dictionaries which meta-dict will be based on
    (setq dict-list (if main-dict
			(if (listp main-dict)
			    main-dict
			  (list main-dict))
		      (if (listp predictive-main-dict)
			  predictive-main-dict
			(list predictive-main-dict))))


    ;; ----- buffer-local dictionary -----
    (when (buffer-file-name)
      (setq filename
	    (concat (file-name-directory (buffer-file-name))
		    predictive-auxiliary-file-location
		    (symbol-name (predictive-buffer-local-dict-name))
		    ".elc"))
      ;; create directory if necessary
      (predictive-create-auxiliary-file-location))
    ;; if the buffer-local dictionary exists, load it, otherwise create it
    (if (and filename (file-exists-p filename))
	(progn
	  (load filename)
	  (setf (dictree-filename (symbol-value
				   (predictive-buffer-local-dict-name)))
		  filename
		buffer-dict
		  (symbol-value (predictive-buffer-local-dict-name))))
      ;; The insertion function inserts a weight multiplied by the multiplier
      ;; if none already exists, otherwise it adds the new weight times the
      ;; multiplier to the existing one, or if supplied weight is nil,
      ;; incremenets existing weight by the multiplier.
      (setq insfun
	    (lambda (weight data)
	      (+ data (* weight predictive-buffer-local-learn-multiplier))))
      ;; create the buffer-local dictionary
      (setq buffer-dict
	    (dictree-create (predictive-buffer-local-dict-name)
			    filename (when filename t) nil
			    '< insfun rankfun nil
			    nil nil nil predictive-completion-speed
			    nil predictive-completion-speed)))


    ;; ----- meta-dictionary -----
    (when (buffer-file-name)
      (setq filename
	    (concat (file-name-directory (buffer-file-name))
		    predictive-auxiliary-file-location
		    (symbol-name (predictive-buffer-local-meta-dict-name))
		    ".elc")))
    ;; if the buffer-local dictionary doesn't exist yet, or needs updating,
    ;; make sure it's unloaded
    (if (or (null filename) (not (file-exists-p filename)) main-dict)
	(when (boundp (predictive-buffer-local-meta-dict-name))
	  (unintern (predictive-buffer-local-meta-dict-name)))
      ;; if the buffer meta-dictionary exists, and we're basing it on
      ;; `predictive-main-dict', load it
      (load filename)
      (setf (dictree-filename
	     (symbol-value (predictive-buffer-local-meta-dict-name)))
	    filename)
      ;; if the meta-dictionary is not based on the current main dictionary,
      ;; prompt user to update it
      (and (not (memq (symbol-value predictive-main-dict)
		      (dictree--meta-dict-dictlist
		       (symbol-value
			(predictive-buffer-local-meta-dict-name)))))
	   (y-or-n-p "Existing buffer-local dictionary is not based on the\
 current main dictionary. Update it? ")
	   (unintern (predictive-buffer-local-meta-dict-name))))

    ;; if the buffer meta-dictionary doesn't exist or is being updated...
    (unless (boundp (predictive-buffer-local-meta-dict-name))
      ;; the combine function adds the weights from the two constituent
      ;; dictionaries
      (setq combfun
	    (lambda (a b)
	      (cond ((null b) a)
		    ((null a) b)
		    (t (cons (+ (car a) (car b)) (cdr b))))))
      ;; create the buffer-local meta-dictionary
      (setq meta-dict
	    (dictree-create-meta-dict
	     (append (list buffer-dict)
		     (mapcar (lambda (dic)
			       (if (dictree-p dic) dic (symbol-value dic)))
			     dict-list))
	     (predictive-buffer-local-meta-dict-name)
	     filename (when filename t) nil combfun nil nil
	     nil nil predictive-completion-speed)))

    ;; set buffer's dictionary to the meta-dictionary
    (setq predictive-buffer-dict (predictive-buffer-local-meta-dict-name))
    ;; add meta-dictionary to the list of dictionaries used by buffer (can't
    ;; use `add-to-list' because we want comparison with `eq' not `equal')
    (unless (memq meta-dict predictive-used-dict-list)
      (setq predictive-used-dict-list
	    (cons meta-dict predictive-used-dict-list)))))



(defun predictive-unload-buffer-local-dict ()
  "Unload the buffer-local dictionary."
  (let ((buffer-dict (predictive-buffer-local-dict-name))
	(meta-dict (predictive-buffer-local-meta-dict-name)))
    (when (boundp buffer-dict)
      (if (dictree-p (symbol-value (predictive-buffer-local-dict-name)))
	  (dictree-unload (symbol-value (predictive-buffer-local-dict-name)))
	(unintern buffer-dict)))
    (when (boundp meta-dict)
      (if (dictree-p (symbol-value (predictive-buffer-local-meta-dict-name)))
	  (dictree-unload
	   (symbol-value (predictive-buffer-local-meta-dict-name)))
	(unintern meta-dict)))))



(defun predictive-save-used-dicts (&optional no-fail-query)
  "Save all dictionaries used by the current buffer.
NO-FAIL-QUERY is passed on to `dictree-save-modified'."
  (dictree-save-modified predictive-used-dict-list
			 nil predictive-dict-compilation
			 nil no-fail-query))




;;; ==================================================================
;;;       Functions and variables to do with which-dict mode

(define-minor-mode predictive-which-dict-mode
    "Toggle predictive mode's which dictionary mode.
With no argument, this command toggles the mode.
A positive prefix argument turns the mode on.
A negative prefix argument turns it off.")



(defun predictive-which-dict-name ()
  ;; Returns the current dictionary name. Used by the
  ;; `predictive-which-dict-mode' mode-line format to display the current
  ;; dictionary the mode line.

  ;; only run if predictive mode is enabled and point has moved since last run
  (let ((dict (predictive-current-dict)) name list)
    (when (dictree-p dict) (setq dict (list dict)))

    ;; get current dictionary name(s)
    (if (null dict)
	(setq name "" list nil)

      ;; if dict is the buffer-local meta-dictioary, display name of main
      ;; dictionary it's based on instead
      (if (and (string= (dictree-name (car dict))
			(predictive-buffer-local-dict-name))
	       (dictree-meta-dict-p (car dict)))
	  (setq name (dictree-name (nth 1 (dictree-meta-dict-dictlist
					   (car dict)))))
	(setq name (dictree-name (car dict))))
;;;   ;; truncate to 15 characters
;;;   (when (> (length name) 15) (setq name (substring name 0 15)))
      ;; filter list to remove "-dict-" and "-predictive-" prefixes
      (when (string-match "-*dict-*\\|-*predictive-*" name)
	(setq name (replace-match "" nil nil name)))

      ;; if current dictionary is a list, add "..." to end of name, and
      ;; construct list of all dictionary names for help-echo text
      (if (= (length dict) 1)
	  (setq list nil)
	(setq name (concat name "..."))
	(setq list (mapconcat 'dictree-name dict "\n"))
	;; filter list to remove "dict-" and "predictive-" prefixes
	(while (string-match "^dict-*\\|^predictive-*" list)
	  (setq list (replace-match "" nil nil list)))))

    ;; return the dictionary name
    (cons name list)))




;;; ===============================================================
;;;                       Compatibility Stuff

(unless (fboundp 'replace-regexp-in-string)
  (require 'predictive-compat)
  (defalias 'replace-regexp-in-string
            'predictive-compat-replace-regexp-in-string))




;;; ===============================================================
;;;                   Register Completion-UI source

(completion-ui-register-source
 predictive-complete
 :name predictive
 :completion-args 2
 :accept-functions (lambda (prefix completion &optional arg)
		    (run-hook-with-args 'predictive-accept-functions
					prefix completion arg))
 :reject-functions (lambda (prefix completion &optional arg)
		    (run-hook-with-args 'predictive-reject-functions
					prefix completion arg))
 :menu predictive-menu-function
 :browser predictive-browser-function
 :word-thing predictive-word-thing)



(provide 'predictive)
(require 'predictive-latex)
(require 'predictive-texinfo)
(require 'predictive-html)


;;; predictive.el ends here
