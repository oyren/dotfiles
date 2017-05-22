;; -*- mode: emacs-lisp -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

(defun dotspacemacs/layers ()
  "Configuration Layers declaration.
You should not put any user code in this function besides modifying the variable
values."
  (setq-default
   ;; Base distribution to use. This is a layer contained in the directory
   ;; `+distribution'. For now available distributions are `spacemacs-base'
   ;; or `spacemacs'. (default 'spacemacs)
   dotspacemacs-distribution 'spacemacs
   ;; Lazy installation of layers (i.e. layers are installed only when a file
   ;; with a supported type is opened). Possible values are `all', `unused'
   ;; and `nil'. `unused' will lazy install only unused layers (i.e. layers
   ;; not listed in variable `dotspacemacs-configuration-layers'), `all' will
   ;; lazy install any layer that support lazy installation even the layers
   ;; listed in `dotspacemacs-configuration-layers'. `nil' disable the lazy
   ;; installation feature and you have to explicitly list a layer in the
   ;; variable `dotspacemacs-configuration-layers' to install it.
   ;; (default 'unused)
   dotspacemacs-enable-lazy-installation 'unused
   ;; If non-nil then Spacemacs will ask for confirmation before installing
   ;; a layer lazily. (default t)
   dotspacemacs-ask-for-lazy-installation t
   ;; If non-nil layers with lazy install support are lazy installed.
   ;; List of additional paths where to look for configuration layers.
   ;; Paths must have a trailing slash (i.e. `~/.mycontribs/')
   dotspacemacs-configuration-layer-path '()
   ;; List of configuration layers to load.
   dotspacemacs-configuration-layers
   '(
     ;; ----------------------------------------------------------------
     ;; Example of useful layers you may want to use right away.
     ;; Uncomment some layer names and press <SPC f e R> (Vim style) or
     ;; <M-m f e R> (Emacs style) to install them.
     ;; ----------------------------------------------------------------
     (auto-completion
      :variables
      auto-completion-enable-snippets-in-popup t
      auto-completion-return-key-behavior nil
      auto-completion-tab-key-behavior 'cycle
      auto-completion-private-snippets-directory "~/.spacemacs.d/snippets/"
      auto-completion-enable-help-tooltip 'manual
      :disabled-for erc)
     c-c++
     (elfeed :variables rmh-elfeed-org-files (list "~/.dotfiles/spacemacs/elfeed.org")) ;; SPC a f
     git ;; SPC g s
     html
     (ibuffer :variables ibuffer-group-buffers-by nil) ;; SPC b B
     latex
     nixos
     (org :variables
          org-enable-github-support t
          org-enable-reveal-js-support t
     )
     ranger ;; SPC a r
     (shell :variables shell-default-shell 'eshell) ;; SPC '
     (syntax-checking :variables syntax-checking-enable-by-default nil)
     ;; SPC t s to activate ;; SPC e to use


     ;;ivy
     ;; auto-completion
     ;; better-defaults
     emacs-lisp
     ;; git
     ;; markdown
     ;; org
     ;; (shell :variables
     ;;        shell-default-height 30
     ;;        shell-default-position 'bottom)
     ;; spell-checking
     ;; syntax-checking
     ;; version-control
     )
   ;; List of additional packages that will be installed without being
   ;; wrapped in a layer. If you need some configuration for these
   ;; packages, then consider creating a layer. You can also put the
   ;; configuration in `dotspacemacs/user-config'.
   dotspacemacs-additional-packages '()
   ;; A list of packages that cannot be updated.
   dotspacemacs-frozen-packages '()
   ;; A list of packages that will not be installed and loaded.
   dotspacemacs-excluded-packages '()
   ;; Defines the behaviour of Spacemacs when installing packages.
   ;; Possible values are `used-only', `used-but-keep-unused' and `all'.
   ;; `used-only' installs only explicitly used packages and uninstall any
   ;; unused packages as well as their unused dependencies.
   ;; `used-but-keep-unused' installs only the used packages but won't uninstall
   ;; them if they become unused. `all' installs *all* packages supported by
   ;; Spacemacs and never uninstall them. (default is `used-only')
   dotspacemacs-install-packages 'used-only))

(defun dotspacemacs/init ()
  "Initialization function.
This function is called at the very startup of Spacemacs initialization
before layers configuration.
You should not put any user code in there besides modifying the variable
values."
  ;; This setq-default sexp is an exhaustive list of all the supported
  ;; spacemacs settings.
  (setq-default
   ;; If non nil ELPA repositories are contacted via HTTPS whenever it's
   ;; possible. Set it to nil if you have no way to use HTTPS in your
   ;; environment, otherwise it is strongly recommended to let it set to t.
   ;; This variable has no effect if Emacs is launched with the parameter
   ;; `--insecure' which forces the value of this variable to nil.
   ;; (default t)
   dotspacemacs-elpa-https t
   ;; Maximum allowed time in seconds to contact an ELPA repository.
   dotspacemacs-elpa-timeout 5
   ;; If non nil then spacemacs will check for updates at startup
   ;; when the current branch is not `develop'. Note that checking for
   ;; new versions works via git commands, thus it calls GitHub services
   ;; whenever you start Emacs. (default nil)
   dotspacemacs-check-for-update nil
   ;; If non-nil, a form that evaluates to a package directory. For example, to
   ;; use different package directories for different Emacs versions, set this
   ;; to `emacs-version'.
   dotspacemacs-elpa-subdirectory nil
   ;; One of `vim', `emacs' or `hybrid'.
   ;; `hybrid' is like `vim' except that `insert state' is replaced by the
   ;; `hybrid state' with `emacs' key bindings. The value can also be a list
   ;; with `:variables' keyword (similar to layers). Check the editing styles
   ;; section of the documentation for details on available variables.
   ;; (default 'vim)
   dotspacemacs-editing-style 'vim
   ;; If non nil output loading progress in `*Messages*' buffer. (default nil)
   dotspacemacs-verbose-loading nil
   ;; Specify the startup banner. Default value is `official', it displays
   ;; the official spacemacs logo. An integer value is the index of text
   ;; banner, `random' chooses a random text banner in `core/banners'
   ;; directory. A string value must be a path to an image format supported
   ;; by your Emacs build.
   ;; If the value is nil then no banner is displayed. (default 'official)
   dotspacemacs-startup-banner nil
   ;; List of items to show in startup buffer or an association list of
   ;; the form `(list-type . list-size)`. If nil then it is disabled.
   ;; Possible values for list-type are:
   ;; `recents' `bookmarks' `projects' `agenda' `todos'."
   ;; List sizes may be nil, in which case
   ;; `spacemacs-buffer-startup-lists-length' takes effect.
   dotspacemacs-startup-lists '((recents . 5)
                                (projects . 7))
   ;; True if the home buffer should respond to resize events.
   dotspacemacs-startup-buffer-responsive t
   ;; Default major mode of the scratch buffer (default `text-mode')
   dotspacemacs-scratch-mode 'text-mode
   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press <SPC> T n to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light)
   dotspacemacs-themes '(monokai alect-light)
   ;; If non nil the cursor color matches the state color in GUI Emacs.
   dotspacemacs-colorize-cursor-according-to-state t
   ;; Default font, or prioritized list of fonts. `powerline-scale' allows to
   ;; quickly tweak the mode-line size to make separators look not too crappy.
   dotspacemacs-default-font '("Source Code Pro"
                               :size 13
                               :weight normal
                               :width normal
                               :powerline-scale 1.1)
   ;; The leader key
   dotspacemacs-leader-key "SPC"
   ;; The key used for Emacs commands (M-x) (after pressing on the leader key).
   ;; (default "SPC")
   dotspacemacs-emacs-command-key "SPC"
   ;; The key used for Vim Ex commands (default ":")
   dotspacemacs-ex-command-key ":"
   ;; The leader key accessible in `emacs state' and `insert state'
   ;; (default "M-m")
   dotspacemacs-emacs-leader-key "M-m"
   ;; Major mode leader key is a shortcut key which is the equivalent of
   ;; pressing `<leader> m`. Set it to `nil` to disable it. (default ",")
   dotspacemacs-major-mode-leader-key ","
   ;; Major mode leader key accessible in `emacs state' and `insert state'.
   ;; (default "C-M-m")
   dotspacemacs-major-mode-emacs-leader-key "C-M-m"
   ;; These variables control whether separate commands are bound in the GUI to
   ;; the key pairs C-i, TAB and C-m, RET.
   ;; Setting it to a non-nil value, allows for separate commands under <C-i>
   ;; and TAB or <C-m> and RET.
   ;; In the terminal, these pairs are generally indistinguishable, so this only
   ;; works in the GUI. (default nil)
   dotspacemacs-distinguish-gui-tab nil
   ;; If non nil `Y' is remapped to `y$' in Evil states. (default nil)
   dotspacemacs-remap-Y-to-y$ nil
   ;; If non-nil, the shift mappings `<' and `>' retain visual state if used
   ;; there. (default t)
   dotspacemacs-retain-visual-state-on-shift t
   ;; If non-nil, J and K move lines up and down when in visual mode.
   ;; (default nil)
   dotspacemacs-visual-line-move-text nil
   ;; If non nil, inverse the meaning of `g' in `:substitute' Evil ex-command.
   ;; (default nil)
   dotspacemacs-ex-substitute-global nil
   ;; Name of the default layout (default "Default")
   dotspacemacs-default-layout-name "Default"
   ;; If non nil the default layout name is displayed in the mode-line.
   ;; (default nil)
   dotspacemacs-display-default-layout nil
   ;; If non nil then the last auto saved layouts are resume automatically upon
   ;; start. (default nil)
   dotspacemacs-auto-resume-layouts nil
   ;; Size (in MB) above which spacemacs will prompt to open the large file
   ;; literally to avoid performance issues. Opening a file literally means that
   ;; no major mode or minor modes are active. (default is 1)
   dotspacemacs-large-file-size 1
   ;; Location where to auto-save files. Possible values are `original' to
   ;; auto-save the file in-place, `cache' to auto-save the file to another
   ;; file stored in the cache directory and `nil' to disable auto-saving.
   ;; (default 'cache)
   dotspacemacs-auto-save-file-location 'cache
   ;; Maximum number of rollback slots to keep in the cache. (default 5)
   dotspacemacs-max-rollback-slots 5
   ;; If non nil, `helm' will try to minimize the space it uses. (default nil)
   dotspacemacs-helm-resize nil
   ;; if non nil, the helm header is hidden when there is only one source.
   ;; (default nil)
   dotspacemacs-helm-no-header nil
   ;; define the position to display `helm', options are `bottom', `top',
   ;; `left', or `right'. (default 'bottom)
   dotspacemacs-helm-position 'bottom
   ;; Controls fuzzy matching in helm. If set to `always', force fuzzy matching
   ;; in all non-asynchronous sources. If set to `source', preserve individual
   ;; source settings. Else, disable fuzzy matching in all sources.
   ;; (default 'always)
   dotspacemacs-helm-use-fuzzy 'always
   ;; If non nil the paste micro-state is enabled. When enabled pressing `p`
   ;; several times cycle between the kill ring content. (default nil)
   dotspacemacs-enable-paste-transient-state nil
   ;; Which-key delay in seconds. The which-key buffer is the popup listing
   ;; the commands bound to the current keystroke sequence. (default 0.4)
   dotspacemacs-which-key-delay 0.4
   ;; Which-key frame position. Possible values are `right', `bottom' and
   ;; `right-then-bottom'. right-then-bottom tries to display the frame to the
   ;; right; if there is insufficient space it displays it at the bottom.
   ;; (default 'bottom)
   dotspacemacs-which-key-position 'bottom
   ;; If non nil a progress bar is displayed when spacemacs is loading. This
   ;; may increase the boot time on some systems and emacs builds, set it to
   ;; nil to boost the loading time. (default t)
   dotspacemacs-loading-progress-bar t
   ;; If non nil the frame is fullscreen when Emacs starts up. (default nil)
   ;; (Emacs 24.4+ only)
   dotspacemacs-fullscreen-at-startup nil
   ;; If non nil `spacemacs/toggle-fullscreen' will not use native fullscreen.
   ;; Use to disable fullscreen animations in OSX. (default nil)
   dotspacemacs-fullscreen-use-non-native nil
   ;; If non nil the frame is maximized when Emacs starts up.
   ;; Takes effect only if `dotspacemacs-fullscreen-at-startup' is nil.
   ;; (default nil) (Emacs 24.4+ only)
   dotspacemacs-maximized-at-startup nil
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's active or selected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-active-transparency 90
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's inactive or deselected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-inactive-transparency 90
   ;; If non nil show the titles of transient states. (default t)
   dotspacemacs-show-transient-state-title t
   ;; If non nil show the color guide hint for transient state keys. (default t)
   dotspacemacs-show-transient-state-color-guide t
   ;; If non nil unicode symbols are displayed in the mode line. (default t)
   dotspacemacs-mode-line-unicode-symbols nil
   ;; If non nil smooth scrolling (native-scrolling) is enabled. Smooth
   ;; scrolling overrides the default behavior of Emacs which recenters point
   ;; when it reaches the top or bottom of the screen. (default t)
   dotspacemacs-smooth-scrolling t
   ;; Control line numbers activation.
   ;; If set to `t' or `relative' line numbers are turned on in all `prog-mode' and
   ;; `text-mode' derivatives. If set to `relative', line numbers are relative.
   ;; This variable can also be set to a property list for finer control:
   ;; '(:relative nil
   ;;   :disabled-for-modes dired-mode
   ;;                       doc-view-mode
   ;;                       markdown-mode
   ;;                       org-mode
   ;;                       pdf-view-mode
   ;;                       text-mode
   ;;   :size-limit-kb 1000)
   ;; (default nil)
   dotspacemacs-line-numbers nil
   ;; Code folding method. Possible values are `evil' and `origami'.
   ;; (default 'evil)
   dotspacemacs-folding-method 'evil
   ;; If non-nil smartparens-strict-mode will be enabled in programming modes.
   ;; (default nil)
   dotspacemacs-smartparens-strict-mode nil
   ;; If non-nil pressing the closing parenthesis `)' key in insert mode passes
   ;; over any automatically added closing parenthesis, bracket, quote, etc…
   ;; This can be temporary disabled by pressing `C-q' before `)'. (default nil)
   dotspacemacs-smart-closing-parenthesis nil
   ;; Select a scope to highlight delimiters. Possible values are `any',
   ;; `current', `all' or `nil'. Default is `all' (highlight any scope and
   ;; emphasis the current one). (default 'all)
   dotspacemacs-highlight-delimiters 'all
   ;; If non nil, advise quit functions to keep server open when quitting.
   ;; (default nil)
   dotspacemacs-persistent-server nil
   ;; List of search tool executable names. Spacemacs uses the first installed
   ;; tool of the list. Supported tools are `ag', `pt', `ack' and `grep'.
   ;; (default '("ag" "pt" "ack" "grep"))
   dotspacemacs-search-tools '("ag" "pt" "ack" "grep")
   ;; The default package repository used if no explicit repository has been
   ;; specified with an installed package.
   ;; Not used for now. (default nil)
   dotspacemacs-default-package-repository nil
   ;; Delete whitespace while saving buffer. Possible values are `all'
   ;; to aggressively delete empty line and long sequences of whitespace,
   ;; `trailing' to delete only the whitespace at end of lines, `changed'to
   ;; delete only whitespace for changed lines or `nil' to disable cleanup.
   ;; (default nil)
   dotspacemacs-whitespace-cleanup nil
   ))

(defun dotspacemacs/user-init ()
  "Initialization function for user code.
It is called immediately after `dotspacemacs/init', before layer configuration
executes.
 This function is mostly useful for variables that need to be set
before packages are loaded. If you are unsure, you should try in setting them in
`dotspacemacs/user-config' first."
  ;; Avy
  avy-all-windows 'all-frames
  ;; Evil
  evil-shift-round nil
  )

(defun dotspacemacs/user-config ()
  "Configuration function for user code.
This function is called at the very end of Spacemacs initialization after
layers configuration.
This is the place where most of your configurations should be done. Unless it is
explicitly specified that a variable should be set before a package is loaded,
you should place your code here."
  ;; Beancount
  (add-to-list 'load-path "~/src/beancount")
  (require 'beancount)
  (add-to-list 'auto-mode-alist '("\\.beancount\\'" . beancount-mode))

  ;; Custom Key Bindings

  ;; Indent
  (setq-default indent-tabs-mode t)
  (setq-default tab-width 4)
  (setq-default c-basic-offset 2)
  (setq indent-line-function 'nil)
  (setq tab-always-indent 'nil)
  (setq spacemacs-indent-sensitive-modes (add-to-list 'spacemacs-indent-sensitive-modes 'nix-mode))

  ;; Latex
  (progn (setq TeX-view-program-selection '((output-pdf "Zathura"))))

  ;; Miscellaneous
  (add-hook 'text-mode-hook 'auto-fill-mode)

  (with-eval-after-load 'org

	(setq org-todo-keywords
		  (quote ((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
				  (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)" "PHONE" "MEETING"))))

	(setq org-todo-keyword-faces
		  (quote (("TODO" :foreground "red" :weight bold)
				  ("NEXT" :foreground "blue" :weight bold)
				  ("DONE" :foreground "forest green" :weight bold)
				  ("WAITING" :foreground "orange" :weight bold)
				  ("HOLD" :foreground "magenta" :weight bold)
				  ("CANCELLED" :foreground "forest green" :weight bold)
				  ("MEETING" :foreground "forest green" :weight bold)
				  ("PHONE" :foreground "forest green" :weight bold))))

	(setq org-todo-state-tags-triggers
		  (quote (("CANCELLED" ("CANCELLED" . t))
				  ("WAITING" ("WAITING" . t))
				  ("HOLD" ("WAITING") ("HOLD" . t))
				  (done ("WAITING") ("HOLD"))
				  ("TODO" ("WAITING") ("CANCELLED") ("HOLD"))
				  ("NEXT" ("WAITING") ("CANCELLED") ("HOLD"))
				  ("DONE" ("WAITING") ("CANCELLED") ("HOLD")))))

	(setq org-directory "~/org")
	(setq org-default-notes-file "~/org/refile.org")

	;; Capture templates for: TODO tasks, Notes, appointments, phone calls, meetings, and org-protocol
	(setq org-capture-templates
		  (quote (("t" "todo" entry (file "~/org/refile.org")
				   "* TODO %?\n%U\n%a\n" :clock-in t :clock-resume t)
				  ("r" "respond" entry (file "~/org/refile.org")
				   "* NEXT Respond to %:from on %:subject\nSCHEDULED: %t\n%U\n%a\n" :clock-in t :clock-resume t :immediate-finish t)
				  ("n" "note" entry (file "~/org/refile.org")
				   "* %? :NOTE:\n%U\n%a\n" :clock-in t :clock-resume t)
				  ("j" "Journal" entry (file+datetree "~/org/diary.org")
				   "* %?\n%U\n" :clock-in t :clock-resume t)
				  ("w" "org-protocol" entry (file "~/org/refile.org")
				   "* TODO Review %c\n%U\n" :immediate-finish t)
				  ("m" "Meeting" entry (file "~/org/refile.org")
				   "* MEETING with %? :MEETING:\n%U" :clock-in t :clock-resume t)
				  ("p" "Phone call" entry (file "~/org/refile.org")
				   "* PHONE %? :PHONE:\n%U" :clock-in t :clock-resume t)
				  ("h" "Habit" entry (file "~/org/refile.org")
				   "* NEXT %?\n%U\n%a\nSCHEDULED: %(format-time-string \"%<<%Y-%m-%d %a .+1d/3d>>\")\n:PROPERTIES:\n:STYLE: habit\n:REPEAT_TO_STATE: NEXT\n:END:\n"))))

	(setq org-use-fast-todo-selection t)
	(setq org-treat-S-cursor-todo-selection-as-state-change nil)
	(setq org-stuck-projects (quote ("" nil nil "")))

	(setq org-agenda-files (quote ("~/org/")))
	;; Do not dim blocked tasks
	(setq org-agenda-dim-blocked-tasks nil)

	;; Compact the block agenda view
	(setq org-agenda-compact-blocks t)
	(setq org-agenda-span 'day)
	;; Custom agenda command definitions
	(setq org-agenda-custom-commands
		  (quote (("N" "Notes" tags "NOTE"
				   ((org-agenda-overriding-header "Notes")
					(org-tags-match-list-sublevels t)))
				  ("h" "Habits" tags-todo "STYLE=\"habit\""
				   ((org-agenda-overriding-header "Habits")
					(org-agenda-sorting-strategy
					 '(todo-state-down effort-up category-keep))))
				  (" " "Agenda"
				   ((agenda "" nil)
					(tags "REFILE"
						  ((org-agenda-overriding-header "Tasks to Refile")
						   (org-tags-match-list-sublevels nil)))
					(tags-todo "-CANCELLED/!"
							   ((org-agenda-overriding-header "Stuck Projects")
								(org-agenda-skip-function 'bh/skip-non-stuck-projects)
								(org-agenda-sorting-strategy
								 '(category-keep))))
					(tags-todo "-HOLD-CANCELLED/!"
							   ((org-agenda-overriding-header "Projects")
								(org-agenda-skip-function 'bh/skip-non-projects)
								(org-tags-match-list-sublevels 'indented)
								(org-agenda-sorting-strategy
								 '(category-keep))))
					(tags-todo "-CANCELLED/!NEXT"
							   ((org-agenda-overriding-header (concat "Project Next Tasks"
																	  (if bh/hide-scheduled-and-waiting-next-tasks
																		  ""
																		" (including WAITING and SCHEDULED tasks)")))
								(org-agenda-skip-function 'bh/skip-projects-and-habits-and-single-tasks)
								(org-tags-match-list-sublevels t)
								(org-agenda-todo-ignore-scheduled bh/hide-scheduled-and-waiting-next-tasks)
								(org-agenda-todo-ignore-deadlines bh/hide-scheduled-and-waiting-next-tasks)
								(org-agenda-todo-ignore-with-date bh/hide-scheduled-and-waiting-next-tasks)
								(org-agenda-sorting-strategy
								 '(todo-state-down effort-up category-keep))))
					(tags-todo "-REFILE-CANCELLED-WAITING-HOLD/!"
							   ((org-agenda-overriding-header (concat "Project Subtasks"
																	  (if bh/hide-scheduled-and-waiting-next-tasks
																		  ""
																		" (including WAITING and SCHEDULED tasks)")))
								(org-agenda-skip-function 'bh/skip-non-project-tasks)
								(org-agenda-todo-ignore-scheduled bh/hide-scheduled-and-waiting-next-tasks)
								(org-agenda-todo-ignore-deadlines bh/hide-scheduled-and-waiting-next-tasks)
								(org-agenda-todo-ignore-with-date bh/hide-scheduled-and-waiting-next-tasks)
								(org-agenda-sorting-strategy
								 '(category-keep))))
					(tags-todo "-REFILE-CANCELLED-WAITING-HOLD/!"
							   ((org-agenda-overriding-header (concat "Standalone Tasks"
																	  (if bh/hide-scheduled-and-waiting-next-tasks
																		  ""
																		" (including WAITING and SCHEDULED tasks)")))
								(org-agenda-skip-function 'bh/skip-project-tasks)
								(org-agenda-todo-ignore-scheduled bh/hide-scheduled-and-waiting-next-tasks)
								(org-agenda-todo-ignore-deadlines bh/hide-scheduled-and-waiting-next-tasks)
								(org-agenda-todo-ignore-with-date bh/hide-scheduled-and-waiting-next-tasks)
								(org-agenda-sorting-strategy
								 '(todo-state-down))))
					(tags-todo "-CANCELLED+WAITING|HOLD/!"
							   ((org-agenda-overriding-header (concat "Waiting and Postponed Tasks"
																	  (if bh/hide-scheduled-and-waiting-next-tasks
																		  ""
																		" (including WAITING and SCHEDULED tasks)")))
								(org-agenda-skip-function 'bh/skip-non-tasks)
								(org-tags-match-list-sublevels nil)
								(org-agenda-todo-ignore-scheduled bh/hide-scheduled-and-waiting-next-tasks)
								(org-agenda-todo-ignore-deadlines bh/hide-scheduled-and-waiting-next-tasks)))
					(tags "-REFILE/"
						  ((org-agenda-overriding-header "Tasks to Archive")
						   (org-agenda-skip-function 'bh/skip-non-archivable-tasks)
						   (org-tags-match-list-sublevels nil))))
				   nil))))

	; Targets include this file and any file contributing to the agenda - up to 9 levels deep
	;(setq org-refile-targets (quote ((nil :maxlevel . 9)
	;								 (org-agenda-files :maxlevel . 9))))
	(setq org-refile-targets '((nil :maxlevel . 9)
							   (org-agenda-files :maxlevel . 9)))
	(setq org-outline-path-complete-in-steps nil)         ; Refile in a single go
	; Use full outline paths for refile targets - we file directly with IDO
	(setq org-refile-use-outline-path t)

	; Allow refile to create parent tasks with confirmation
	(setq org-refile-allow-creating-parent-nodes (quote confirm))

	; Use the current window for indirect buffer display
	(setq org-indirect-buffer-display 'current-window)

	;;;; Refile settings
	; Exclude DONE state tasks from refile targets
	(defun bh/verify-refile-target ()
	  "Exclude todo keywords with a done state from refile targets"
	  (not (member (nth 2 (org-heading-components)) org-done-keywords)))

	(setq org-refile-target-verify-function 'bh/verify-refile-target)

	;; Remove empty LOGBOOK drawers on clock out
	(defun bh/remove-empty-drawer-on-clock-out ()
	  (interactive)
	  (save-excursion
		(beginning-of-line 0)
		(org-remove-empty-drawer-at (point))))

	(add-hook 'org-clock-out-hook 'bh/remove-empty-drawer-on-clock-out 'append)
	(defun bh/org-auto-exclude-function (tag)
	  "Automatic task exclusion in the agenda with / RET"
	  (and (cond
			((string= tag "hold")
			 t)
			((string= tag "farm")
			 t))
		   (concat "-" tag)))

	(setq org-agenda-auto-exclude-function 'bh/org-auto-exclude-function)

	;;
	;; Resume clocking task when emacs is restarted
	(org-clock-persistence-insinuate)
	;;
	;; Show lot of clocking history so it's easy to pick items off the C-F11 list
	(setq org-clock-history-length 23)
	;; Resume clocking task on clock-in if the clock is open
	(setq org-clock-in-resume t)
	;; Change tasks to NEXT when clocking in
	(setq org-clock-in-switch-to-state 'bh/clock-in-to-next)
	;; Separate drawers for clocking and logs
	(setq org-drawers (quote ("PROPERTIES" "LOGBOOK")))
	;; Save clock data and state changes and notes in the LOGBOOK drawer
	(setq org-clock-into-drawer t)
	;; Sometimes I change tasks I'm clocking quickly - this removes clocked tasks with 0:00 duration
	(setq org-clock-out-remove-zero-time-clocks t)
	;; Clock out when moving task to a done state
	(setq org-clock-out-when-done t)
	;; Save the running clock and all clock history when exiting Emacs, load it on startup
	(setq org-clock-persist t)
	;; Do not prompt to resume an active clock
	(setq org-clock-persist-query-resume nil)
	;; Enable auto clock resolution for finding open clocks
	(setq org-clock-auto-clock-resolution (quote when-no-clock-is-running))
	;; Include current clocking task in clock reports
	(setq org-clock-report-include-clocking-task t)

	(setq bh/keep-clock-running nil)

	(defun bh/clock-in-to-next (kw)
	  "Switch a task from TODO to NEXT when clocking in.
Skips capture tasks, projects, and subprojects.
Switch projects and subprojects from NEXT back to TODO"
	  (when (not (and (boundp 'org-capture-mode) org-capture-mode))
		(cond
		 ((and (member (org-get-todo-state) (list "TODO"))
			   (bh/is-task-p))
		  "NEXT")
		 ((and (member (org-get-todo-state) (list "NEXT"))
			   (bh/is-project-p))
		  "TODO"))))

	(defun bh/find-project-task ()
	  "Move point to the parent (project) task if any"
	  (save-restriction
		(widen)
		(let ((parent-task (save-excursion (org-back-to-heading 'invisible-ok) (point))))
		  (while (org-up-heading-safe)
			(when (member (nth 2 (org-heading-components)) org-todo-keywords-1)
			  (setq parent-task (point))))
		  (goto-char parent-task)
		  parent-task)))

	(defun bh/punch-in (arg)
	  "Start continuous clocking and set the default task to the
selected task.  If no task is selected set the Organization task
as the default task."
	  (interactive "p")
	  (setq bh/keep-clock-running t)
	  (if (equal major-mode 'org-agenda-mode)
		  ;;
		  ;; We're in the agenda
		  ;;
		  (let* ((marker (org-get-at-bol 'org-hd-marker))
				 (tags (org-with-point-at marker (org-get-tags-at))))
			(if (and (eq arg 4) tags)
				(org-agenda-clock-in '(16))
			  (bh/clock-in-organization-task-as-default)))
		;;
		;; We are not in the agenda
		;;
		(save-restriction
		  (widen)
										; Find the tags on the current task
		  (if (and (equal major-mode 'org-mode) (not (org-before-first-heading-p)) (eq arg 4))
			  (org-clock-in '(16))
			(bh/clock-in-organization-task-as-default)))))

	(defun bh/punch-out ()
	  (interactive)
	  (setq bh/keep-clock-running nil)
	  (when (org-clock-is-active)
		(org-clock-out))
	  (org-agenda-remove-restriction-lock))

	(defun bh/clock-in-default-task ()
	  (save-excursion
		(org-with-point-at org-clock-default-task
		  (org-clock-in))))

	(defun bh/clock-in-parent-task ()
	  "Move point to the parent (project) task if any and clock in"
	  (let ((parent-task))
		(save-excursion
		  (save-restriction
			(widen)
			(while (and (not parent-task) (org-up-heading-safe))
			  (when (member (nth 2 (org-heading-components)) org-todo-keywords-1)
				(setq parent-task (point))))
			(if parent-task
				(org-with-point-at parent-task
				  (org-clock-in))
			  (when bh/keep-clock-running
				(bh/clock-in-default-task)))))))

	(defvar bh/organization-task-id "eb155a82-92b2-4f25-a3c6-0304591af2f9")

	(defun bh/clock-in-organization-task-as-default ()
	  (interactive)
	  (org-with-point-at (org-id-find bh/organization-task-id 'marker)
		(org-clock-in '(16))))

	(defun bh/clock-out-maybe ()
	  (when (and bh/keep-clock-running
				 (not org-clock-clocking-in)
				 (marker-buffer org-clock-default-task)
				 (not org-clock-resolving-clocks-due-to-idleness))
		(bh/clock-in-parent-task)))

	(add-hook 'org-clock-out-hook 'bh/clock-out-maybe 'append)

	(setq org-time-stamp-rounding-minutes (quote (1 1)))

	(setq org-agenda-clock-consistency-checks
		  (quote (:max-duration "4:00"
								:min-duration 0
								:max-gap 0
								:gap-ok-around ("4:00"))))

	;; Sometimes I change tasks I'm clocking quickly - this removes clocked tasks with 0:00 duration
	(setq org-clock-out-remove-zero-time-clocks t)

	;; Agenda clock report parameters
	(setq org-agenda-clockreport-parameter-plist
		  (quote (:link t :maxlevel 5 :fileskip0 t :compact t :narrow 80)))

	;; Set default column view headings: Task Effort Clock_Summary
	(setq org-columns-default-format "%80ITEM(Task) %10Effort(Effort){:} %10CLOCKSUM"	)

	;; Agenda log mode items to display (closed and state changes by default)
	(setq org-agenda-log-mode-items (quote (closed state)))

    ;; global Effort estimate values
	;; global STYLE property values for completion
	(setq org-global-properties (quote (("Effort_ALL" . "0:15 0:30 0:45 1:00 2:00 3:00 4:00 5:00 6:00 0:00")
										("STYLE_ALL" . "habit"))))

	(defun bh/is-project-p ()
	  "Any task with a todo keyword subtask"
	  (save-restriction
		(widen)
		(let ((has-subtask)
			  (subtree-end (save-excursion (org-end-of-subtree t)))
			  (is-a-task (member (nth 2 (org-heading-components)) org-todo-keywords-1)))
		  (save-excursion
			(forward-line 1)
			(while (and (not has-subtask)
						(< (point) subtree-end)
						(re-search-forward "^\*+ " subtree-end t))
			  (when (member (org-get-todo-state) org-todo-keywords-1)
				(setq has-subtask t))))
		  (and is-a-task has-subtask))))

	(defun bh/is-project-subtree-p ()
	  "Any task with a todo keyword that is in a project subtree.
		Callers of this function already widen the buffer view."
	  (let ((task (save-excursion (org-back-to-heading 'invisible-ok)
								  (point))))
		(save-excursion
		  (bh/find-project-task)
		  (if (equal (point) task)
			  nil
			t))))

	(defun bh/is-task-p ()
	  "Any task with a todo keyword and no subtask"
	  (save-restriction
		(widen)
		(let ((has-subtask)
			  (subtree-end (save-excursion (org-end-of-subtree t)))
			  (is-a-task (member (nth 2 (org-heading-components)) org-todo-keywords-1)))
		  (save-excursion
			(forward-line 1)
			(while (and (not has-subtask)
						(< (point) subtree-end)
						(re-search-forward "^\*+ " subtree-end t))
			  (when (member (org-get-todo-state) org-todo-keywords-1)
				(setq has-subtask t))))
		  (and is-a-task (not has-subtask)))))

	(defun bh/is-subproject-p ()
	  "Any task which is a subtask of another project"
	  (let ((is-subproject)
			(is-a-task (member (nth 2 (org-heading-components)) org-todo-keywords-1)))
		(save-excursion
		  (while (and (not is-subproject) (org-up-heading-safe))
			(when (member (nth 2 (org-heading-components)) org-todo-keywords-1)
			  (setq is-subproject t))))
		(and is-a-task is-subproject)))

	(defun bh/list-sublevels-for-projects-indented ()
	  "Set org-tags-match-list-sublevels so when restricted to a subtree we list all subtasks.
  This is normally used by skipping functions where this variable is already local to the agenda."
	  (if (marker-buffer org-agenda-restrict-begin)
		  (setq org-tags-match-list-sublevels 'indented)
		(setq org-tags-match-list-sublevels nil))
	  nil)

	(defun bh/list-sublevels-for-projects ()
	  "Set org-tags-match-list-sublevels so when restricted to a subtree we list all subtasks.
  This is normally used by skipping functions where this variable is already local to the agenda."
	  (if (marker-buffer org-agenda-restrict-begin)
		  (setq org-tags-match-list-sublevels t)
		(setq org-tags-match-list-sublevels nil))
	  nil)

	(defvar bh/hide-scheduled-and-waiting-next-tasks t)

	(defun bh/toggle-next-task-display ()
	  (interactive)
	  (setq bh/hide-scheduled-and-waiting-next-tasks (not bh/hide-scheduled-and-waiting-next-tasks))
	  (when  (equal major-mode 'org-agenda-mode)
		(org-agenda-redo))
	  (message "%s WAITING and SCHEDULED NEXT Tasks" (if bh/hide-scheduled-and-waiting-next-tasks "Hide" "Show")))

	(defun bh/skip-stuck-projects ()
	  "Skip trees that are not stuck projects"
	  (save-restriction
		(widen)
		(let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
		  (if (bh/is-project-p)
			  (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
					 (has-next ))
				(save-excursion
				  (forward-line 1)
				  (while (and (not has-next) (< (point) subtree-end) (re-search-forward "^\\*+ NEXT " subtree-end t))
					(unless (member "WAITING" (org-get-tags-at))
					  (setq has-next t))))
				(if has-next
					nil
				  next-headline)) ; a stuck project, has subtasks but no next task
			nil))))

	(defun bh/skip-non-stuck-projects ()
	  "Skip trees that are not stuck projects"
	  ;; (bh/list-sublevels-for-projects-indented)
	  (save-restriction
		(widen)
		(let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
		  (if (bh/is-project-p)
			  (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
					 (has-next ))
				(save-excursion
				  (forward-line 1)
				  (while (and (not has-next) (< (point) subtree-end) (re-search-forward "^\\*+ NEXT " subtree-end t))
					(unless (member "WAITING" (org-get-tags-at))
					  (setq has-next t))))
				(if has-next
					next-headline
				  nil)) ; a stuck project, has subtasks but no next task
			next-headline))))

	(defun bh/skip-non-projects ()
	  "Skip trees that are not projects"
	  ;; (bh/list-sublevels-for-projects-indented)
	  (if (save-excursion (bh/skip-non-stuck-projects))
		  (save-restriction
			(widen)
			(let ((subtree-end (save-excursion (org-end-of-subtree t))))
			  (cond
			   ((bh/is-project-p)
				nil)
			   ((and (bh/is-project-subtree-p) (not (bh/is-task-p)))
				nil)
			   (t
				subtree-end))))
		(save-excursion (org-end-of-subtree t))))

	(defun bh/skip-non-tasks ()
	  "Show non-project tasks.
Skip project and sub-project tasks, habits, and project related tasks."
	  (save-restriction
		(widen)
		(let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
		  (cond
		   ((bh/is-task-p)
			nil)
		   (t
			next-headline)))))

	(defun bh/skip-project-trees-and-habits ()
	  "Skip trees that are projects"
	  (save-restriction
		(widen)
		(let ((subtree-end (save-excursion (org-end-of-subtree t))))
		  (cond
		   ((bh/is-project-p)
			subtree-end)
		   ((org-is-habit-p)
			subtree-end)
		   (t
			nil)))))

	(defun bh/skip-projects-and-habits-and-single-tasks ()
	  "Skip trees that are projects, tasks that are habits, single non-project tasks"
	  (save-restriction
		(widen)
		(let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
		  (cond
		   ((org-is-habit-p)
			next-headline)
		   ((and bh/hide-scheduled-and-waiting-next-tasks
				 (member "WAITING" (org-get-tags-at)))
			next-headline)
		   ((bh/is-project-p)
			next-headline)
		   ((and (bh/is-task-p) (not (bh/is-project-subtree-p)))
			next-headline)
		   (t
			nil)))))

	(defun bh/skip-project-tasks-maybe ()
	  "Show tasks related to the current restriction.
When restricted to a project, skip project and sub project tasks, habits, NEXT tasks, and loose tasks.
When not restricted, skip project and sub-project tasks, habits, and project related tasks."
	  (save-restriction
		(widen)
		(let* ((subtree-end (save-excursion (org-end-of-subtree t)))
			   (next-headline (save-excursion (or (outline-next-heading) (point-max))))
			   (limit-to-project (marker-buffer org-agenda-restrict-begin)))
		  (cond
		   ((bh/is-project-p)
			next-headline)
		   ((org-is-habit-p)
			subtree-end)
		   ((and (not limit-to-project)
				 (bh/is-project-subtree-p))
			subtree-end)
		   ((and limit-to-project
				 (bh/is-project-subtree-p)
				 (member (org-get-todo-state) (list "NEXT")))
			subtree-end)
		   (t
			nil)))))

	(defun bh/skip-project-tasks ()
	  "Show non-project tasks.
Skip project and sub-project tasks, habits, and project related tasks."
	  (save-restriction
		(widen)
		(let* ((subtree-end (save-excursion (org-end-of-subtree t))))
		  (cond
		   ((bh/is-project-p)
			subtree-end)
		   ((org-is-habit-p)
			subtree-end)
		   ((bh/is-project-subtree-p)
			subtree-end)
		   (t
			nil)))))

	(defun bh/skip-non-project-tasks ()
	  "Show project tasks.
Skip project and sub-project tasks, habits, and loose non-project tasks."
	  (save-restriction
		(widen)
		(let* ((subtree-end (save-excursion (org-end-of-subtree t)))
			   (next-headline (save-excursion (or (outline-next-heading) (point-max)))))
		  (cond
		   ((bh/is-project-p)
			next-headline)
		   ((org-is-habit-p)
			subtree-end)
		   ((and (bh/is-project-subtree-p)
				 (member (org-get-todo-state) (list "NEXT")))
			subtree-end)
		   ((not (bh/is-project-subtree-p))
			subtree-end)
		   (t
			nil)))))

	(defun bh/skip-projects-and-habits ()
	  "Skip trees that are projects and tasks that are habits"
	  (save-restriction
		(widen)
		(let ((subtree-end (save-excursion (org-end-of-subtree t))))
		  (cond
		   ((bh/is-project-p)
			subtree-end)
		   ((org-is-habit-p)
			subtree-end)
		   (t
			nil)))))

	(defun bh/skip-non-subprojects ()
	  "Skip trees that are not projects"
	  (let ((next-headline (save-excursion (outline-next-heading))))
		(if (bh/is-subproject-p)
			nil
		  next-headline)))

	(defun bh/skip-non-archivable-tasks ()
	  "Skip trees that are not available for archiving"
	  (save-restriction
		(widen)
		;; Consider only tasks with done todo headings as archivable candidates
		(let ((next-headline (save-excursion (or (outline-next-heading) (point-max))))
			  (subtree-end (save-excursion (org-end-of-subtree t))))
		  (if (member (org-get-todo-state) org-todo-keywords-1)
			  (if (member (org-get-todo-state) org-done-keywords)
				  (let* ((daynr (string-to-int (format-time-string "%d" (current-time))))
						 (a-month-ago (* 60 60 24 (+ daynr 1)))
						 (last-month (format-time-string "%Y-%m-" (time-subtract (current-time) (seconds-to-time a-month-ago))))
						 (this-month (format-time-string "%Y-%m-" (current-time)))
						 (subtree-is-current (save-excursion
											   (forward-line 1)
											   (and (< (point) subtree-end)
													(re-search-forward (concat last-month "\\|" this-month) subtree-end t)))))
					(if subtree-is-current
						subtree-end ; Has a date in this month or last month, skip it
					  nil))  ; available to archive
				(or subtree-end (point-max)))
			next-headline))))

	; Enable habit tracking (and a bunch of other modules)
	(setq org-modules (quote (org-bbdb
							  org-bibtex
							  org-crypt
							  org-gnus
							  org-id
							  org-info
							  org-jsinfo
							  org-habit
							  org-inlinetask
							  org-irc
							  org-mew
							  org-mhe
							  org-protocol
							  org-rmail
							  org-vm
							  org-wl
							  org-w3m)))

	; position the habit graph on the agenda to the right of the default
	(setq org-habit-graph-column 50)

	;; Reveal
	;;(setq org-reveal-root "~/src/reveal.js")
	(setq org-reveal-root "file:/home/user/src/reveal.js")
  )

  ;; Spaceline
  (setq powerline-default-separator 'arrow
        spaceline-buffer-encoding-abbrev-p nil
        spaceline-version-control-p nil
        spaceline-erc-track-p nil)
  (spaceline-toggle-org-clock-on)


)
;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#0a0814" "#f2241f" "#67b11d" "#b1951d" "#4f97d7" "#a31db1" "#28def0" "#b2b2b2"])
 '(package-selected-packages
   (quote
    (helm-themes helm-swoop helm-projectile helm-nixos-options helm-mode-manager helm-gitignore helm-flx helm-descbinds helm-css-scss helm-company helm-c-yasnippet helm-ag ace-jump-helm-line magit magit-popup git-commit with-editor web-mode tagedit slim-mode scss-mode sass-mode pug-mode ox-reveal ox-gfm nix-mode less-css-mode haml-mode emmet-mode elfeed-web simple-httpd elfeed-org elfeed-goodies ace-jump-mode noflet elfeed company-web web-completion-data company-nixos-options nixos-options company-auctex auctex-latexmk auctex ledger-mode flycheck-ledger disaster company-c-headers cmake-mode clang-format xterm-color smeargle shell-pop ranger orgit org-projectile org-present org-pomodoro flycheck-pos-tip flycheck company-quickhelp pos-tip fuzzy company-statistics company auto-yasnippet yasnippet ac-ispell auto-complete ws-butler winum which-key wgrep volatile-highlights vi-tilde-fringe uuidgen use-package toc-org spaceline powerline smex restart-emacs request rainbow-delimiters popwin persp-mode pcre2el paradox spinner org-plus-contrib org-bullets open-junk-file neotree move-text macrostep lorem-ipsum linum-relative link-hint ivy-hydra info+ indent-guide hydra hungry-delete hl-todo highlight-parentheses highlight-numbers parent-mode highlight-indentation hide-comnt help-fns+ helm-make helm helm-core google-translate golden-ratio flx-ido flx fill-column-indicator fancy-battery eyebrowse expand-region exec-path-from-shell evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-surround evil-search-highlight-persist evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-lisp-state smartparens evil-indent-plus evil-iedit-state iedit evil-exchange evil-escape evil-ediff evil-args evil-anzu anzu evil goto-chg undo-tree eval-sexp-fu highlight elisp-slime-nav dumb-jump popup f s diminish define-word counsel-projectile projectile pkg-info epl counsel swiper ivy column-enforce-mode clean-aindent-mode bind-map bind-key auto-highlight-symbol auto-compile packed dash async aggressive-indent adaptive-wrap ace-window ace-link avy))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((((class color) (min-colors 257)) (:foreground "#F8F8F2" :background "#272822")) (((class color) (min-colors 89)) (:foreground "#F5F5F5" :background "#1B1E1C")))))
