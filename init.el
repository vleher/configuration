;;; init.el --- Emacs Configuration All
;; -*- coding: utf-8; lexical-binding: t -*-

;;; Commentary:
(require 'package)					 ; Bring in to the environment all package management functions

;; A list of package repositories
;;; Code:
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
						 ("org"	  . "https://orgmode.org/elpa/")
						 ("elpa"  . "https://elpa.gnu.org/packages/")
						 ("nongnu" . "https://elpa.nongnu.org/nongnu/")))

;; Constants.
(defconst --emacs-start-time (current-time))
(defconst --lisp-dir (concat user-emacs-directory "lisp/"))
(defconst --misc-dir (concat user-emacs-directory "misc/"))
(defconst --yas-dir (concat user-emacs-directory "snippets/"))
(defconst --themes-dir (concat user-emacs-directory "themes/"))
(defconst --user-cache-dir (concat user-emacs-directory "cache/"))
(defconst --auto-save-dir (concat user-emacs-directory "auto-save/"))

(setq package-gnupghome-dir "~/.emacs.d/elpa/gnupg/")

(package-initialize)				;; Initializes the package system and prepares it to be used
(unless package-archive-contents	 ; Unless a package archive already exists,
  (package-refresh-contents))		 ; Refresh package contents so that Emacs knows which packages to load

;; Initialize use-package on non-linux platforms
(unless (package-installed-p 'use-package)		  ; Unless "use-package" is installed, install "use-package"
  (package-refresh-contents)
  (package-install 'use-package))
(eval-and-compile
  (setq use-package-always-ensure t
		use-package-expand-minimally t))
(require 'use-package)							  ; Once it's installed, we load it using require
;; Make sure packages are downloaded and installed before they are run
;; also frees you from having to put :ensure t after installing EVERY PACKAGE.
(setq use-package-always-ensure t)
(setq package-native-compile t)

(setq lexical-binding t)
(setq frame-resize-pixelwise t)
;;(setq debug-on-error t)

;; Garbage collect at the end of the startup
(add-hook 'after-init-hook #'garbage-collect t)

;; Load server if not already running
(load "server")
(unless (server-running-p) (server-start))

;; Configure the Emacs Frame
(setq inhibit-startup-screen t)
(tool-bar-mode 0)
(blink-cursor-mode 0)
(tab-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(setq frame-title-format '("" "[%b] <%f> - Emacs " emacs-version))
(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 1024 1024))
(setq ring-bell-function 'ignore)
(setq initial-scratch-message nil)
(setq sentence-end-double-space nil)
(setq-default cursor-type 'bar)
(prefer-coding-system 'utf-8-unix)

(setq use-dialog-box nil)
(setq kill-do-not-save-duplicates t)
(setq help-window-select t)

;;set the line number mode to true
(setq line-number-mode t)
(global-display-line-numbers-mode t)
(column-number-mode t)
(delete-selection-mode t)
(size-indication-mode t)
(global-font-lock-mode t)
(global-hl-line-mode t)

;; package update configuration
(use-package auto-package-update
  :ensure t
  :defer 10
  :config
  ;; Delete residual old versions
  (setq auto-package-update-delete-old-versions t)
  ;; Do not bother me when updates have taken place.
  (setq auto-package-update-hide-results t)
  ;; Update installed packages at startup if there is an update pending.
  (auto-package-update-maybe))

;;; Theme configuration
(use-package nord-theme
  :ensure t
  :config (setq nord-region-highlight "frost"))
(load-theme 'nord t)

;; Try to fix the mode line
(use-package diminish  :config (diminish 'visual-line-mode))

;; Garbage Collection
(use-package gcmh  :diminish)
(gcmh-mode 1)

;; Visual Fill Column
(global-visual-line-mode t)
(use-package visual-fill-column
  :config
  (setq-default visual-fill-column-width 300)
  (setq-default visual-line-fill-column-mode t)
  (setq-default visual-fill-column-center-text t)
  (setq-default visual-fill-column-enable-sensible-window-split t)
  (setq-default visual-fill-column-fringes-outside-margins t) )
(global-visual-fill-column-mode t)
(setq whitespace-line-column 264)

(save-place-mode)

;; Winner mode
(winner-mode t)

;; Ediff configuration
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
(setq ediff-split-window-function 'split-window-horizontally)

;; Windows Hacks
(use-package vc-defer
  :diminish vc-defer-mode
  :config (add-to-list 'vc-defer-backends 'git) (vc-defer-mode))

;; Parenthesis
(show-paren-mode 1)
(use-package smartparens  :diminish smartparens-mode :config (smartparens-mode t))
;; Automatically add ending brackets and braces
(electric-pair-mode 1)
;; Make return key also do indent in the current buffer
(electric-indent-local-mode 1)
;; make return key also do indent globally
(electric-indent-mode 1)

;; Save hooks
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Tabs vs Spaces
;; Indent uses tabs
(setq-default indent-tabs-mode t)
;; Set the default tab size to 4
(setq-default tab-width 4)
;; Tab key will always indent command
(setq-default tab-always-indent t)
(setq indent-line-function 'insert-tab)

;; Save History
(setq history-length 25)
(use-package savehist :init (savehist-mode))

;; Unicode fonts
(use-package unicode-fonts
  :config
  (unicode-fonts-setup))

;; Recent flie list
(use-package recentf)
(recentf-mode 1)
(setq recentf-max-saved-items 50)

;; Increase the maximum for eval depth
(setq max-lisp-eval-depth 100000)

;; Move to first whitespace or begninning of line if none. Pressing again goes to the beginning if
;; there was whitespace.
(global-set-key [remap move-beginning-of-line] 'smarter-move-beginning-of-line)
(global-set-key (kbd "M-SPC") 'cycle-spacing)

;; Make font bigger/smaller.
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
(global-set-key (kbd "C-0") 'text-scale-adjust)

(global-set-key (kbd "C-<tab>") 'tab-next)
;; VLF
(use-package vlf)

;; Orderless
(use-package orderless
  :init
  ;; Set completion-styles
  (setq completion-styles '(initials flex partial-completion basic orderless)
		completion-category-defaults nil
		completion-category-overrides nil))

;; Windmove
(windmove-default-keybindings)

;; Ace Window
(use-package ace-window )
(global-set-key (kbd "M-o") 'ace-window)
(setq aw-keys '(?a ?h ?e ?t ?i ?s ?w ?n))
(setq aw-dispatch-always t)

;; Revert the buffer automatically if it changes in the filesystem
(global-auto-revert-mode t)
(setq auto-revert-check-vc-info t)

;; Resume the previous session
(desktop-save-mode 1)
;; Smoother scrolling
;; (setq scroll-margin 5)
;; (setq scroll-conservatively 100)
;; (setq scroll-preserve-screen-position 1)
(use-package smooth-scrolling
  :config
  (setq smooth-scrolling-mode t))
(use-package good-scroll
  :config
  (setq good-scroll-mode t))

;; Put backup files neatly away
(let ((backup-dir (concat user-emacs-directory	"/backups"))
	  (auto-saves-dir (concat user-emacs-directory "/auto-saves")))
  (dolist (dir (list backup-dir auto-saves-dir))
	(when (not (file-directory-p dir))
	  (make-directory dir t)))
  (setq backup-directory-alist `(("." . ,backup-dir))
		auto-save-file-name-transforms `((".*" ,auto-saves-dir t))
		auto-save-list-file-prefix (concat auto-saves-dir ".saves-")))

(setq backup-by-copying t	 ; Don't delink hardlinks
	  delete-old-versions t	 ; Clean up the backups
	  version-control t		 ; Use version numbers on backups,
	  kept-new-versions 5	 ; keep some new versions
	  kept-old-versions 2)	 ; and some old ones, too

;; Set initial mode
(setq initial-major-mode 'org-mode)
(setq org-use-sub-superscripts nil)
(setq org-export-with-sub-superscripts nil)

;; Set fonts (for linux and windows)
(cond
 ((find-font (font-spec :name "Noto Sans Mono"))
  (set-face-attribute 'default nil :font "Noto Sans Mono-9.5"))
 ((find-font (font-spec :name "DejaVu Sans Mono"))
  (set-face-attribute 'default nil :font "DejaVu Sans Mono-9.5"))
 ((find-font (font-spec :name "Consolas"))
  (progn
	(set-face-attribute 'default nil :font "Consolas-10.0"))))

;; Making it easier to discover Emacs key presses.
(use-package which-key
  :diminish
  :defer 5
  :config (which-key-mode)
  (which-key-setup-side-window-bottom)
  (setq which-key-idle-delay 1.05))

;; Treesitter
(use-package treesit-auto
  :demand t
  :custom (treesit-auto-install 'prompt)
  :config (treesit-auto-add-to-auto-mode-alist 'all)(global-treesit-auto-mode))

;; Dim the inactive buffers
(use-package dimmer
  :custom (dimmer-fraction 0.2)
  :config (dimmer-mode))

;; paren matching with color
(use-package rainbow-delimiters :hook ((prog-mode . rainbow-delimiters-mode)))

(use-package htmlize :defer t)

;; Crux Configuration
(use-package crux)
(global-set-key (kbd "C-c s") 'crux-create-scratch-buffer)

;; Apheleia
(use-package apheleia  :diminish :config (apheleia-global-mode +1))

;; Tramp
(use-package tramp)
(setq tramp-default-method "ssh")
(setq tramp-verbose 1)
(setq tramp-default-remote-shell "/bin/bash")

(connection-local-set-profile-variables 'tramp-connection-local-default-shell-profile
										'((shell-file-name . "/bin/bash")
										  (shell-command-switch . "-c")))

;; Org Mode Configuration
(use-package org
  :hook ((org-mode . visual-line-mode) (org-mode . pt/org-mode-hook))
  :hook ((org-src-mode . display-line-numbers-mode))
  :bind (("C-c a" . org-agenda))
  :custom
  (org-adapt-indentation t)
  (org-indent-mode t)
  (org-hide-emphasis-markers nil)
  (org-indent-indentation-per-level 5)
  (org-list-description-max-indent 5)
  (org-pretty-entities t)
  (org-return-follows-link t)
  (org-special-ctrl-a/e t)
  (org-src-ask-before-returning-to-edit-buffer nil "org-src is kinda needy out of the box")
  (org-src-window-setup 'current-window)
  (org-cycle-include-plain-lists 'integrate)
  (org-agenda-files '("~/workspace/notes/"))
  (org-startup-folded 'content)
  (org-startup-indented t)
  (org-startup-with-inline-images t)
  (org-toggle-pretty-entities nil)
  (org-export-with-sub-superscripts nil)
  (org-todo-keywords '((sequence "TODO" "WORKING" "WAITING" "|" "DONE" "CANCELLED" "HOLD")))

  :config
  (defun pt/org-mode-hook ())
  (defun make-inserter (c) '(lambda () (interactive) (insert-char c)))
  (defun zero-width () (interactive) (insert "​")))
(setq org-default-notes-dir "~/workspace/notes/")
(setq org-agenda-prefix-format
	  '((agenda . " %i %-36c%?-18t% s")
		(todo   . " %i %-36c%?-18t% s")
		(tags   . " %i %-36c%?-18t% s")
		(search . " %i %-36c%?-18t% s")))

(setq org-agenda-custom-commands
	  '(("d" "Dashboard"
		 ((agenda "" ((org-agenda-span 1)
					  (org-agenda-block-separator nil)
					  (org-agenda-overriding-header " ")))
		  (todo "WAITING"
				((org-agenda-block-separator nil) (org-agenda-overriding-header "\n")))
		  (todo "WORKING"
				((org-agenda-block-separator nil) (org-agenda-overriding-header "\n")))
		  (todo "HOLD"
				((org-agenda-block-separator nil) (org-agenda-overriding-header "\n")))
		  (todo "TODO"
				((org-agenda-block-separator nil) (org-agenda-overriding-header "\n")))
		  ))))

(load "~/configuration/org-publish.el")

(put 'erase-buffer 'disabled nil)

;; Fuzzy Search
(setq search-whitespace-regexp ".*")
(setq isearch-lax-whitespace t)
(setq isearch-regexp-lax-whitespace nil)
(setq isearch-lazy-count t)

(use-package eldoc :config (global-eldoc-mode))
(setq eldoc-echo-area-use-multiline-p nil)

;; RIPgrep and rg
(use-package ripgrep)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-headline-done ((t nil)))
 '(org-level-1 ((t (:extend nil :foreground "#8FBCBB" :weight extra-bold :height 1.1))))
 '(shadow ((t (:foreground "gray52")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Custom splitting functions ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun vsplit-last-buffer ()
  "Splits window vertically and load the next buffer."
  (interactive)
  (split-window-vertically)
  (other-window 1 nil)
  (switch-to-next-buffer))
(defun hsplit-last-buffer ()
  "Splits window horizontally and load the next buffer."
  (interactive)
  (split-window-horizontally)
  (other-window 1 nil)
  (switch-to-next-buffer))

(global-set-key (kbd "C-x 2") 'vsplit-last-buffer)
(global-set-key (kbd "C-x 3") 'hsplit-last-buffer)

;; All the Icons
(use-package all-the-icons )

;; Magit configuration
(use-package magit
  :diminish magit-auto-revert-mode
  :diminish auto-revert-mode
  :bind (("C-x g" . #'magit-status))
  :config
  (defun pt/commit-hook () (set-fill-column 164))
  (add-hook 'git-commit-setup-hook #'pt/commit-hook)
  (add-to-list 'magit-no-confirm 'stage-all-changes))

(use-package diff-hl
  :config
  (global-diff-hl-mode)
  (diff-hl-flydiff-mode)
  (diff-hl-margin-mode)
  (add-hook 'magit-pre-refresh-hook 'diff-hl-magit-pre-refresh)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
  :custom
  (diff-hl-disable-on-remote t)
  (diff-hl-margin-symbols-alist
   '((insert . " ")
	 (delete . " ")
	 (change . " ")
	 (unknown . "?")
	 (ignored . "i"))))


;;;; Corfu
(use-package corfu
  ;; Optional customizations
  :custom
  (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  (corfu-auto t)                 ;; Enable auto completion
  (corfu-auto-delay 0.50)
  (corfu-auto-timer 0.750)
  (corfu-auto-prefix 2)
  (corfu-min-width 80)
  (corfu-separator ?\s)          ;; Orderless field separator
  (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  (corfu-quit-no-match t)      ;; Never quit, even if there is no match
  (completion-styles '(initials basic orderless))
  ;; (corfu-preview-current nil)    ;; Disable current candidate preview
  ;; (corfu-preselect-first nil)    ;; Disable candidate preselection
  ;; (corfu-on-exact-match nil)     ;; Configure handling of exact matches
  ;; (corfu-scroll-margin 5)        ;; Use scroll margin

  ;; Enable Corfu only for certain modes.
  ;; :hook ((prog-mode . corfu-mode)
  ;;        (shell-mode . corfu-mode)
  ;;        (eshell-mode . corfu-mode))

  ;; Recommended: Enable Corfu globally.
  ;; This is recommended since Dabbrev can be used globally (M-/).
  ;; See also `corfu-excluded-modes'.
  :init
  (global-corfu-mode t))

;; A few more useful configurations...
(use-package emacs
  :init
  ;; TAB cycle if there are only few candidates
  (setq completion-cycle-threshold 3)

  ;; Emacs 28: Hide commands in M-x which do not apply to the current mode.
  ;; Corfu commands are hidden, since they are not supposed to be used via M-x.
  ;; (setq read-extended-command-predicate
  ;;       #'command-completion-default-include-p)

  ;; Enable indentation+completion using the TAB key.
  ;; `completion-at-point' is often bound to M-TAB.
  ;;(setq tab-always-indent 'complete)
  )

;; Cape with corfu
;; Add extensions
(use-package cape)

;; yasnippet configuration
(use-package yasnippet  :diminish yas-minor-mode :config (yas-global-mode t))
(use-package yasnippet-snippets :diminish yas-minor-mode)

;; Flymake mode
(with-eval-after-load "flymake"
  (define-key flymake-mode-map (kbd "M-n") 'flymake-goto-next-error)
  (define-key flymake-mode-map (kbd "M-p") 'flymake-goto-prev-error))

;; Enable flyover-mode globally
(use-package flyover)
(add-hook 'flycheck-mode-hook #'flyover-mode)

;; Configure which error levels to display
;; Possible values: error, warning, info
(setq flyover-levels '(error warning info))  ; Show all levels
;; (setq flyover-levels '(error warning))    ; Show only errors and warnings
;; (setq flyover-levels '(error))            ; Show only errors
;; Use theme colors for error/warning/info faces
(setq flyover-use-theme-colors t)

;; Adjust background lightness (lower values = darker)
(setq flyover-background-lightness 45)

;; Make icon background darker than foreground
(setq flyover-percent-darker 40)

(setq flyover-text-tint 'lighter) ;; or 'darker or nil

;; "Percentage to lighten or darken the text when tinting is enabled."
(setq flyover-text-tint-percent 50)
;; Choose which checkers to use (flycheck, flymake, or both)
(setq flyover-checkers '(flycheck flymake))

;; Enable debug messages
(setq flyover-debug nil)
;; Time in seconds to wait before checking and displaying errors after a change
(setq flyover-debounce-interval 0.2)

;;;;; Treemacs ;;;;;;
(use-package treemacs
  :config
  (treemacs-follow-mode t)
  (treemacs-filewatch-mode t)
  (treemacs-fringe-indicator-mode `always)
  :commands (treemacs))

(add-hook 'treemacs-mode-hook (lambda() (display-line-numbers-mode -1)))
(use-package treemacs-icons-dired :hook (dired-mode . treemacs-icons-dired-enable-once) )
(add-hook 'dired-mode-hook 'treemacs-icons-dired-mode)

;; Git Info in Dired
(use-package dired-git-info  :after dired :commands (dired-git-info-mode))

(use-package hydra)

;;;;; Vertico and Completion ;;;;;
(use-package prescient
  :config
  (setq prescient-save-file (concat user-emacs-directory "prescient-save"))
  (prescient-persist-mode +1))

(use-package marginalia
  :config	(marginalia-mode +1))

(use-package vertico
  :init
  (vertico-mode)
  (setq vertico-resize -1)
  (setq vertico-cycle t))
(vertico-reverse-mode t)
(setq vertico-multiform-commands
      '((consult-line buffer)
        (consult-imenu buffer)
		(consult-ripgrep buffer)))
(setq vertico-multiform-categories
      '((consult-grep buffer)
		(imenu buffer)
		(occur buffer)
		(symbol (vertico-sort-function . vertico-sort-alpha))))

(vertico-multiform-mode)

;; Extra project stuff
(setq project-vc-extra-root-markers '(".project.el"))
(global-set-key (kbd "C-x C-b") 'ibuffer-jump)

;; Configuration for Consult
(use-package consult
  ;; Replace bindings. Lazily loaded due by `use-package'.
  :bind (;; C-c bindings (mode-specific-map)
		 ("C-c h" . consult-history)
		 ("C-c m" . consult-mode-command)
		 ("C-c k" . consult-kmacro)
		 ;; C-x bindings (ctl-x-map)
		 ("C-x M-:" . consult-complex-command)	   ;; orig. repeat-complex-command
		 ("C-x b" . consult-buffer)				   ;; orig. switch-to-buffer
		 ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
		 ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
		 ("C-x r b" . consult-bookmark)			   ;; orig. bookmark-jump
		 ("C-x p b" . consult-project-buffer)	   ;; orig. project-switch-to-buffer
		 ;; Custom M-# bindings for fast register access
		 ("M-#" . consult-register-load)
		 ("M-'" . consult-register-store)		   ;; orig. abbrev-prefix-mark (unrelated)
		 ("C-M-#" . consult-register)
		 ;; Other custom bindings
		 ("M-y" . consult-yank-pop)				   ;; orig. yank-pop
		 ("<help> a" . consult-apropos)			   ;; orig. apropos-command
		 ;; M-g bindings (goto-map)
		 ("M-g e" . consult-compile-error)
		 ("M-g g" . consult-goto-line)			   ;; orig. goto-line
		 ("M-g M-g" . consult-goto-line)		   ;; orig. goto-line
		 ("M-g o" . consult-outline)			   ;; Alternative: consult-org-heading
		 ("M-g m" . consult-mark)
		 ("M-g k" . consult-global-mark)
		 ("M-g i" . consult-imenu)
		 ("M-g I" . consult-imenu-multi)
		 ;; M-s bindings (search-map)
		 ("M-s d" . consult-find)
		 ("M-s D" . consult-locate)
		 ("M-s g" . consult-grep)
		 ("M-s G" . consult-git-grep)
		 ("M-s r" . consult-ripgrep)
		 ("M-s l" . consult-line)
		 ("M-s L" . consult-line-multi)
		 ("M-s m" . consult-multi-occur)
		 ("M-s k" . consult-keep-lines)
		 ("M-s u" . consult-focus-lines)
		 ;; Isearch integration
		 ("M-s e" . consult-isearch-history)
		 :map isearch-mode-map
		 ("M-e" . consult-isearch-history)		   ;; orig. isearch-edit-string
		 ("M-s e" . consult-isearch-history)	   ;; orig. isearch-edit-string
		 ("M-s l" . consult-line)				   ;; needed by consult-line to detect isearch
		 ("M-s L" . consult-line-multi)			   ;; needed by consult-line to detect isearch
		 ;; Minibuffer history
		 :map minibuffer-local-map
		 ("M-s" . consult-history)				   ;; orig. next-matching-history-element
		 ("M-r" . consult-history))				   ;; orig. previous-matching-history-element

  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  ;; The :init configuration is always executed (Not lazy)
  :init

  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0.5
		register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
		xref-show-definitions-function #'consult-xref)

  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :config

  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  (setq consult-preview-key 'any)

  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-recent-file
   consult--source-project-recent-file :preview-key '(:debounce 0.4 any))

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; (kbd "C-+")

  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)

  ;; By default `consult-project-function' uses `project-root' from project.el.
  ;; Optionally configure a different project root function.
  ;; There are multiple reasonable alternatives to chose from.
  ;;;; 1. project.el (the default)
  ;; (setq consult-project-function #'consult--default-project--function)
  ;;;; 2. projectile.el (projectile-project-root)
  ;; (autoload 'projectile-project-root "projectile")
  ;; (setq consult-project-function (lambda (_) (projectile-project-root)))
  ;;;; 3. vc.el (vc-root-dir)
  ;; (setq consult-project-function (lambda (_) (vc-root-dir)))
  ;;;; 4. locate-dominating-file
  ;; (setq consult-project-function (lambda (_) (locate-dominating-file "." ".git")))
  )

(use-package consult-ls-git :after consult)
(use-package consult-eglot :after consult)
(use-package consult-yasnippet :after consult)

;; XML Mode
(defun nxml-where ()
  "Display the hierarchy of XML elements the point is on as a path."
  (interactive)
  (let ((path nil))
    (save-excursion
      (save-restriction
        (widen)
        (while (and (< (point-min) (point)) ;; Doesn't error if point is at beginning of buffer
                    (condition-case nil
                        (progn
                          (nxml-backward-up-element) ; always returns nil
                          t)
                      (error nil)))
          (setq path (cons (xmltok-start-tag-local-name) path)))
        (if (called-interactively-p t)
            (message "/%s" (mapconcat 'identity path "/"))
          (format "/%s" (mapconcat 'identity path "/")))))))
(setq nxml-child-indent 4 nxml-attribute-indent 4)
(setq nxml-outline-child-indent 4)
(defun xml-find-file-hook ()
  "XML Find file hook to switch to nxml mode and display xpath."
  (when (derived-mode-p 'nxml-mode)
    (which-function-mode t)
    (setq which-func-mode t)
    (add-hook 'which-func-functions 'nxml-where t t)))

(add-hook 'find-file-hook 'xml-find-file-hook t)

;; Configuration for embark
(use-package embark
  :ensure
  :bind
  (("C-." . embark-act)			;; pick some comfortable binding
   ("M-." . embark-dwim)		;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'
  :init
  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)
  :config
  (define-key embark-file-map     (kbd "o") (my/embark-ace-action find-file))
  (define-key embark-buffer-map   (kbd "o") (my/embark-ace-action switch-to-buffer))

  (define-key embark-file-map     (kbd "h") (my/embark-split-action find-file split-window-below))
  (define-key embark-buffer-map   (kbd "h") (my/embark-split-action switch-to-buffer split-window-below))

  (define-key embark-file-map     (kbd "v") (my/embark-split-action find-file split-window-right))
  (define-key embark-buffer-map   (kbd "v") (my/embark-split-action switch-to-buffer split-window-right))
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
			   '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
				 nil
				 (window-parameters (mode-line-format . none)))))

;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :after (embark consult)
  :demand t ; only necessary if you have the hook below
  ;; if you want to have consult previews as you move around an
  ;; auto-updating embark collect buffer
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(eval-when-compile
  (defmacro my/embark-ace-action (fn)
    `(defun ,(intern (concat "my/embark-ace-" (symbol-name fn))) ()
       (interactive)
       (with-demoted-errors "%s"
         (require 'ace-window)
         (aw-switch-to-window (aw-select nil))
         (call-interactively (symbol-function ',fn)))))

  (defmacro my/embark-split-action (fn split-type)
    `(defun ,(intern (concat "my/embark-"
                             (symbol-name fn)
                             "-"
                             (car (last  (split-string
                                          (symbol-name split-type) "-"))))) ()
       (interactive)
       (funcall #',split-type)
       (call-interactively #',fn))))

;; Avy
(use-package avy)
(avy-setup-default)
(global-set-key (kbd "M-s a") 'avy-goto-char-timer)

;; Golden
(use-package golden-ratio :diminish)
(golden-ratio-mode 1)
(setq golden-ratio-auto-scale t)
(setq golden-ratio-max-width 200)

;; CSV mode ;;
(use-package csv-mode :mode ("\\.csv$" . csv-mode))

;;; Json ;;;;
(use-package json-mode
  :mode ("\\.json$" . json-mode))

;; PHP
(use-package php-mode)

;;; lsp-mode
;; (use-package lsp-mode
;;   :init
;;   (setq lsp-keymap-prefix "C-c l"
;; 		lsp-keep-workspace-alive nil
;; 		lsp-signature-doc-lines 5
;; 		lsp-lens-enable t)
;;   (defun my/lsp-mode-setup-completion ()
;; 	(setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
;; 		  '(orderless)))
;;   :hook
;;   (java-mode . lsp-deferred)
;;   (java-ts-mode . lsp-deferred)
;;   (lsp-mode . lsp-enable-which-key-integration)
;;   (lsp-completion-mode . my/lsp-mode-setup-completion)
;;   :commands
;;   (lsp lsp-deferred)
;;   :custom
;;   (lsp-completion-provider :none)
;;   :config
;;   (define-key lsp-mode-map (kbd "C-c l") lsp-command-map))

;; lsp-ui
;; (use-package lsp-ui
;;   :hook
;;   (lsp-mode . lsp-ui-mode)
;;   :commands
;;   (lsp-ui-mode)
;;   :init
;;   (setq lsp-ui-doc-position 'bottom
;; 		lsp-ui-doc-side 'right
;; 		lsp-ui-doc-delay 1.0
;; 		lsp-ui-doc-show-with-cursor t
;; 		lsp-ui-sideline-update-mode 'point))

;; (use-package dap-mode
;;   :after lsp-mode
;;   :config (dap-auto-configure-mode))
;; (setq dap-auto-configure-features '(sessions locals controls tooltip))

;; (setq lsp-idle-delay 0.5)
;; (setq lsp-log-io nil)

;;(add-hook 'prog-mode-hook #'lsp-deferred)

;; Java mode
;; (use-package lsp-java
;;   :config
;;   (add-hook 'java-mode-hook 'lsp)
;;   (add-hook 'java-ts-mode-hook 'lsp)
;;   (add-hook 'lsp-mode-hook #'lsp-lens-mode)
;;   (add-hook 'java-mode-hook #'lsp-java-lens-mode))

;; (setq lsp-java-format-settings-url "")
;; (setq lsp-java-inhibit-message t)
;; (setq lsp-java-format-tab-size 4)

;; (use-package lsp-sonarlint
;;   :custom
;;   (lsp-sonarlint-auto-download t)
;;   (lsp-sonarlint-show-analyzer-logs nil))

;; (add-to-list 'lsp-sonarlint-modes-enabled  'java-ts-mode)

;;(setq lsp-unzip-script lsp-ext-pwsh-script)

(setq eglot-events-buffer-size 0)
(setq eglot-report-progress nil)
(use-package eglot-java )
(with-eval-after-load 'eglot-java
  (define-key eglot-java-mode-map (kbd "C-c l n") #'eglot-java-file-new)
  (define-key eglot-java-mode-map (kbd "C-c l x") #'eglot-java-run-main)
  (define-key eglot-java-mode-map (kbd "C-c l t") #'eglot-java-run-test)
  (define-key eglot-mode-map (kbd "C-c l a") 'eglot-code-actions)
  (define-key eglot-mode-map (kbd "C-c l r") 'eglot-rename)
  (define-key eglot-java-mode-map (kbd "C-c l q") #'eglot-code-action-quickfix)
  (define-key eglot-java-mode-map (kbd "C-c l r") #'eglot-rename))

(add-hook 'java-mode-hook (lambda ()
							(remove-hook 'eglot-connect-hook #'eglot-signal-didChangeConfiguration t)))

(use-package cognitive-complexity :load-path "~/workspace/cognitive-complexity")

;; REPL development
;; (use-package repl-driven-development
;;   :config
;;   (repl-driven-development [C-x C-j] java)       ;; e“X”ecute “j”ava
;;   (repl-driven-development [C-x C-n] javascript) ;; e“X”ecute “n”odejs
;;   (repl-driven-development [C-x C-p] python)     ;; e“X”ecute “p”ython
;;   (repl-driven-development [C-x C-t] terminal))  ;; e“X”ecute “t”erminal


;; Java Formatting
;; (add-to-list 'eglot-server-programs
;; 			 `((java-mode java-ts-mode) . ("jdtls"
;; 										   :initializationOptions
;; 										   (:settings
;; 											(:java
;; 											 (:format (:enabled t :settings (:url "~/workspace/conf/eclipse-formatter-java.xml" :profile "google-variant")))
;; 											 :extendedClientCapabilities (:classFileContentsSupport t)))
;; 										   ))
;; 			 )

;; CSS
(require 'css-mode)
(setq-default css-indent-offset 4)

(setq auto-mode-alist
	  (append '(("\\.min.css$" . fundamental-mode) ;; Faster to load.
				("\\.css$" . css-mode)
				("\\.style$" . css-mode))
			  auto-mode-alist))

;; Markdown
(use-package markdown-mode
  :mode (("\\.markdown\\'" . markdown-mode)
		 ("\\.md\\'" . markdown-mode))
  :config
  ;; Turn off auto-fill-mode beacuse markdown is sensitive about newlines.
  (add-hook 'markdown-mode-hook
			(lambda ()
			  (auto-fill-mode 0)
			  (visual-line-mode t))))

;; Python
(require 'python)
(setq python-indent-offset 2)
(use-package python-black
  :bind (("C-c b" . python-black-buffer)))

(use-package pyvenv
  :config
  (pyvenv-mode 1))

(use-package anaconda-mode
  :bind (("C-c C-x" . next-error))
  :config
  (require 'pyvenv)
  (add-hook 'python-ts-mode-hook 'anaconda-mode))

(use-package highlight-indent-guides
  :config
  (add-hook 'python-ts-mode-hook 'highlight-indent-guides-mode)
  (setq highlight-indent-guides-method 'character))

;; Rust Configuration
(use-package rustic
  :config
  (setq rustic-format-on-save t)
  (setq rustic-lsp-client 'eglot)
  )

(use-package cargo-mode )
(add-to-list 'eglot-server-programs
             '((rust-ts-mode rust-mode) .
               ("rust-analyzer" :initializationOptions (:check (:command "clippy")))))

(remove-hook 'rustic-mode-hook 'flycheck-mode)
(add-hook 'rust-mode-hook 'eglot-ensure)

;; Eglot
(add-hook 'prog-mode-hook 'flymake-mode)
(add-hook 'prog-mode-hook 'corfu-mode)

(add-hook 'java-ts-mode-hook 'eglot-ensure)
(add-hook 'java-ts-mode-hook 'eglot-java-mode)
(add-hook 'java-ts-mode-hook 'corfu-mode)

(add-hook 'php-ts-mode 'eglot-ensure)
(add-hook 'c-ts-mode 'eglot-ensure)
(add-hook 'c++-ts-mode 'eglot-ensure)
(add-hook 'sh-ts-mode 'eglot-ensure)
(add-hook 'shell-ts-mode 'eglot-ensure)
(add-hook 'css-ts-mode 'eglot-ensure)
(add-hook 'json-ts-mode 'eglot-ensure)
(add-hook 'js-ts-mode 'eglot-ensure)
(add-hook 'perl-ts-mode 'eglot-ensure)
(add-hook 'python-ts-mode 'eglot-ensure)
(add-hook 'yaml-ts-mode 'eglot-ensure)
(add-hook 'rust-ts-mode 'eglot-ensure)

;; Set Java VM for windows
(when (eq system-type 'windows-nt)
  (setq lsp-java-configuration-runtimes '[(:name "JavaSE-11" :path "C:/Users/leherv/.jdks/openjdk-11.0.9.1") 	 (:name "JavaSE-23" :path "C:/Users/leherv/.jdks/jdk-23.0.1/")])
  (setenv "JAVA_HOME" "C:/Users/leherv/.jdks/jdk-23.0.1/")
  (setq lsp-java-java-path "C:/Users/leherv/.jdks/jdk-23.0.1/bin/java")
  (setenv "PATH" "C:/msys64/ucrt64/bin;C:/msys64/usr/bin/"))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("98b4ef49c451350c28a8c20c35c4d2def5d0b8e5abbc962da498c423598a1cdd"
	 default))
 '(org-file-apps
   '((auto-mode . emacs) (directory . emacs) ("\\.x?html?\\'" . default)
	 ("\\.pdf\\'" . default) ("\\.docx?\\'" . "open %s")
	 ("\\.xlsm?\\'" . "open %s")))
 '(package-selected-packages
   '(all-the-icons anaconda-mode apheleia auto-package-update cape
				   cargo-mode cloc code-compass consult-eglot
				   consult-ls-git consult-yasnippet corfu crux
				   csv-mode diff-hl diminish dimmer dired-git-info
				   eglot-java embark-consult flyover gcmh golden-ratio
				   good-scroll highlight-indent-guides htmlize
				   json-mode magit marginalia nord-theme orderless
				   php-mode prescient python-black pyvenv
				   rainbow-delimiters repl-driven-development ripgrep
				   rustic smartparens smooth-scrolling
				   treemacs-icons-dired treesit-auto unicode-fonts
				   vc-defer vertico visual-fill-column vlf which-key
				   yasnippet-snippets)))

(provide 'init)
;;; init.el ends here
