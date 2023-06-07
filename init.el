;;; init.el --- Emacs Configuration All
;; -*- coding: utf-8; lexical-binding: t -*-

;;; Commentary:
(require 'package)					 ; Bring in to the environment all package management functions

;; A list of package repositories
;;; Code:
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
						 ("org"	  . "https://orgmode.org/elpa/")
						 ("elpa"  . "https://elpa.gnu.org/packages/")))

;; Constants.
(defconst --emacs-start-time (current-time))
(defconst --lisp-dir (concat user-emacs-directory "lisp/"))
(defconst --misc-dir (concat user-emacs-directory "misc/"))
(defconst --yas-dir (concat user-emacs-directory "snippets/"))
(defconst --themes-dir (concat user-emacs-directory "themes/"))
(defconst --user-cache-dir (concat user-emacs-directory "cache/"))
(defconst --auto-save-dir (concat user-emacs-directory "auto-save/"))

(package-initialize)				;; Initializes the package system and prepares it to be used
(unless package-archive-contents	 ; Unless a package archive already exists,
  (package-refresh-contents))		 ; Refresh package contents so that Emacs knows which packages to load

;; Initialize use-package on non-linux platforms
(unless (package-installed-p 'use-package)		  ; Unless "use-package" is installed, install "use-package"
  (package-install 'use-package))
(require 'use-package)							  ; Once it's installed, we load it using require
;; Make sure packages are downloaded and installed before they are run
;; also frees you from having to put :ensure t after installing EVERY PACKAGE.
(setq use-package-always-ensure t)
(setq lexical-binding t)

;; package update configuration
(use-package auto-package-update
  :defer 10
  :config
  ;; Delete residual old versions
  (setq auto-package-update-delete-old-versions t)
  ;; Do not bother me when updates have taken place.
  (setq auto-package-update-hide-results t)
  ;; Update installed packages at startup if there is an update pending.
  (auto-package-update-maybe))

;; Load server if not already running
(load "server")
(unless (server-running-p) (server-start))

;; Theme configuration
(use-package nord-theme :ensure t :config (setq nord-region-highlight "frost"))
(load-theme 'nord t)

;; Configure the Emacs Frame
(setq inhibit-startup-screen t)
(tool-bar-mode 0)
(blink-cursor-mode 0)
(tab-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(setq frame-title-format '("" "[%b] <%f> - Emacs " emacs-version))
(setq gc-cons-threshold 10000000)
(setq ring-bell-function 'ignore)
(setq initial-scratch-message nil)
(setq sentence-end-double-space nil)

(setq use-dialog-box nil)
(setq kill-do-not-save-duplicates t)
(setq help-window-select t)

(setq whitespace-line-column 264)

;;set the line number mode to true
(setq line-number-mode t)
(global-display-line-numbers-mode t)
(column-number-mode t)
(delete-selection-mode t)
(size-indication-mode t)
(global-visual-line-mode t)
(global-font-lock-mode t)
(global-hl-line-mode t)

(save-place-mode)

;; Parenthesis
(show-paren-mode 1)
(use-package smartparens :diminish :ensure t :config (smartparens-mode))
;; Automatically add ending brackets and braces
(electric-pair-mode 1)
;; Make return key also do indent in the current buffer
(electric-indent-local-mode 1)
;; make return key also do indent globally
(electric-indent-mode 1)

;; Save hooks
(add-hook 'before-save-hook 'lsp-format-buffer)
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
  :ensure t
  :config
  (unicode-fonts-setup))

;; Recent flie list
(use-package recentf)
(recentf-mode 1)
(setq recentf-max-saved-items 50)

;; Increase the maximum for eval depth
(setq max-lisp-eval-depth 10000)

;; Move to first whitespace or begninning of line if none. Pressing again goes to the beginning if
;; there was whitespace.
(global-set-key [remap move-beginning-of-line] 'smarter-move-beginning-of-line)
(global-set-key (kbd "M-SPC") 'cycle-spacing)

;; Make font bigger/smaller.
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
(global-set-key (kbd "C-0") 'text-scale-adjust)

;; Orderless
(use-package orderless :ensure t)
;; Set completion-styles
(setq completion-styles '(initials flex partial-completion basic orderless))

;; Windmove configuration
(windmove-default-keybindings 'meta)
(setq windmove-wrap-around t)

;; Revert the buffer automatically if it changes in the filesystem
(global-auto-revert-mode t)
(setq auto-revert-check-vc-info t)

;; Resume the previous session
(desktop-save-mode 1)
;; Smoother scrolling
(setq scroll-margin 0)
(setq scroll-conservatively 100000)
(setq scroll-preserve-screen-position 1)

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

;; Set fonts (for linux and windows)
(cond
 ((find-font (font-spec :name "Noto Sans Mono"))
  (set-face-attribute 'default nil :font "Noto Sans Mono-9.5"))
 ((find-font (font-spec :name "DejaVu Sans Mono"))
  (set-face-attribute 'default nil :font "DejaVu Sans Mono-9.5"))
 ((find-font (font-spec :name "Consolas"))
  (progn
	(set-face-attribute 'default nil :font "Consolas-10"))))

;; Try to fix the mode line
(use-package diminish :config (diminish 'visual-line-mode))

;; Making it easier to discover Emacs key presses.
(use-package which-key
  :diminish
  :defer 5
  :config (which-key-mode)
  (which-key-setup-side-window-bottom)
  (setq which-key-idle-delay 1.05))

;; Treesitter
(use-package tree-sitter :ensure t :diminish :config (global-tree-sitter-mode) :hook (tree-sitter-mode . tree-sitter-hl-mode))
(use-package tree-sitter-langs :ensure t :after tree-sitter)

;; Dim the inactive buffers
(use-package dimmer
  :custom (dimmer-fraction 0.2)
  :config (dimmer-mode))

;; paren matching with color
(use-package rainbow-delimiters :hook ((prog-mode . rainbow-delimiters-mode)))

(use-package htmlize :defer t)

;; Crux Configuration
(use-package crux)

;; Apheleia
(use-package apheleia :diminish :ensure t :config (apheleia-global-mode +1))

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
  :hook ((org-mode . visual-line-mode) (org-mode . pt/org-mode-hook) (org-mode . flyspell-mode))
  :hook ((org-src-mode . display-line-numbers-mode))
  :custom
  (org-adapt-indentation t)
  (org-special-ctrl-a/e t)
  (org-return-follows-link t)
  (org-src-ask-before-returning-to-edit-buffer nil "org-src is kinda needy out of the box")
  (org-src-window-setup 'current-window)
  (org-pretty-entities t)
  (org-startup-indented t)
  (org-list-description-max-indent 5)
  (org-indent-indentation-per-level 10)
  (org-todo-keywords '((sequence "TODO" "WORKING" "WAITING" "|" "DONE" "CANCELLED")))

  :config
  (defun pt/org-mode-hook ())
  (defun make-inserter (c) '(lambda () (interactive) (insert-char c)))
  (defun zero-width () (interactive) (insert "​")))

(use-package org-modern
  :config (global-org-modern-mode)
  :custom (org-modern-variable-pitch nil))

(use-package org-roam
  :ensure t
  :custom
  (org-roam-directory (file-truename "~/workspace/org-files/"))
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n g" . org-roam-graph)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n c" . org-roam-capture)
         ;; Dailies
         ("C-c n j" . org-roam-dailies-capture-today))
  :config
  ;; If you're using a vertical completion framework, you might want a more informative completion interface
  (setq org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
  (setq org-roam-mode-sections (list #'org-roam-backlinks-section
									 #'org-roam-reflinks-section
									 #'org-roam-unlinked-references-section))
  (org-roam-db-autosync-mode)
  ;; If using org-roam-protocol
  (require 'org-roam-protocol))

(use-package org-brain :ensure t
  :init
  (setq org-brain-path "~/workspace/org-brain/")
  :config
  (bind-key "C-c b" 'org-brain-prefix-map org-mode-map)
  (setq org-id-track-globally t)
  (setq org-id-locations-file "~/.emacs.d/.org-id-locations")
  (add-hook 'before-save-hook #'org-brain-ensure-ids-in-buffer)
  (push '("b" "Brain" plain (function org-brain-goto-end)
          "* %i%?" :empty-lines 1)
        org-capture-templates)
  (setq org-brain-visualize-default-choices 'all)
  (setq org-brain-title-max-length 12)
  (setq org-brain-include-file-entries nil
        org-brain-file-entries-use-title nil))

;; Allows you to edit entries directly from org-brain-visualize
(use-package polymode
  :config
  (add-hook 'org-brain-visualize-mode-hook #'org-brain-polymode))

(put 'erase-buffer 'disabled nil)

;; Fuzzy Search
(setq search-whitespace-regexp ".*")
(setq isearch-lax-whitespace t)
(setq isearch-regexp-lax-whitespace nil)
(setq isearch-lazy-count t)

(use-package eldoc :diminish :config (global-eldoc-mode))

;; RIPgrep and rg
(use-package ripgrep)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Custom splitting functions ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun vsplit-last-buffer ()
  (interactive)
  (split-window-vertically)
  (other-window 1 nil)
  (switch-to-next-buffer))
(defun hsplit-last-buffer ()
  (interactive)
  (split-window-horizontally)
  (other-window 1 nil)
  (switch-to-next-buffer))

(global-set-key (kbd "C-x 2") 'vsplit-last-buffer)
(global-set-key (kbd "C-x 3") 'hsplit-last-buffer)

;; All the Icons
(use-package all-the-icons :ensure t)

;; Magit configuration
(use-package magit
  :diminish magit-auto-revert-mode
  :diminish auto-revert-mode
  :bind (("C-x g" . #'magit-status))
  :config
  (defun pt/commit-hook () (set-fill-column 80))
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

;; CSV mode ;;
(use-package csv-mode :mode ("\\.csv$" . csv-mode))

;;; Json ;;;;
(use-package json-mode
  :mode ("\\.json$" . json-mode))

;; Rust
(use-package rustic :ensure t)

(use-package cargo-mode :ensure t)
(use-package cargo :hook (rust-mode . cargo-minor-mode)
  :bind ("C-c C-c C-n" . cargo-process-new))
(use-package flycheck-rust :ensure t :after flycheck)
(use-package racer
  :hook (rust-mode . racer-mode))

(use-package rust-playground :ensure t)
(use-package toml-mode :ensure t)

(add-hook 'rust-mode-hook #'racer-mode)
(add-hook 'racer-mode-hook #'eldoc-mode)

(add-hook 'flycheck-mode-hook #'flycheck-rust-setup)
(add-hook 'rustic-mode-hook #'lsp)
(add-hook 'rustic-mode-hook #'flycheck-mode)
(add-hook 'racer-mode-hook #'corfu-mode)


;; PHP
(use-package php-mode)

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

;;;; Corfu
(use-package corfu
  ;; Optional customizations
  :custom
  (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  (corfu-auto t)                 ;; Enable auto completion
  (corfu-auto-timer 0.5)
  (corfu-auto-prefix 1)
  (corfu-separator ?\s)          ;; Orderless field separator
  (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  ;; (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
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
  (global-corfu-mode))

;; Aggressive completion, cheap prefix filtering.
(setq-local corfu-auto t
            corfu-auto-delay 0
            corfu-auto-prefix 0)

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
  (setq tab-always-indent 'complete))

(use-package flycheck :config (global-flycheck-mode))

;; yasnippet configuration
(use-package yasnippet :diminish :config (yas-global-mode t))
(use-package yasnippet-snippets :diminish)

;;;;; Treemacs ;;;;;;
(use-package treemacs
  :ensure t
  :defer t)
(add-hook 'treemacs-mode-hook (lambda() (display-line-numbers-mode -1)))
(use-package treemacs-icons-dired :hook (dired-mode . treemacs-icons-dired-enable-once) :ensure t)
(add-hook 'dired-mode-hook 'treemacs-icons-dired-mode)

;; Git Info in Dired
(use-package dired-git-info :ensure t :after dired :commands (dired-git-info-mode))

;; Eglot
;;(use-package eglot :ensure t)
;;(use-package eglot-java :ensure t :after eglot)

;; lsp mode
(use-package lsp-mode
  :ensure t
  :init
  (setq lsp-keymap-prefix "C-c l"
		lsp-enable-file-watchers nil
		lsp-idle-delay 0.500)
  (defun my/lsp-mode-setup-completion()
	(setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults)) '(flex)))
  :hook
  ((lsp-mode . lsp-enable-which-key-integration)
   (java-mode . #'lsp-deferred))
  (lsp-completion-mode . my/lsp-mode-setup-completion)
  :custom
  ;;(lsp-completion-provider :none)
  (lsp-rust-analyzer-cargo-watch-command "clippy")
  (lsp-eldoc-render-all nil)
  (lsp-idle-delay 0.6)
  ;; enable / disable the hints as you prefer:
  (lsp-rust-analyzer-server-display-inlay-hints t)
  (lsp-rust-analyzer-display-lifetime-elision-hints-enable "skip_trivial")
  (lsp-rust-analyzer-display-chaining-hints t)
  (lsp-rust-analyzer-display-lifetime-elision-hints-use-parameter-names t)
  (lsp-rust-analyzer-display-closure-return-type-hints t)
  (lsp-rust-analyzer-display-parameter-hints t)
  (lsp-rust-analyzer-display-reborrow-hints t))

(use-package hydra)

(use-package lsp-ui
  :ensure t
  :after (lsp-mode)
  :bind (:map lsp-ui-mode-map
			  ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
			  ([remap xref-find-references] . lsp-ui-peek-find-references))
  :custom
  (lsp-ui-peek-always-show t)
  (lsp-ui-sideline-show-hover t)
  (lsp-ui-doc-enable nil))

(use-package lsp-java :ensure t :after lsp-mode)

;; Sonarlint
(use-package lsp-sonarlint
  :ensure t
  :after lsp-java)
;; (require 'lsp-sonarlint-java)
(setq lsp-sonarlint-java-enabled t)

;;(require 'lsp-sonarlint-php)
(setq lsp-sonarlint-php-enabled t)

;;(require 'lsp-sonarlint-html)
(setq lsp-sonarlint-html-enabled t)

;;(require 'lsp-sonarlint-javascript)
(setq lsp-sonarlint-javascript-enabled t)

;;(require 'lsp-sonarlint-typescript)
(setq lsp-sonarlint-typescript-enabled t)

;; ;; DAP mode for debugging
(use-package dap-mode
  :after (lsp-mode)
  :functions dap-hydra/nil
  :config
  (require 'dap-java)
  :bind (:map lsp-mode-map
			  ("C-c d" . dap-debug)
			  ("C-c D" . dap-hydra))
  :hook ((dap-mode . dap-ui-mode)
		 (dap-session-created . (lambda (&_rest) (dap-hydra)))
		 (dap-terminated . (lambda (&_rest) (dap-hydra/nil)))))
;;(use-package dap-java :ensure t)

;;;;; Vertico and Completion ;;;;;
(use-package prescient
  :config
  (setq prescient-save-file (concat user-emacs-directory "prescient-save"))
  (prescient-persist-mode +1))

(use-package marginalia
  :config	(marginalia-mode +1))

(use-package vertico :init (vertico-mode)
  (setq vertico-resize -1)
  (setq vertico-cycle t))

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
		 ("M-g f" . consult-flycheck)				;; Alternative: consult-flycheck
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
   consult-theme
   :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-recent-file
   consult--source-project-recent-file
   )

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

(use-package consult-flycheck :after consult)
(use-package consult-ls-git :after consult)
;;(use-package consult-eglot :after consult)
(use-package consult-lsp :after consult)
(use-package consult-yasnippet :diminish :after consult)

;; Configuration for embark
(use-package embark
  :bind
  (("C-." . embark-act)			;; pick some comfortable binding
   ("M-." . embark-dwim)		;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'
  :init
  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)
  :config
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

;;; Ansi colors in compilation buffer
(require 'xterm-color)
(setq compilation-environment '("TERM=xterm-256color"))
(defun my/advice-compilation-filter (f proc string)
  (funcall f proc (xterm-color-filter string)))
(advice-add 'compilation-filter :around #'my/advice-compilation-filter)

;; Java specific configuration
(setq lsp-enable-symbol-highlighting t)
(setq lsp-ui-doc-enable t)
(setq lsp-ui-doc-show-with-cursor t)
(setq lsp-ui-doc-show-with-mouse nil)
(setq lsp-ui-doc-delay 1.5)
(setq lsp-ui-doc-position 'bottom)
(setq lsp-ui-doc-max-width 132)
(setq lsp-lens-enable t)
(setq lsp-headerline-breadcrumb-enable t)
(setq lsp-ui-sideline-enable t)
(setq lsp-ui-sideline-show-code-actions nil)
(setq lsp-ui-sideline-show-hover t)
(setq lsp-modeline-code-actions-enable t)
(setq lsp-eldoc-enable-hover t)
(setq lsp-modeline-diagnostics-enable t)
(setq lsp-signature-auto-activate t) ;; you could manually request them via `lsp-signature-activate`
(setq lsp-signature-render-documentation t)
(setq lsp-completion-show-detail t)
(setq lsp-completion-show-kind t)
(setq lsp-java-format-settings-url "https://raw.githubusercontent.com/google/styleguide/gh-pages/eclipse-java-google-style.xml")
;;(setq lsp-java-format-settings-url "~/git/devenv/eclipse/eclipse-formatter-java.xml")
(setq lsp-java-format-settings-profile "GoogleStyle")

;;(add-hook 'java-mode-hook 'eglot-ensure)
;;(add-hook 'java-mode-hook 'eglot-java-mode)
(add-hook 'java-mode-hook #'lsp)
(add-hook 'java-mode-hook #'flycheck-mode)
(add-hook 'java-mode-hook #'corfu-mode)
;;(add-hook 'java-mode-hook #'lsp-java-boot-lens-mode)

;;(add-hook 'php-mode 'eglot-ensure)
;;(add-hook 'c-mode 'eglot-ensure)
;; (add-hook 'sh-mode 'eglot-ensure)
;; (add-hook 'shell-mode 'eglot-ensure)
;; (add-hook 'css-mode 'eglot-ensure)
;; (add-hook 'json-mode 'eglot-ensure)
;; (add-hook 'js-mode 'eglot-ensure)
;; (add-hook 'perl-mode 'eglot-ensure)
;; (add-hook 'python-mode 'eglot-ensure)
;; (add-hook 'yaml-mode 'eglot-ensure)

(add-hook 'css-mode-hook 'lsp-css)
(add-hook 'shell-mode-hook 'bash-ls)

;; Set Java VM for windows
(when (eq system-type 'windows-nt)
  (setenv "JAVA_HOME" "C:\\Users\\leherv\\.jdks\\openjdk-18.0.1.1\\")
  (setq lsp-java-java-path "C:\\Users\\leherv\\.jdks\\openjdk-18.0.1.1\\bin\\java"))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(connection-local-criteria-alist
	'(((:application tramp)
	   tramp-connection-local-default-system-profile tramp-connection-local-default-shell-profile)))
 '(connection-local-profile-alist
	'((tramp-connection-local-darwin-ps-profile
	   (tramp-process-attributes-ps-args "-acxww" "-o" "pid,uid,user,gid,comm=abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ" "-o" "state=abcde" "-o" "ppid,pgid,sess,tty,tpgid,minflt,majflt,time,pri,nice,vsz,rss,etime,pcpu,pmem,args")
	   (tramp-process-attributes-ps-format
		(pid . number)
		(euid . number)
		(user . string)
		(egid . number)
		(comm . 52)
		(state . 5)
		(ppid . number)
		(pgrp . number)
		(sess . number)
		(ttname . string)
		(tpgid . number)
		(minflt . number)
		(majflt . number)
		(time . tramp-ps-time)
		(pri . number)
		(nice . number)
		(vsize . number)
		(rss . number)
		(etime . tramp-ps-time)
		(pcpu . number)
		(pmem . number)
		(args)))
	  (tramp-connection-local-busybox-ps-profile
	   (tramp-process-attributes-ps-args "-o" "pid,user,group,comm=abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ" "-o" "stat=abcde" "-o" "ppid,pgid,tty,time,nice,etime,args")
	   (tramp-process-attributes-ps-format
		(pid . number)
		(user . string)
		(group . string)
		(comm . 52)
		(state . 5)
		(ppid . number)
		(pgrp . number)
		(ttname . string)
		(time . tramp-ps-time)
		(nice . number)
		(etime . tramp-ps-time)
		(args)))
	  (tramp-connection-local-bsd-ps-profile
	   (tramp-process-attributes-ps-args "-acxww" "-o" "pid,euid,user,egid,egroup,comm=abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ" "-o" "state,ppid,pgid,sid,tty,tpgid,minflt,majflt,time,pri,nice,vsz,rss,etimes,pcpu,pmem,args")
	   (tramp-process-attributes-ps-format
		(pid . number)
		(euid . number)
		(user . string)
		(egid . number)
		(group . string)
		(comm . 52)
		(state . string)
		(ppid . number)
		(pgrp . number)
		(sess . number)
		(ttname . string)
		(tpgid . number)
		(minflt . number)
		(majflt . number)
		(time . tramp-ps-time)
		(pri . number)
		(nice . number)
		(vsize . number)
		(rss . number)
		(etime . number)
		(pcpu . number)
		(pmem . number)
		(args)))
	  (tramp-connection-local-default-shell-profile
	   (shell-file-name . "/bin/bash")
	   (shell-command-switch . "-c"))
	  (tramp-connection-local-default-system-profile
	   (path-separator . ":")
	   (null-device . "/dev/null"))))
 '(custom-safe-themes
	'("3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" default))
 '(package-selected-packages
	'(polymode org-brain org-roam smart-mode-line smart-mode-line-atom-one-dark-theme smart-mode-line-powerline-theme mood-line orderless lsp-java cargo auto-package-update org-modern marginalia racer nord-theme corfu dired-git-info prescient dimmer consult-ls-git vertico json-mode ripgrep consult-flycheck treemacs-icons-dired rainbow-delimiters consult-yasnippet diff-hl unicode-fonts diminish apheleia consult-lsp flycheck-rust all-the-icons crux embark-consult rust-playground magit htmlize lsp-sonarlint lsp-ui php-mode toml-mode tree-sitter-langs csv-mode rustic which-key yasnippet-snippets smartparens cargo-mode)))

(provide 'init)
;;; init.el ends here
