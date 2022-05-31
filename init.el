;;; init.el --- Emacs Configuration All
;; -*- coding: utf-8; lexical-binding: t -*-

;;; Commentary:
;;
(require 'package)                   ; Bring in to the environment all package management functions

;; A list of package repositories
;;; Code:
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
			 ("org"   . "https://orgmode.org/elpa/")
			 ("elpa"  . "https://elpa.gnu.org/packages/")))

;; Constants.
(defconst --emacs-start-time (current-time))
(defconst --lisp-dir (concat user-emacs-directory "lisp/"))
(defconst --misc-dir (concat user-emacs-directory "misc/"))
(defconst --yas-dir (concat user-emacs-directory "snippets/"))
(defconst --themes-dir (concat user-emacs-directory "themes/"))
(defconst --user-cache-dir (concat user-emacs-directory "cache/"))
(defconst --auto-save-dir (concat user-emacs-directory "auto-save/"))

(package-initialize)                ;; Initializes the package system and prepares it to be used
(unless package-archive-contents     ; Unless a package archive already exists,
  (package-refresh-contents))        ; Refresh package contents so that Emacs knows which packages to load

;; Initialize use-package on non-linux platforms
(unless (package-installed-p 'use-package)        ; Unless "use-package" is installed, install "use-package"
  (package-install 'use-package))
(require 'use-package)                            ; Once it's installed, we load it using require
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

;; Configure the Emacs Frame
(setq inhibit-startup-screen t)
(tool-bar-mode 0)
(blink-cursor-mode 0)
(menu-bar-mode -1)
(setq frame-title-format '("" "[%b] <%f> - Emacs " emacs-version))
(setq gc-cons-threshold 10000000)
(setq ring-bell-function 'ignore)
(setq initial-scratch-message nil)
(setq sentence-end-double-space nil)
(put 'scroll-left 'disabled nil)
(setq use-dialog-box nil)
(setq kill-do-not-save-duplicates t)
(setq help-window-select t)

;; set everything to UTF8
(set-charset-priority 'unicode)
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(setq default-process-coding-system '(utf-8-unix . utf-8-unix))

;;set the line number mode to true
(setq line-number-mode t)
(global-display-line-numbers-mode t)
(column-number-mode t)
(delete-selection-mode t)
(size-indication-mode t)
(global-visual-line-mode t)
(global-font-lock-mode t)
(global-hl-line-mode t)
(show-paren-mode 1)
(save-place-mode)
;; Automatically add ending brackets and braces
(electric-pair-mode 1)
(tab-bar-mode -1)
(setq whitespace-line-column 264)

;; Recent flie list
(use-package recentf)
(recentf-mode 1)
(setq recentf-max-saved-items 50)

;; Increase the maximum for eval depth
(setq max-lisp-eval-depth 10000)

;; Move to first whitespace or begninning of line if none. Pressing again goes to the beginning if
;; there was whitespace.
(global-set-key [remap move-beginning-of-line] 'smarter-move-beginning-of-line)

;; Cycle through "just one space", "no spaces" and original number of spaces,
;; instead of just "just one space". It does not delete newlines either
(global-set-key (kbd "M-SPC")
		'(lambda () (interactive) (cycle-spacing +1 t)))

;; Make font bigger/smaller.
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
(global-set-key (kbd "C-0") 'text-scale-adjust)

;; Revert the buffer automatically if it changes in the filesystem
(global-auto-revert-mode 1)
;; Resume the previous session
(desktop-save-mode 1)
;; Smoother scrolling
(setq scroll-margin 0)
(setq scroll-conservatively 100000)
(setq scroll-preserve-screen-position 1)

;; See if we can maximize by default
(custom-set-variables
 '(initial-frame-alist (quote ((fullscreen . maximized)))))

;; Put backup files neatly away
(let ((backup-dir "~/emacs.d/backups")
      (auto-saves-dir "~/emacs.d/auto-saves/"))
  (dolist (dir (list backup-dir auto-saves-dir))
    (when (not (file-directory-p dir))
      (make-directory dir t)))
  (setq backup-directory-alist `(("." . ,backup-dir))
	auto-save-file-name-transforms `((".*" ,auto-saves-dir t))
	auto-save-list-file-prefix (concat auto-saves-dir ".saves-")))

(setq backup-by-copying t    ; Don't delink hardlinks
      delete-old-versions t  ; Clean up the backups
      version-control t      ; Use version numbers on backups,
      kept-new-versions 5    ; keep some new versions
      kept-old-versions 2)   ; and some old ones, too

;; Cleanup whitespace before saving
(add-hook 'before-save-hook 'whitespace-cleanup)

;; Set fonts (for linux and windows)
(cond
 ((find-font (font-spec :name "DejaVu Sans"))
  (set-face-attribute 'default nil :font "DejaVu Sans-10.0"))
 ((find-font (font-spec :name "Noto Sans"))
  (set-face-attribute 'default nil :font "Noto Sans-10.0"))
 ((find-font (font-spec :name "Calibri-12"))
  (progn
    (set-face-attribute 'default nil :font "Calibria-12")
    (set-face-attribute 'mode-line nil :font "Calibri-9"))))

;; Try to fix the mode line
(use-package diminish :config (diminish 'visual-line-mode))
(use-package mood-line :config (mood-line-mode))

;; Making it easier to discover Emacs key presses.
(use-package which-key
  :diminish
  :defer 5
  :config (which-key-mode)
  (which-key-setup-side-window-bottom)
  (setq which-key-idle-delay 1.05))

;; Better fonts
(use-package all-the-icons)
(use-package all-the-icons-dired
  :after all-the-icons
  :hook (dired-mode . all-the-icons-dired-mode))
(use-package all-the-icons-ibuffer :hook (ibuffer-mode . all-the-icons-ibuffer-mode))
(use-package all-the-icons-completion
  :config
  (all-the-icons-completion-mode)
  (add-hook 'marginalia-mode-hook #'all-the-icons-completion-marginalia-setup))

;; Dim the inactive buffers
(use-package dimmer
  :custom (dimmer-fraction 0.31)
  :config (dimmer-mode))

;; paren matching with color
(use-package rainbow-delimiters :hook ((prog-mode . rainbow-delimiters-mode)))

(use-package htmlize :defer t)

;; Crux Configuration
(use-package crux)

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
  :bind (("C-c o c" . org-capture)
	 ("C-c o a" . org-agenda)
	 ("C-c o A" . consult-org-agenda)
	 :map org-mode-map
	 ("M-<left>" . nil)
	 ("M-<right>" . nil)
	 ("C-c c" . #'org-mode-insert-code)
	 ("C-c a f" . #'org-shifttab)
	 ("C-c a S" . #'zero-width))
  :custom
  (org-adapt-indentation t)
  (org-special-ctrl-a/e t)
  (org-return-follows-link t)
  (org-src-ask-before-returning-to-edit-buffer nil "org-src is kinda needy out of the box")
  (org-src-window-setup 'current-window)
  (org-pretty-entities t)
  (org-startup-indented t)
  (org-list-description-max-indent 5)
  (whitespace-style '(face tabs empty trailing))

  :config
  (defun pt/org-mode-hook ())
  (defun make-inserter (c) '(lambda () (interactive) (insert-char c)))
  (defun zero-width () (interactive) (insert "​"))

  (defun org-mode-insert-code ()
    "Like markdown-insert-code, but for org instead."
    (interactive)
    (org-emphasize ?~)))

(use-package org-modern
  :config (global-org-modern-mode)
  :custom (org-modern-variable-pitch nil))

(use-package org-bullets :hook (org-mode . org-bullets-mode))

;;Easy kill setup and configuration
;; (use-package easy-kill
;;   :config
;;   (global-set-key [remap kill-ring-save] #'easy-kill)
;;   (global-set-key [remap mark-sexp] #'easy-mark))

(put 'erase-buffer 'disabled nil)

;; Multiple Cursor Support
(use-package multiple-cursors
  :bind (("C-c C-e m" . #'mc/edit-lines)
	 ("C-c C-e d" . #'mc/mark-all-dwim)))

;;Adding flyspell for spell checking.
;; (use-package flyspell
;;   :defer t
;;   :init
;;   (progn
;;     (add-hook 'prog-mode-hook 'flyspell-prog-mode)
;;     (add-hook 'text-mode-hook 'flyspell-mode)
;;     (add-hook 'emacs-lisp-mode-hook 'flyspell-mode)
;;     (add-hook 'lisp-interaction-mode-hook 'flyspell-mode)
;;     )
;;   :config
;;   ;; Sets flyspell correction to use two-finger mouse click
;;   (define-key flyspell-mouse-map [down-mouse-3] #'flyspell-correct-word)
;;   )

;; Whitespace configuration
(use-package whitespace
  :diminish whitespace-mode
  :init
  (add-hook 'prog-mode-hook 'whitespace-mode))

;; Theme configuration
(add-to-list 'custom-theme-load-path (expand-file-name --themes-dir))
(load-theme 'nord t)

;; Org2Blog configuration
;; (use-package xml-rpc
;;   :defer t)
;; (use-package netrc
;;   :defer t)

;; (setq org2blog/wp-show-post-in-browser t)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("0710b0bdd59c8a7aacf0640591b38fcad5978a0fcfff3fdd999e63499ada8e3e" "dbade2e946597b9cda3e61978b5fcc14fa3afa2d3c4391d477bdaeff8f5638c5" "801a567c87755fe65d0484cb2bded31a4c5bb24fd1fe0ed11e6c02254017acb2" "6bffac6f528e43839861be1d7facf8054b57edc1ffc70f7be885da7d181ecbac" "37768a79b479684b0756dec7c0fc7652082910c37d8863c35b702db3f16000f8" "549ccbd11c125a4e671a1e8d3609063a91228e918ffb269e57bd2cd2c0a6f1c6" default))
 '(fido-mode t)
 '(org-agenda-files nil nil nil "Customized with use-package org")
 '(package-selected-packages
   '(diff-hl magit-libgit libgit multiple-cursors org-alert org-modern rainbow-delimiters mood-line diminish git-gutter-fringe company-fuzzy company-org-block company-php treemacs-all-the-icons treemacs-icons-dired treemacs-magit marginalia prescient orderless lsp-java lsp-ui hydra lsp-mode projectile company-flx company-box company markdown-mode php-mode json-mode selectrum-prescient selectrum maven-test-mode javadoc-lookup mvn helm-lsp yasnippet-snippets yasnippet flycheck nordless-theme csv-mode magithub tao-theme dracula-theme git-gutter org-bullets magit dimmer all-the-icons-dired all-the-icons which-key auto-package-update log4j-mode ace-window aggressive-indent easy-kill use-package org2blog nord-theme))
 '(size-indication-mode t))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;Aggressive indent mode
(use-package aggressive-indent
  :hook
  (c-mode . aggressive-indent-mode)
  (emacs-lisp-mode . aggressive-indent-mode)
  (java-mode . aggressive-indent-mode))

;; Window switching configuration ;; Ace Window
(use-package ace-window
  :config
  (global-set-key (kbd "<C-tab>") 'ace-window)
  (setq aw-keys '(?e ?t ?a ?h ?i ?s ?w ?n ?p ?c)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Custom splitting functions ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun vsplit-last-buffer ()
  (interactive)
  (split-window-vertically)
  (other-window 1 nil)
  (switch-to-next-buffer)
  )
(defun hsplit-last-buffer ()
  (interactive)
  (split-window-horizontally)
  (other-window 1 nil)
  (switch-to-next-buffer)
  )

(global-set-key (kbd "C-x 2") 'vsplit-last-buffer)
(global-set-key (kbd "C-x 3") 'hsplit-last-buffer)

;; Magit configuration
(use-package magit
  :diminish magit-auto-revert-mode
  :diminish auto-revert-mode
  :bind (("C-x g" . #'magit-status))
  :config
  (defun pt/commit-hook () (set-fill-column 80))
  (add-hook 'git-commit-setup-hook #'pt/commit-hook)
  (add-to-list 'magit-no-confirm 'stage-all-changes))

(use-package libgit :after magit)

(use-package magit-libgit
  :after (magit libgit))

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

;; Java specific configuration
(add-hook 'java-mode-hook #'lsp)
(add-hook 'java-mode-hook 'flycheck-mode)
(add-hook 'java-mode-hook 'company-mode)
(add-hook 'after-init-hook 'global-company-mode)

;;; Json ;;;;
(use-package json-mode
  :mode ("\\.json$" . json-mode))

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

;;;; Company ;;;;
(use-package company
  :config
  (setq company-idle-delay 0.3
	company-minimum-prefix-length 1
	company-require-match nil
	company-selection-wrap-around t
	company-tooltip-align-annotations t)
  (global-company-mode 1)
  (global-set-key (kbd "C-,") 'company-complete))

;; Show icons in company completion UI.
(use-package company-box
  :config
  (if (display-graphic-p)
      ;; Show font icons in windowed mode.
      (setq company-box-icons-alist 'company-box-icons-all-the-icons
	    company-box-color-icon t)
    ;; Show compatible icons in terminal.
    (setq company-box-icons-alist 'company-box-icons-icons-in-terminal))
  :hook (company-mode . company-box-mode))

(use-package company-flx
  :requires company
  :config
  (company-flx-mode +1))

;;;; Projectile ;;;;;
(use-package projectile
  :config
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (projectile-mode +1))
(setq projectile-indexing-method 'alien)

(use-package flycheck :init (global-flycheck-mode))

;; yasnippet configuration
(use-package yasnippet :config (yas-global-mode) :custom (yas-prompt-functions '(yas-completing-prompt)))
(use-package yasnippet-snippets )

;; lsp mode
(use-package lsp-mode
  :hook (
	 (lsp-mode . lsp-enable-which-key-integration)
	 (java-mode . #'lsp-deferred)
	 )
  :init (setq
	 lsp-keymap-prefix "C-c l"              ; this is for which-key integration documentation, need to use lsp-mode-map
	 lsp-enable-file-watchers nil
	 read-process-output-max (* 1024 1024)  ; 1 mb
	 lsp-idle-delay 0.500
	 )
  :config
  (with-eval-after-load 'lsp-intelephense
    (setf (lsp--client-multi-root (gethash 'iph lsp-clients)) nil))
  (define-key lsp-mode-map (kbd "C-c l") lsp-command-map)
  )
(use-package hydra)
(use-package lsp-ui

  :after (lsp-mode)
  :bind (:map lsp-ui-mode-map
	      ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
	      ([remap xref-find-references] . lsp-ui-peek-find-references))
  :init (setq lsp-ui-doc-delay 1.5
	      lsp-ui-doc-position 'bottom
	      lsp-ui-doc-max-width 100
	      ))

(use-package lsp-java  :config (add-hook 'java-mode-hook 'lsp))

;; DAP mode for debugging
(use-package dap-mode

  :after (lsp-mode)
  :functions dap-hydra/nil
  :config
  (require 'dap-java)
  :bind (:map lsp-mode-map
	      ("<f5>" . dap-debug)
	      ("M-<f5>" . dap-hydra))
  :hook ((dap-mode . dap-ui-mode)
	 (dap-session-created . (lambda (&_rest) (dap-hydra)))
	 (dap-terminated . (lambda (&_rest) (dap-hydra/nil)))))
(use-package dap-java :ensure nil)

;;;;; Selectrum ;;;;;
(use-package orderless
  :config
  (setq completion-styles '(orderless basic)))

(use-package prescient
  :config
  (setq prescient-save-file (concat user-emacs-directory "prescient-save"))
  (prescient-persist-mode +1))

(use-package marginalia
  :config   (marginalia-mode +1))

(use-package selectrum
  :requires orderless
  :config
  (setq selectrum-refine-candidates-function #'orderless-filter
	selectrum-highlight-candidates-function #'orderless-highlight-matches
	selectrum-count-style 'current/matches
	selectrum-max-window-height 15)
  (selectrum-mode +1))

(use-package selectrum-prescient
  :requires selectrum prescient
  :config
  ;; Use filtring from only `completion-styles' and not selectrum.
  (setq selectrum-prescient-enable-filtering nil)

  ;; But enable frequency and recency ordering from selectrum.
  (selectrum-prescient-mode +1))

(global-set-key (kbd "C-x C-.") #'selectrum-repeat)

;; Configuration for Consult
(use-package consult
  ;; Replace bindings. Lazily loaded due by `use-package'.
  :bind (;; C-c bindings (mode-specific-map)
	 ("C-c h" . consult-history)
	 ("C-c m" . consult-mode-command)
	 ("C-c k" . consult-kmacro)
	 ;; C-x bindings (ctl-x-map)
	 ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
	 ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
	 ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
	 ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
	 ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
	 ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
	 ;; Custom M-# bindings for fast register access
	 ("M-#" . consult-register-load)
	 ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
	 ("C-M-#" . consult-register)
	 ;; Other custom bindings
	 ("M-y" . consult-yank-pop)                ;; orig. yank-pop
	 ("<help> a" . consult-apropos)            ;; orig. apropos-command
	 ;; M-g bindings (goto-map)
	 ("M-g e" . consult-compile-error)
	 ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
	 ("M-g g" . consult-goto-line)             ;; orig. goto-line
	 ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
	 ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
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
	 ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
	 ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
	 ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
	 ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
	 ;; Minibuffer history
	 :map minibuffer-local-map
	 ("M-s" . consult-history)                 ;; orig. next-matching-history-element
	 ("M-r" . consult-history))                ;; orig. previous-matching-history-element

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
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key (kbd "M-."))
  ;; (setq consult-preview-key (list (kbd "<S-down>") (kbd "<S-up>")))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-theme
   :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-recent-file
   consult--source-project-recent-file
   :preview-key (kbd "M-."))

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; (kbd "C-+")

  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  ;; (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)

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

;; Configuration for embark
(use-package embark
  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("C-;" . embark-dwim)        ;; good alternative: M-.
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

;;;;; Hydra ;;;;;
;; (use-package hydra
;;   :config
;;   ;; Easier cycling of yanking.
;;   (defhydra yank-pop-hydra ()
;;     "yank"
;;     ("C-y" yank nil)
;;     ("M-y" yank-pop nil)
;;     ("y" (yank-pop 1) "next")
;;     ("Y" (yank-pop -1) "prev"))

;;   (global-set-key (kbd "M-y") #'yank-pop-hydra/yank-pop)
;;   (global-set-key (kbd "C-y") #'yank-pop-hydra/yank)

;;   (defhydra compilation-hydra (:columns 4)
;;     ("c" compile "Compile")
;;     ("C" compile-from-buffer-folder "Compile from buffer folder")
;;     ("r" recompile "Recompile")
;;     ("k" netrom/kill-compilation "Stop")
;;     ("n" next-error "Next error")
;;     ("N" next-error-skip-warnings "Next error, skip warnings")
;;     ("p" previous-error "Previous error")
;;     ("f" first-error "First error")
;;     ("l" netrom/compilation-last-error "Last error")
;;     ("s" netrom/compilation-toggle-scroll "Toggle scroll")
;;     ("t" netrom/compilation-toggle-threshold "Toggle threshold")
;;     ("q" nil "Cancel" :color blue))

;;   (global-set-key [(f5)] 'compilation-hydra/body)

;;   ;; Define hydra for programming modes.
;;   (add-hook 'prog-mode-hook
;;             (lambda ()
;;               ;; Using local-set-key because defining the bindings in prog-mode-map will get
;;               ;; overridden by c++-mode bindings, for instance. This shadows them instead.
;;               (when (member major-mode '(c++-mode c-mode))
;;                 (local-set-key (kbd "C-c C-c") 'compilation-hydra/body)))))


;;treemacs
(use-package lsp-treemacs
  :after (lsp-mode treemacs)

  :commands lsp-treemacs-errors-list
  :bind (:map lsp-mode-map
	      ("C-[" . lsp-treemacs-errors-list)))

(use-package treemacs
  :commands (treemacs)
  :after (lsp-mode))

(use-package treemacs-icons-dired
  :hook (dired-mode . treemacs-icons-dired-enable-once))

(when (eq system-type 'windows-nt)
  (setenv "JAVA_HOME" "~/.jdks/openjdk-17.0.1")
  (setq lsp-java-java-path "~/.jdks/openjdk-17.0.1/bin/java"))

;; Set Windows-specific preferences if running in a Windows environment.
;; (defun udf-windows-setup () (interactive)
;;   ;; The variable `git-shell-path' contains the path to the `Git\bin'
;;   (setq git-shell-path ("C:\\Program Files\\Git\\bin"))
;;   (setq git-shell-executable (concat git-shell-path "\\bash.exe"))
;;   (add-to-list 'exec-path git-shell-path)
;;   (setenv "PATH" (concat git-shell-path ";" (getenv "PATH"))))

;; (if (eq system-type 'windows-nt)
;;     (udf-windows-setup))

(provide 'init)
;;; init.el ends here
