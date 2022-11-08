;;; init.el --- Emacs Configuration All
;; -*- coding: utf-8; lexical-binding: t -*-

;;; Commentary:
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
;; Make return key also do indent in the current buffer
(electric-indent-local-mode 1)
;; make return key also do indent globally
(electric-indent-mode 1)
(tab-bar-mode -1)
(setq whitespace-line-column 264)

;; Save hooks
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(add-hook 'after-save-hook 'read-only-mode)
(global-set-key (kbd "C-i") 'read-only-mode)
(setq-default view-read-only t)
(setq-default buffer-read-only t)

;; Tabs vs Spaces
;; Indent always uses spaces and never tabs
(setq-default indent-tabs-mode nil)
;; Set the default tab size to 4
(setq-default tab-width 4)
;; Tab key will always indent command
(setq-default tab-always-indent t)
(setq indent-line-function 'insert-tab)

;; Save History
(setq history-length 25)
(savehist-mode 1)

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

;; Revert the buffer automatically if it changes in the filesystem
(global-auto-revert-mode 1)

;; Resume the previous session
(desktop-save-mode 1)
;; Smoother scrolling
(setq scroll-margin 0)
(setq scroll-conservatively 100000)
(setq scroll-preserve-screen-position 1)

;; Put backup files neatly away
(let ((backup-dir (concat user-emacs-directory  "/backups"))
      (auto-saves-dir (concat user-emacs-directory "/auto-saves")))
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

;; Set fonts (for linux and windows)
(cond
 ((find-font (font-spec :name "DejaVu Sans Mono"))
  (set-face-attribute 'default nil :font "DejaVu Sans Mono-10.0")
  (set-face-attribute 'mode-line nil :font "DejaVu Sans Mono-8.0"))
 ((find-font (font-spec :name "Noto Sans Mono"))
  (set-face-attribute 'default nil :font "Noto Sans Mono-10.0")
  (set-face-attribute 'mode-line nil :font "Noto Sans Mono-8.0"))
 ((find-font (font-spec :name "Calibri"))
  (progn
    (set-face-attribute 'default nil :font "Consolas-10")
    (set-face-attribute 'mode-line nil :font "Consolas-9"))))

;; Try to fix the mode line
(use-package diminish :config (diminish 'visual-line-mode))

;; Making it easier to discover Emacs key presses.
(use-package which-key
  :diminish
  :defer 5
  :config (which-key-mode)
  (which-key-setup-side-window-bottom)
  (setq which-key-idle-delay 1.05))

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

(put 'erase-buffer 'disabled nil)

;; Fuzzy Search
(setq search-whitespace-regexp ".*")
(setq isearch-lax-whitespace t)
(setq isearch-regexp-lax-whitespace nil)

;; Multiple Cursor Support
(use-package multiple-cursors
  :bind (("C-c C-e m" . #'mc/edit-lines)
	     ("C-c C-e d" . #'mc/mark-all-dwim)))

;; Whitespace configuration
(use-package whitespace
  :diminish whitespace-mode
  :init
  (add-hook 'prog-mode-hook 'whitespace-mode))

(use-package eldoc :diminish :config (global-eldoc-mode))

;; RIPgrep and rg
(use-package ripgrep)

;; Theme configuration
(use-package nord-theme)
(load-theme 'nord t)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(region ((t (:extend t :background "#000000")))))

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
  :diminish
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
  :diminish
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
  :diminish
  :config
  (company-flx-mode +1))

;;;; Projectile ;;;;;
(use-package projectile
  :config
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (projectile-mode +1))
(setq projectile-indexing-method 'alien)

(use-package flycheck :config (global-flycheck-mode))

;; yasnippet configuration
(use-package yasnippet :diminish :config (yas-global-mode) :custom (yas-prompt-functions '(yas-completing-prompt)))
(use-package yasnippet-snippets :diminish)

;;;;; Treemacs ;;;;;;
(use-package treemacs
  :ensure t
  :defer t
  )

(use-package treemacs-projectile :after (treemacs projectile) :ensure t)
(use-package treemacs-icons-dired :hook (dired-mode . treemacs-icons-dired-enable-once) :ensure t)
(add-hook 'dired-mode-hook 'treemacs-icons-dired-mode)

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

(use-package lsp-java)

;; Sonarlint
(use-package lsp-sonarlint
  :after lsp-java)
(setq lsp-sonarlint-java-enabled t)

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
	     ("M-g f" . consult-flycheck)               ;; Alternative: consult-flycheck
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
  (setq consult-preview-key 'any)
  (setq consult-preview-key (kbd "M-."))
  (setq consult-preview-key (list (kbd "<S-down>") (kbd "<S-up>")))
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
  (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)

  ;; By default `consult-project-function' uses `project-root' from project.el.
  ;; Optionally configure a different project root function.
  ;; There are multiple reasonable alternatives to chose from.
  ;;;; 1. project.el (the default)
  ;; (setq consult-project-function #'consult--default-project--function)
  ;;;; 2. projectile.el (projectile-project-root)
  (autoload 'projectile-project-root "projectile")
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

;; Java specific configuration
(setq lsp-enable-symbol-highlighting t)
(setq lsp-ui-doc-enable t)
(setq lsp-ui-doc-show-with-cursor t)
(setq lsp-ui-doc-show-with-mouse t)
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

(add-hook 'java-mode-hook #'lsp)
(add-hook 'java-mode-hook 'flycheck-mode)
(add-hook 'java-mode-hook 'company-mode)
(add-hook 'after-init-hook 'global-company-mode)

;; Set Java VM for windows
(when (eq system-type 'windows-nt)
  (setenv "JAVA_HOME" "C:\\Users\\leherv\\.jdks\\openjdk-18.0.1.1\\")
  (setq lsp-java-java-path "C:\\Users\\leherv\\.jdks\\openjdk-18.0.1.1\\bin\\java"))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(unicode-fonts lsp-sonarlint-java lsp-sonarlint csv csv-mode treemacs-projectile yasnippet-snippets which-key use-package treemacs-magit treemacs-icons-dired treemacs-all-the-icons selectrum-prescient ripgrep rg rainbow-delimiters projectile org-modern org-bullets org-alert orderless nord-theme multiple-cursors mood-line marginalia magit-libgit lsp-ui lsp-java json-mode htmlize git-gutter-fringe flycheck embark-consult easy-kill dimmer diminish diff-hl crux company-php company-org-block company-fuzzy company-flx company-box auto-package-update all-the-icons-ibuffer all-the-icons-dired all-the-icons-completion aggressive-indent))
 '(warning-suppress-log-types
   '((comp)
     ((undo discard-info))
     (emacs)))
 '(warning-suppress-types
   '(((undo discard-info))
     (emacs))))

(provide 'init)
;;; init.el ends here
