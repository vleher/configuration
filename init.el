;;; Emacs Configuration --- all

(require 'package)                   ; Bring in to the environment all package management functions

;; A list of package repositories
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

;; Create necessary directories if missing.
;;(mkdir --user-cache-dir)
;;(mkdir --auto-save-dir)

(package-initialize)                 ; Initializes the package system and prepares it to be used
(unless package-archive-contents     ; Unless a package archive already exists,
  (package-refresh-contents))        ; Refresh package contents so that Emacs knows which packages to load

;; Initialize use-package on non-linux platforms
(unless (package-installed-p 'use-package)        ; Unless "use-package" is installed, install "use-package"
  (package-install 'use-package))
(require 'use-package)                            ; Once it's installed, we load it using require
(setq use-package-always-ensure t)
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

;; set everything to UTF8
(set-charset-priority 'unicode)
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(setq default-process-coding-system '(utf-8-unix . utf-8-unix))

;; Move to first whitespace or begninning of line if none. Pressing again goes to the beginning if
;; there was whitespace.
(global-set-key [remap move-beginning-of-line] 'smarter-move-beginning-of-line)

;; Cycle through "just one space", "no spaces" and original number of spaces,
;; instead of just "just one space". It does not delete newlines, too.
(global-set-key (kbd "M-SPC")
                '(lambda () (interactive) (cycle-spacing +1 t)))

;; Make font bigger/smaller.
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
(global-set-key (kbd "C-0") 'text-scale-adjust)

;; Make sure packages are downloaded and installed before they are run
;; also frees you from having to put :ensure t after installing EVERY PACKAGE.
(setq use-package-always-ensure t)
(setq lexical-binding t)

;; Revert the buffer automatically if it changes in the filesystem
(global-auto-revert-mode 1)
;; Resume the previous session
(desktop-save-mode 1)
;; Smoother scrolling
(setq scroll-margin 0)
(setq scroll-conservatively 100000)
(setq scroll-preserve-screen-position 1)

;; All the backups go to the custom folder
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))

;; I want spaces for indentation
(setq-default indent-tabs-mode nil)
(setq-default tab-bar-mode t)
(setq-default whitespace-line-column 264)

;; Cleanup whitespace before saving
(add-hook 'before-save-hook 'whitespace-cleanup)

(set-face-attribute 'default t :font "Noto Sans-10.5")
(set-face-attribute 'default nil :font "Noto Sans-10.5")

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

;; Dim the inactive buffers
(use-package dimmer
  :custom (dimmer-fraction 0.31)
  :config (dimmer-mode))

;; Org Mode Customization
;; set maximum indentation for description lists
(setq org-list-description-max-indent 5)
;;Disable highlighting long lines
(setq whitespace-style '(face tabs empty trailing))
;; prevent demoting heading also shifting text inside sections
(setq org-adapt-indentation t)
(add-hook 'org-mode-hook 'org-indent-mode)

;; Efficient version control.
;;
;; Bottom of Emacs will show what branch you're on
;; and whether the local file is modified or not.

(use-package htmlize :defer t)
;; Main use: Org produced htmls are coloured.
;; Can be used to export a file into a coloured html.

;; Get org-headers to look pretty! E.g., * → ⊙, ** ↦ ◯, *** ↦ ★
;; https://github.com/emacsorphanage/org-bullets
(use-package org-bullets
  :hook (org-mode . org-bullets-mode))

;;set the line number mode to true
(setq line-number-mode t)
(global-display-line-numbers-mode t)
(column-number-mode 1)
(delete-selection-mode t)
(size-indication-mode 1)
(global-visual-line-mode 1)
(global-hl-line-mode 1)
(show-paren-mode 1)

(global-auto-revert-mode 1)
(menu-bar-mode -1)
(tool-bar-mode -1)
(tab-bar-mode -1)

(recentf-mode 1)
(setq recentf-max-saved-items 50)

;; Fido mode and configuration
(fido-mode t)

;;Easy kill setup and configuration
(use-package easy-kill
  :ensure t
  :config
  (global-set-key [remap kill-ring-save] #'easy-kill)
  (global-set-key [remap mark-sexp] #'easy-mark))

(put 'erase-buffer 'disabled nil)
;;Adding flyspell for spell checking.
(use-package flyspell
  :ensure t
  :defer t
  :init
  (progn
    (add-hook 'prog-mode-hook 'flyspell-prog-mode)
    (add-hook 'text-mode-hook 'flyspell-mode)
    (add-hook 'emacs-lisp-mode-hook 'flyspell-mode)
    (add-hook 'lisp-interaction-mode-hook 'flyspell-mode)
    )
  :config
  ;; Sets flyspell correction to use two-finger mouse click
  (define-key flyspell-mouse-map [down-mouse-3] #'flyspell-correct-word)
  )

;; Whitespace configuration
(use-package whitespace
  :ensure t
  :diminish whitespace-mode
  :init
  (add-hook 'prog-mode-hook 'whitespace-mode)
  )

;; Theme configuration
(add-to-list 'custom-theme-load-path (expand-file-name --themes-dir))
(load-theme 'nord t)
(setq nord-region-highlight "frost")

;; Org2Blog configuration
(use-package xml-rpc
  :ensure t
  :defer t)
(use-package netrc
  :ensure t
  :defer t)

(setq org2blog/wp-show-post-in-browser t)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("0710b0bdd59c8a7aacf0640591b38fcad5978a0fcfff3fdd999e63499ada8e3e" "dbade2e946597b9cda3e61978b5fcc14fa3afa2d3c4391d477bdaeff8f5638c5" "801a567c87755fe65d0484cb2bded31a4c5bb24fd1fe0ed11e6c02254017acb2" "6bffac6f528e43839861be1d7facf8054b57edc1ffc70f7be885da7d181ecbac" "37768a79b479684b0756dec7c0fc7652082910c37d8863c35b702db3f16000f8" "549ccbd11c125a4e671a1e8d3609063a91228e918ffb269e57bd2cd2c0a6f1c6" default))
 '(fido-mode t)
 '(package-selected-packages
   '(marginalia prescient orderless lsp-java lsp-ui hydra lsp-mode projectile company-flx company-box company markdown-mode php-mode json-mode selectrum-prescient selectrum maven-test-mode javadoc-lookup mvn helm-lsp yasnippet-snippets yasnippet flycheck nordless-theme csv-mode magithub tao-theme dracula-theme git-gutter org-bullets magit dimmer all-the-icons-dired all-the-icons which-key auto-package-update log4j-mode ace-window aggressive-indent easy-kill use-package org2blog nord-theme))
 '(size-indication-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;Aggressive indent mode
;;(use-package aggressive-indent :ensure t)

;; Window switching configuration ;; Ace Window
(use-package ace-window
  :ensure t
  :config
  (global-set-key (kbd "<C-tab>") 'ace-window)
  (setq aw-keys '(?e ?t ?a ?h ?i ?s ?w ?n ?p ?c)))


;; Magit configuration
(use-package magit
  :bind ("C-x g" . magit-status))

(use-package git-gutter
  :config
  (global-git-gutter-mode 't))
(put 'scroll-left 'disabled nil)

;; Java specific configuration
(add-hook 'java-mode-hook #'lsp)
(add-hook 'java-mode-hook 'flycheck-mode)
(add-hook 'java-mode-hook 'company-mode)
(add-hook 'after-init-hook 'global-company-mode)

;; Show all documestation
(setq lsp-eldoc-render-all t)

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
  :ensure t
  :config
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (projectile-mode +1))
(setq projectile-indexing-method 'alien)

(use-package flycheck :ensure t :init (global-flycheck-mode))

;; yasnippet configuration
(use-package yasnippet :config (yas-global-mode))
(use-package yasnippet-snippets :ensure t)

;; lsp mode
(use-package lsp-mode
  :ensure t
  :hook (
         (lsp-mode . lsp-enable-which-key-integration)
         (java-mode . #'lsp-deferred)
         )
  :init (setq
         lsp-keymap-prefix "C-c l"              ; this is for which-key integration documentation, need to use lsp-mode-map
         lsp-enable-file-watchers nil
         read-process-output-max (* 1024 1024)  ; 1 mb
         lsp-completion-provider :capf
         lsp-idle-delay 0.500
         )
  :config
  (setq lsp-intelephense-multi-root nil) ; don't scan unnecessary projects
  (setq lsp-completion-enable-additional-text-edit nil)
  (with-eval-after-load 'lsp-intelephense
    (setf (lsp--client-multi-root (gethash 'iph lsp-clients)) nil))
  (define-key lsp-mode-map (kbd "C-c l") lsp-command-map)
  )
(use-package hydra)
(use-package lsp-ui
  :ensure t
  :after (lsp-mode)
  :bind (:map lsp-ui-mode-map
              ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
              ([remap xref-find-references] . lsp-ui-peek-find-references))
  :init (setq lsp-ui-doc-delay 1.5
              lsp-ui-doc-position 'bottom
	      lsp-ui-doc-max-width 100
              ))

(use-package lsp-java :ensure t :config (add-hook 'java-mode-hook 'lsp))

;; DAP mode for debugging
(use-package dap-mode
  :ensure t
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

;; (use-package helm-lsp
;;   :ensure t
;;   :after (lsp-mode)
;;   :commands (helm-lsp-workspace-symbol)
;;   :init (define-key lsp-mode-map [remap xref-find-apropos] #'helm-lsp-workspace-symbol))
;; (use-package helm
;;   :ensure t
;;   :init
;;   (helm-mode 1)
;;   (progn (setq helm-buffers-fuzzy-matching t))
;;   :bind
;;   (("C-c h" . helm-command-prefix))
;;   (("M-x" . helm-M-x))
;;   (("C-x C-f" . helm-find-files))
;;   (("C-x b" . helm-buffers-list))
;;   (("C-c b" . helm-bookmarks))
;;   (("C-c f" . helm-recentf))   ;; Add new key to recentf
;;   (("C-c g" . helm-grep-do-git-grep)))  ;; Search using grep in a git project

;;;;; Selectrum ;;;;;

(use-package orderless
  :config
  (setq completion-styles '(orderless)))

(use-package prescient
  :config
  (setq prescient-save-file (concat user-emacs-directory "prescient-save"))
  (prescient-persist-mode +1))

(use-package marginalia
  :config
  (marginalia-mode +1))

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

;;;;; Hydra ;;;;;

(use-package hydra
  :config
  ;; Easier cycling of yanking.
  (defhydra yank-pop-hydra ()
    "yank"
    ("C-y" yank nil)
    ("M-y" yank-pop nil)
    ("y" (yank-pop 1) "next")
    ("Y" (yank-pop -1) "prev"))

  (global-set-key (kbd "M-y") #'yank-pop-hydra/yank-pop)
  (global-set-key (kbd "C-y") #'yank-pop-hydra/yank)

  (defhydra compilation-hydra (:columns 4)
    ("c" compile "Compile")
    ("C" compile-from-buffer-folder "Compile from buffer folder")
    ("r" recompile "Recompile")
    ("k" netrom/kill-compilation "Stop")
    ("n" next-error "Next error")
    ("N" next-error-skip-warnings "Next error, skip warnings")
    ("p" previous-error "Previous error")
    ("f" first-error "First error")
    ("l" netrom/compilation-last-error "Last error")
    ("s" netrom/compilation-toggle-scroll "Toggle scroll")
    ("t" netrom/compilation-toggle-threshold "Toggle threshold")
    ("q" nil "Cancel" :color blue))

  (global-set-key [(f5)] 'compilation-hydra/body)

  ;; Define hydra for programming modes.
  (add-hook 'prog-mode-hook
            (lambda ()
              ;; Using local-set-key because defining the bindings in prog-mode-map will get
              ;; overridden by c++-mode bindings, for instance. This shadows them instead.
              (when (member major-mode '(c++-mode c-mode))
                (local-set-key (kbd "C-c C-c") 'compilation-hydra/body)))))


;;treemacs
(use-package lsp-treemacs
  :after (lsp-mode treemacs)
  :ensure t
  :commands lsp-treemacs-errors-list
  :bind (:map lsp-mode-map
              ("M-9" . lsp-treemacs-errors-list)))

(use-package treemacs
  :ensure t
  :commands (treemacs)
  :after (lsp-mode))

;; Automatically add ending brackets and braces
(electric-pair-mode 1)

;;(setenv "JAVA_HOME" "~/.jdks/openjdk-17.0.1")
;;(setq lsp-java-java-path "~/.jdks/openjdk-17.0.1/bin/java")

(provide 'init)
;;; init.el ends here
