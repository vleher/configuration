(require 'package)                   ; Bring in to the environment all package management functions

;; A list of package repositories
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org"   . "https://orgmode.org/elpa/")
                         ("elpa"  . "https://elpa.gnu.org/packages/")))

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

;; Configure the Emacs Frame
(setq inhibit-startup-screen t)
(tool-bar-mode 0)
(blink-cursor-mode 0)
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

(set-face-attribute 'default t :font "Noto Sans-11")
(set-face-attribute 'default nil :font "Noto Sans-11")

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
(use-package magit
  :config (global-set-key (kbd "C-x g") 'magit-status))

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

;; Fido mode and configuration
(fido-mode t)

;;Ivy configuration
;; (use-package counsel
;;   :after ivy
;;   :config (counsel-mode))

;; (use-package ivy
;;   :defer 0.1
;;   :diminish
;;   :bind (("C-c C-r" . ivy-resume)
;;          ("C-x B" . ivy-switch-buffer-other-window))
;;   :custom
;;   (ivy-count-format "(%d/%d) ")
;;   (ivy-use-virtual-buffers t)
;;   :config (ivy-mode))

;; (use-package swiper
;;   :after ivy
;;  )

;; (global-set-key (kbd "C-s") 'swiper)
;; (global-set-key (kbd "C-r") 'swiper-backward)
;; (global-set-key (kbd "M-x") 'counsel-M-x)
;; (global-set-key (kbd "C-x b") 'ivy-switch-buffer)
;; (global-set-key (kbd "C-c v") 'ivy-push-view)
;; (global-set-key (kbd "C-c V") 'ivy-pop-view)

;;Autocomplete
;; (use-package company
;;   :diminish
;;   :bind (("C-." . #'company-complete))
;;   )

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
(load-theme 'nord t)

;; Org2Blog configuration
(use-package xml-rpc
  :ensure t
  :defer t)
(use-package netrc
  :ensure t
  :defer t)

(use-package org2blog
  :ensure t
  :bind ("C-x w" .  org2blog-user-interface)
  :defer t
  :init
  (setq org2blog/wp-blog-alist
        '(("lostsaloon"
           :url "http://www-testsite.lostsaloon.com/xmlrpc.php"
           :username "barkeep"
           :password "D-Young06."
           :confirm t))
        ))


(setq org2blog/wp-show-post-in-browser t)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("dbade2e946597b9cda3e61978b5fcc14fa3afa2d3c4391d477bdaeff8f5638c5" "801a567c87755fe65d0484cb2bded31a4c5bb24fd1fe0ed11e6c02254017acb2" "6bffac6f528e43839861be1d7facf8054b57edc1ffc70f7be885da7d181ecbac" "37768a79b479684b0756dec7c0fc7652082910c37d8863c35b702db3f16000f8" "549ccbd11c125a4e671a1e8d3609063a91228e918ffb269e57bd2cd2c0a6f1c6" default))
 '(fido-mode t)
 '(package-selected-packages
   '(tao-theme dracula-theme git-gutter org-bullets magit dimmer all-the-icons-dired all-the-icons which-key auto-package-update log4j-mode ace-window aggressive-indent easy-kill use-package org2blog nord-theme))
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
