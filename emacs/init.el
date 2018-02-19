(setq gc-cons-threshold (* 500 1024 1024)) ;;GC threshold to 500MB

(setq vc-make-backup-files t) ;;backup files covered by version control

(setq
 backup-by-copying t
 delete-old-versions t 
 kept-new-versions 6
 kept-old-versions 2
 version-control t ;;numeric backup versions
 backup-directory-alist '(("." . "~/.emacs.d/backups")) ;; place for backups
)

(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save-list/" t)))

(setq inhibit-startup-screen t) ;;no startup screen
(setq initial-scratch-message "") ;;no scratch message
(setq inhibit-splash-screen t) ;;no splash screen

(setq sentence-end-double-space nil)

;; (setq cursor-type '(bar . 2)) ;; vertical line 2px width

(setq large-file-warning-threshold (* 15 1024 1024)) ;;large file warning 15MB

(setq ring-bell-function 'ignore)
(setq visible-bell t)

(setq resize-mini-windows t)
(setq max-mini-window-height 0.33)

(setq blink-matching-paren nil)
(setq show-paren-delay 0)
(setq show-paren-style 'expression)
(show-paren-mode t)

(display-time-mode 1)

(setq-default fill-column 80) ;;linewrapping after 80
(setq-default indent-tabs-mode nil) ;;do not use tabs for indentation
(setq-default tab-width 2) ;;use 2 spaces instead of tab
(setq-default c-basic-offset 4) ;;offset used by + & -

(setq-default buffer-file-coding-system 'utf-8-auto-unix)
(prefer-coding-system 'utf-8)

(global-linum-mode t) ;;show line numbers everywhere
(column-number-mode t) ;;show column nuber in modeline
(scroll-bar-mode -1) ;;no scroll bar
(tool-bar-mode -1) ;;no tool bar
(menu-bar-mode -1) ;;no menu bar
(blink-cursor-mode -1) ;;no cursor blinking
(global-hl-line-mode 1) ;; hilight current line
(electric-pair-mode 1) ;;insert closing paren automatically
(electric-indent-mode 0) ;;disable auto-indent

(fset 'yes-or-no-p 'y-or-n-p) ;; y | n instead of yes | no

(if (file-exists-p "~/.emacs.secrets")
    (load-file "~/.emacs.secrets"))

;; set default font
;; "TerminusTTF NF-14"
;; "SourceCodePro Nerd Font Mono"
(when (member "SourceCodePro Nerd Font Mono" (font-family-list))
  (set-face-attribute 'default nil :font "SourceCodePro Nerd Font Mono-12"))

;;(set-default-font "TerminusTTF NF-9")
;; ---------------------------------------

(defun my/insert-timestamp()
  "Insert a full ISO 8601 timestamp."
  (interactive)
  (insert (format-time-string "%Y-%m-%dT%T%z")))

(defun my/linum-setup ()
  "Use relative mode for normal state and absolute for insert."
  (when (eq evil-state 'normal)
    (linum-relative-on))
  (add-hook 'evil-insert-state-exit-hook #'linum-relative-on)
  (add-hook 'evil-insert-state-entry-hook (lambda ()
                                            (linum-relative-off)
                                            (linum-mode)
                                            )))
(add-hook 'prog-mode-hook #'my/linum-setup)

;; -----------------------------------------

;; common exec path
(add-to-list 'exec-path "/usr/local/bin")
;; stack bin mac
(add-to-list 'exec-path "/Users/valeriy/.local/bin")
;; stack bin linux
(add-to-list 'exec-path "/home/vagrant/.local/bin")



;; The package manager
(require 'package)
(setq
 package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                    ("org" . "https://orgmode.org/elpa/")
                    ("melpa" . "https://melpa.org/packages/")
                    ("melpa-stable" . "https://stable.melpa.org/packages/"))
 package-archive-priorities '(("melpa-stable" . 1)))

(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)




;; (use-package leuven-theme
;;  :ensure t
;;  :config
;;  (load-theme 'leuven t)
;;  :pin melpa)

;; (use-package atom-one-dark-theme
;;   :ensure t
;;   :pin melpa-stable)

(use-package solarized-theme
 :ensure t
 :config
 (load-theme 'solarized-light t)
 :pin melpa-stable)

(use-package which-key
  :ensure t
  :init
  (setq which-key-sort-order 'which-key-key-order-alpha)
  :config 
  (which-key-mode)
  :pin melpa-stable)

(use-package company
  :ensure t
  :diminish " ⓒ"
  :config
  (add-hook 'prog-mode-hook 'company-mode)
  :pin melpa-stable)

(use-package yasnippet
  :ensure t
  :config
  (yas-global-mode 1)
  :pin melpa-stable)

(use-package linum-relative
  :ensure t
  :pin melpa-stable)

(use-package rainbow-delimiters
  :ensure t
  :config
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
  :pin melpa-stable)

(use-package undo-tree
  :ensure t
  :diminish undo-tree-mode
  :bind* (("M-m u" . undo-tree-undo)
          ("M-m r" . undo-tree-redo)
          ("M-m U" . undo-tree-visualize))
  :config
  (global-undo-tree-mode 1)
  (setq undo-tree-visualizer-timestamps t)
  (setq undo-tree-visualizer-diff t)
  :pin melpa)

(use-package magit
  :ensure t
  :bind* (("M-m g s" . magit-status)
          ("M-m g b" . magit-blame)
          ("M-m g B" . magit-blame-quit)
          ("M-m g l" . magit-log-all))
  :pin melpa-stable)

(use-package evil
  :ensure t
  :config
  (use-package evil-leader
    :ensure t
    :config
    (evil-leader/set-leader "<SPC>")
    (evil-leader/set-key
      "f" 'find-file
      "b" 'switch-to-buffer
      "k" 'kill-buffer
      "g" 'grep
      "ms" 'bookmark-set
      "md" 'bookmark-delete
      "mr" 'bookmark-rename
      "ml" 'helm-bookmarks
      "mb" 'magit-blame
      "mB" 'magit-blame-quit
      "ho" 'helm-occur
      "hr" 'helm-register
      "ht" 'helm-top
      "hp" 'helm-projectile
      "hm" 'helm-mini
      "hb" 'helm-buffers-list
      "hk" 'helm-show-kill-ring
      "hy" 'helm-yas-complete
      "li" 'linum-mode
      "lr" 'linum-relative-toggle
      "ci" 'evilnc-comment-or-uncomment-lines
      "cl" 'evilnc-quick-comment-or-uncomment-to-the-line
      "ll" 'evilnc-quick-comment-or-uncomment-to-the-line
      "cc" 'evilnc-copy-and-comment-lines
      "cp" 'evilnc-comment-or-uncomment-paragraphs
      "cr" 'comment-or-uncomment-region
      "cv" 'evilnc-toggle-invert-comment-line-by-line
      "."  'evilnc-copy-and-comment-operator
      "\\" 'evilnc-comment-operator ; if you prefer backslash key
      )
    (global-evil-leader-mode)
    :pin melpa-stable)
  (use-package evil-nerd-commenter
   :ensure t
   :pin melpa-stable)
  (use-package evil-tutor
   :ensure t
   :pin melpa-stable)
  (evil-mode 1)
  (evilnc-default-hotkeys)
  :pin melpa-stable)

;; window management
(use-package ace-window
  :ensure t
  :config
  (global-set-key (kbd "C-c o") 'ace-window)
  :pin melpa-stable)

(use-package ace-jump-mode
  :ensure t
  :config
  (define-key global-map (kbd "C-c SPC") 'ace-jump-mode)
  :pin melpa-stable)

(use-package projectile
  :ensure t
  :bind* (("M-m SPC d"   . projectile-find-file)
          ("M-m SPC D"   . projectile-switch-project)
          ("M-m SPC TAB" . projectile-find-other-file))
  :init
  (setq projectile-file-exists-remote-cache-expire (* 10 60))
  :diminish projectile-mode
  :config
  (projectile-global-mode))

(use-package helm
  :ensure t
  :diminish helm-mode
  :config 
  (require 'helm-config)
  (global-set-key (kbd "M-x") 'helm-M-x)
  (global-set-key (kbd "C-x b") 'helm-buffers-list)
  (global-set-key (kbd "C-x r b") 'helm-bookmarks);; making: C-x r m, C-x r b
  ;;(global-set-key (kbd "C-x h o") 'helm-occur) ;; simillar to occur
  ;;(global-set-key (kbd "C-x C-f") 'helm-find-files)
(helm-mode 1)
  (use-package helm-projectile
    :ensure t
    :bind* (("M-m SPC p" . helm-projectile))
    :init
    (setq projectile-completion-systtem 'helm)
    :pin melpa-stable)
  :pin melpa-stable)

(use-package restclient
  :ensure t
  :pin melpa)

(use-package intero
  :ensure t
  :config
  (intero-global-mode 1)
  (use-package hindent
    :ensure t
    :init
    (setq hindent-reformat-buffer-on-save t)
    :config
    (add-hook 'haskell-mode-hook #'hindent-mode)
    :pin melpa-stable)
  :pin melpa)

(use-package ensime
  :ensure t
  :config
  (use-package scala-mode
    :ensure t
    :pin melpa)
  (use-package sbt-mode
    :ensure t
    :pin melpa)
  :pin melpa)

(use-package web-mode
  :ensure t
  :mode ("\\.html$" . web-mode)
  :pin melpa-stable)

(use-package js2-mode
  :ensure t
  :mode ("\\.js$" . js2-mode)
  :config
  (use-package jsx-mode
    :ensure t
    :pin melpa-stable)
  :pin melpa-stable)

(use-package tide
  :ensure t
  :config
  (use-package typescript-mode
    :ensure t
    :pin melpa-stable)
  :pin melpa-stable)

(use-package less-css-mode
  :ensure t
  :pin melpa-stable)

(use-package json-mode
  :ensure t
  :mode "\\.json$"
  :pin melpa-stable)

(use-package yaml-mode
  :ensure t
  :mode "\\.yml$"
  :pin melpa-stable)

(use-package markdown-mode
  :ensure t
  :mode ("\\.markdown\\'" "\\.mkd\\'" "\\.md\\'")
  :pin melpa-stable)
