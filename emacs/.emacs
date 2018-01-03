;; global variables
(setq
 create-lockfiles nil
 make-backup-files nil
 scroll-error-top-bottom t
 show-paren-delay 0.5
 use-package-always-ensure t
 ns-pop-up-frames nil
 sentence-end-double-space nil)

;; hilight current line
(global-hl-line-mode)

;; disable the menu bar
(menu-bar-mode 0)

;; turn on visible bell
(setq visible-bell t)

;; no startup screen
(setq inhibit-startup-screen t)

;; no scratch message
(setq initial-scratch-message "")

;; no splash screen
(setq inhibit-splash-screen t)

;; show column nuber in mode-line
(setq column-number-mode t)

;; set garbage collection threshold to 500MB
(setq gc-cons-threshold (* 500 1024 1024))

;; no tool bar
(tool-bar-mode -1)

;; no scroll bar
(scroll-bar-mode -1)

;; no tooltips
(tooltip-mode -1)

;; set cursor shape to be vertical line of 2px width
(setq-default cursor-type '(bar . 2))

;; disable cursor blink
(blink-cursor-mode -1)

;; utf-8
(prefer-coding-system 'utf-8)
(setq-default buffer-file-coding-system 'utf-8-auto-unix)

;; large file warning
(setq large-file-warning-threshold (* 15 1024 1024))

;; ediff defaults
(setq ediff-window-setup-function 'ediff-setup-windows-plain
      ediff-split-window-function 'split-window-horizontally)

;; pdf reading
(setq doc-view-continuous t)

;; buffer local variables
(setq-default
 indent-tabs-mode nil
 tab-width 4
 c-basic-offset 4)

;; add left and right margin
(setq-default
 left-margin-width 1
 right-margin-width 1)

;; modes
(electric-indent-mode 0)

;; global keybindings
(global-unset-key (kbd "C-z"))

;; the package manager
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

;; set common exec path
(add-to-list 'exec-path "/usr/local/bin")

;; haskell ide like features
(use-package intero
  :ensure t
  :config
  (intero-global-mode 1)
  :pin melpa)

(use-package json-mode
  :ensure t
  :mode "\\.json$"
  :pin melpa-stable)

(use-package yaml-mode
  :ensure t
  :mode "\\.yml$"
  :pin melpa-stable)

;; scala ide like features
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

;; JavaScript
(use-package js2-mode
  :ensure t
  :mode ("\\.js$" . js2-mode)
  :pin melpa-stable)

(use-package jsx-mode
  :ensure t
  :pin melpa-stable)

;; typescript ide
(use-package tide
  :ensure t
  :config
  (use-package typescript-mode
    :ensure t
    :pin melpa-stable)
  :pin melpa-stable)

;; less/css
(use-package less-css-mode
  :ensure t
  :pin melpa-stable)

(use-package web-mode
  :ensure t
  :mode ("\\.html$" . web-mode)
  :pin melpa-stable)

;; git support
(use-package magit
  :ensure t
  :bind* (("M-m g s" . magit-status)
          ("M-m g b" . magit-blame)
          ("M-m g B" . magit-blame-quit)
          ("M-m g l" . magit-log-all))
  :pin melpa-stable)

;; key binding display
(use-package which-key
  :ensure t
  :defer t
  :diminish which-key-mode
  :init
  (setq which-key-sort-order 'which-key-key-order-alpha)
  :config 
  (which-key-mode)
  :pin melpa-stable)

;; completion
(use-package company
  :ensure t
  :diminish C
  :pin melpa-stable)

;; markdown
(use-package markdown-mode
  :ensure t
  :mode ("\\.markdown\\'" "\\.mkd\\'" "\\.md\\'")
  :pin melpa-stable)

;; vim emulation
(use-package evil
  :ensure t
  :config
  (evil-mode 1)
  (use-package evil-leader
    :ensure t
    :config
    (global-evil-leader-mode)
    :pin melpa-stable)
  (use-package evil-surround
    :ensure t
    :config
    (global-evil-surround-mode)
    :pin melpa-stable)
  (use-package evil-nerd-commenter
    :ensure t
    :pin melpa-stable)
  :pin melpa-stable)

;; nerd-tree like explorer
(use-package neotree
  :ensure t
  :bind* (("M-m SPC n". neotree-toggle))
  :init
  (setq
   neo-smart-open t
   neo-theme 'arrow)
  :pin melpa-stable)

;; undo
(use-package undo-tree
  :ensure t
  :diminish undo-tree-mode
  :bind* (("M-m u" . undo-tree-undo)
          ("M-m r" . undo-tree-redo)
          ("M-m U" . undo-tree-visualize))
  :config
  (global-undo-tree-mode 1))

;; dir comparison
(use-package ztree
  :ensure t
  :bind* (("M-m z v" . ztree-dir)
          ("M-m z V" . ztree-diff))
  :init
  (setq ztree-dir-move-focus t))

;; bookmark support
(use-package bm
  :ensure t
  :bind (("C-c =" . bm-toggle)
         ("C-c [" . bm-previous)
         ("C-c ]" . bm-next))
  :pin melpa-stable)

;; colored delimiters
(use-package rainbow-delimiters
  :ensure t
  :config
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
  :pin melpa-stable)

;; folding
(use-package vimish-fold
  :ensure t
  :commands (vimish-fold-toggle
             vimish-fold)
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

;; project management
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

;; complete and narrowing selections
(use-package helm
  :ensure t
  :diminish helm-mode
  :config 
  (require 'helm-config)
  (global-set-key (kbd "M-x") 'helm-M-x)
  (global-set-key (kbd "C-x b") 'helm-buffers-list)
  (global-set-key (kbd "C-x r b") 'helm-bookmarks);; making: C-x r m
  ;;(global-set-key (kbd "C-x C-f") 'helm-find-files)
  (helm-mode 1)
  :pin melpa-stable)

(use-package helm-projectile
  :ensure t
  :bind* (("M-m SPC p" . helm-projectile))
  :init
  (setq projectile-completion-systtem 'helm)
  :pin melpa-stable)

;; rest client
(use-package restclient
  :ensure t
  :pin melpa)

;; modeline from spaceemacs
(use-package spaceline
  :ensure t
  :demand t
  :init
  (setq powerline-default-separator 'utf-8)
  :config
  (require 'spaceline-config)
  (spaceline-spacemacs-theme)
  (setq spaceline-highlight-face-func 'spaceline-highlight-face-evil-state)
  :pin melpa-stable)

;;;;;;;;;;;; themes ;;;;;;;;;;;;;;;;

;; leuven, great light theme
;;(use-package leuven-theme
;;  :ensure t
;;  :pin melpa)

;; atom one dark theme
(use-package atom-one-dark-theme
  :ensure t
  :pin melpa-stable)

;; heroku theme
;;(use-package heroku-theme
;;  :ensure t
;;  :pin melpa)

;; solarized
;;(use-package solarized-theme
;;  :ensure t
;;  :config
;;  (load-theme 'solarized-light t)
;;  :pin melpa-stable)

;;(use-package spacemacs-theme
;;  :ensure t
;;  :init
;;  (load-theme 'spacemacs-dark t)
;;  (setq spacemacs-theme-org-agenda-height nil)
;;  (setq spacemacs-theme-org-height nil))
