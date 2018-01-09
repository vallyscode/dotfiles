;; global variables
(setq
 create-lockfiles nil
 make-backup-files nil
 scroll-error-top-bottom t
 show-paren-delay 0.5
 use-package-always-ensure t
 ns-pop-up-frames nil
 sentence-end-double-space nil
 inhibit-startup-screen t ;; no startup screen
 initial-scratch-message "" ;; no scratch message
 inhibit-splash-screen t ;; no splash screen
 column-number-mode t ;; show column number in mode line
 gc-cons-threshold (* 500 1024 1024) ;; garbage collection threshold 500MB
 cursor-type '(bar . 2) ;; vertical line 2px width
 large-file-warning-threshold (* 15 1024 1024) ;; large file warning
 ediff-window-setup-function 'ediff-setup-windows-plain
 ediff-split-window-function 'split-window-horizontally
 doc-view-continuous t ;; pdf reading
)

;; buffer local variables
(setq-default
 indent-tabs-mode nil ;; not tab indentation
 tab-width 4 ;; tab width 4 spaces
 c-basic-offset 4
 buffer-file-coding-system 'utf-8-auto-unix
)

;; modes
(electric-indent-mode 0) ;; disable auto-indent
(electric-pair-mode 1) ;; insert closing parens auomatically
(global-hl-line-mode 1) ;; hilight current line
(menu-bar-mode 0) ;; no menu bar
(tool-bar-mode -1) ;; no tool bar
(scroll-bar-mode -1) ;; no scroll bar
(tooltip-mode -1) ;; no tooltips
(blink-cursor-mode -1) ;; no blink

(prefer-coding-system 'utf-8) ;; utf-8

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

(use-package less-css-mode
  :ensure t
  :pin melpa-stable)

(use-package web-mode
  :ensure t
  :mode ("\\.html$" . web-mode)
  :pin melpa-stable)

(use-package magit
  :ensure t
  :bind* (("M-m g s" . magit-status)
          ("M-m g b" . magit-blame)
          ("M-m g B" . magit-blame-quit)
          ("M-m g l" . magit-log-all))
  :pin melpa-stable)

(use-package which-key
  :ensure t
  :init
  (setq which-key-sort-order 'which-key-key-order-alpha)
  :config 
  (which-key-mode)
  :pin melpa-stable)

;; completion
(use-package company
  :ensure t
  :config
  (add-hook 'prog-mode-hook 'company-mode)
  :pin melpa-stable)

;; markdown
(use-package markdown-mode
  :ensure t
  :mode ("\\.markdown\\'" "\\.mkd\\'" "\\.md\\'")
  :pin melpa-stable)

(use-package evil
  :ensure t
  :config
  (use-package evil-leader
    :ensure t
    :config
    (evil-leader/set-leader "<SPC>")
    (evil-leader/set-key
      "e" 'find-file
      "b" 'switch-to-buffer
      "k" 'kill-buffer
      "g" 'grep
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
  ;; (use-package evil-magit
  ;;   :ensure t
  ;;   :pin melpa-stable)
  (use-package evil-surround
    :ensure t
   :config
   (global-evil-surround-mode)
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
  :pin melpa-stable)

(use-package ztree
  :ensure t
  :bind* (("M-m z v" . ztree-dir)
          ("M-m z V" . ztree-diff))
  :init
  (setq ztree-dir-move-focus t))

(use-package bm
  :ensure t
  :bind (("C-c =" . bm-toggle)
         ("C-c [" . bm-previous)
         ("C-c ]" . bm-next))
  :pin melpa-stable)

(use-package rainbow-delimiters
  :ensure t
  :config
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
  :pin melpa-stable)

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

(use-package restclient
  :ensure t
  :pin melpa)

;;;;;;;;;;;; themes ;;;;;;;;;;;;;;;;

(use-package leuven-theme
 :ensure t
 :config
 (load-theme 'leuven t)
 :pin melpa)

;; (use-package ample-theme
;;   :ensure t
;;   :init
;;   (progn (load-theme 'ample t t)
;;          (load-theme 'ample-flat t t)
;;          (load-theme 'ample-light t t)
;;          (enable-theme 'ample-flat))
;;   :config
;;   (enable-theme 'ample-light)
;;   :pin melpa)

;; (use-package atom-one-dark-theme
;;   :ensure t
;;   :pin melpa-stable)

;;(use-package heroku-theme
;;  :ensure t
;;  :pin melpa)

;;(use-package solarized-theme
;;  :ensure t
;;  :config
;;  (load-theme 'solarized-light t)
;;  :pin melpa-stable)

;; (use-package zenburn-theme
;;   :ensure t
;;   :config
;;   (load-theme 'zenburn t)
;;   :pin melpa-stable)
