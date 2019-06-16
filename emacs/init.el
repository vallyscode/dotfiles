;;; new-init.el --- Valerii's Emacs config -*- lexical-binding: t; -*-

;;; Commentary:

;; My personal Emacs configuration.

;;; Code:

;; display GC message & collect on 50MB allocation
(setq garbage-collection-messages t
      gc-cons-threshold (* 50 1024 1024))

(setq user-full-name "Valerii Lysenko"
      user-mail-address "vallyscode@gmail.com")

;; disable and remove standard startup messages
(setq inhibit-startup-screen t
      inhibit-splash-screen t
      initial-scratch-message "")

;; disable unneeded widgets
(when window-system
  (menu-bar-mode -1)
  (scroll-bar-mode -1)
  (tool-bar-mode -1)
  (tooltip-mode -1))

;; no cursor blink
(blink-cursor-mode -1)

;; add line number to mode line
(line-number-mode t)

;; add column number to mode line
(column-number-mode t)

;; display file size in mode line
(size-indication-mode t)

;; no cursor blink
(blink-cursor-mode -1)

;; disable visual notification
(setq ring-bell-function 'ignore)

;; Wrap lines at 90 characters
(setq-default fill-column 90)

;; enable y/n answers
(fset 'yes-or-no-p 'y-or-n-p)

(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))

;; set utf-8
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

;; disable tab indentation
(setq-default indent-tabs-mode nil
              tab-width 2)

;; warn when opening files is > 15MB
(setq large-file-warning-threshold (* 15 1024 1024))

;; auto revert buffer when file changed on disk
(global-auto-revert-mode t)

;; auto-save
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save-list/" t)))

;; backup
(setq backup-directory-alist '(("." . "~/.emacs.d/backups"))
      backup-by-copying t
      kept-new-versions 6
      kept-old-versions 4
      delete-old-versions t
      version-control t
      vc-make-backup-files t)

;; disable C-z
(global-unset-key (kbd "C-z"))

;; smart tab behavior - indent or complete
(setq tab-always-indent 'complete)

;; fix scope for multi-file
(setq-default flycheck-emacs-lisp-load-path 'inherit)

;; set font
(cond ((string= system-type "windows-nt")
       (add-to-list 'default-frame-alist '(font . "Iosevka NF-12")))
      ((or (string= system-type "gnu/linux")
           (string= system-type "darwin"))
       (add-to-list 'default-frame-alist '(font . "Iosevka Nerd Font Mono-12")))
      (t (message "Failed to set font based on system-type")))

(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)


;; --------------------------------------------------------------


(require 'package)

(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("melpa" . "https://melpa.org/packages/")
                         ("melpa-stable" . "https://stable.melpa.org/packages/"))
      package-archive-priorities '(("melpa-stable" . 1)))

(package-initialize)

;; update the package metadata is the local cache is missing
(unless package-archive-contents
  (package-refresh-contents))

;; always load newest byte code
(setq load-prefer-newer t)

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-verbose t)


;; --------------------------------------------------------------


;; built-in
(use-package diminish
  :ensure t)

(use-package paren
  :init
  (setq show-paren-delay 0
        blink-matching-paren nil)
  :config
  (show-paren-mode t))

(use-package elec-pair
  :config
  (electric-pair-mode t))

(use-package hl-line
  :config
  (global-hl-line-mode t))

(use-package dired
  :init
  (setq dired-recursive-deletes 'always
        dired-recursive-copies 'always)
  :config
  (put 'dired-find-alternate-file 'disabled nil))

(use-package eldoc
  :hook (prog-mode . eldoc-mode))

(use-package hideshow
  :hook (prog-mode . hs-minor-mode))

(use-package saveplace
  :config
  (setq save-place-file "~/.emacs.d/saveplace")
  ;; activate it for all buffers
  (setq-default save-place t))

(use-package recentf
  :config
  (setq recentf-save-file "~/.emacs.d/recentf"
        recentf-max-saved-items 500
        recentf-max-menu-items 15
        ;; disable recentf-cleanup on Emacs start, because it can cause
        ;; problems with remote files
        recentf-auto-cleanup 'never)
  (recentf-mode t))

(use-package windmove
  :config
  ;; use shift + arrow keys to switch between visible buffers
  (windmove-default-keybindings))

(use-package lisp-mode
  :config
  (add-hook 'emacs-lisp-mode-hook #'eldoc-mode)
  (add-hook 'emacs-lisp-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'lisp-interaction-mode-hook #'eldoc-mode))

(use-package org
  :init
  (setq org-startup-indented t)
  (setq org-log-done t)
  (setq org-fontify-whole-heading-line t)
  (setq org-fontify-done-headline t)
  (setq org-fontify-quote-and-verse-blocks t))

(use-package org-indent
  :diminish (org-indent-mode . ""))


;; --------------------------------------------------------------


(defvar evil-leader-map (make-sparse-keymap)
  "Keymap for `leader' key shortcuts.")

;; third-party
(use-package cloud-theme
  :load-path "~/.emacs.d/cloud"
  :config
  (load-theme 'cloud t))

(use-package evil
  :ensure t
  :init
  (setq evil-shift-width 2)
  :config
  (evil-mode t)
  (define-key evil-normal-state-map (kbd "SPC") evil-leader-map)
  (define-key evil-visual-state-map (kbd "SPC") evil-leader-map))

(use-package evil-nerd-commenter
  :ensure t
  :after (evil)
  :commands (evilnc-comment-or-uncomment-lines)
  :init
  (define-key evil-leader-map (kbd "ci") 'evilnc-comment-or-uncomment-lines))

(use-package ivy
  :ensure t
  :commands (ivy-switch-buffer
             ivy-switch-buffer-other-window)
  :config
  (ivy-mode t)
  (setq ivy-use-virtual-buffers t
        enable-recursive-minibuffers t)
  (global-set-key (kbd "C-c C-r") 'ivy-resume)
  (define-key evil-leader-map (kbd "bs") 'ivy-switch-buffer)
  (define-key evil-leader-map (kbd "bo") 'ivy-switch-buffer-other-window))

(use-package swiper
  :ensure t
  :config
  (global-set-key "\C-s" 'swiper)
  :pin melpa-stable)

(use-package counsel
  :ensure t
  :commands (counsel-find-file
             counsel-git
             counsel-git-grep)
  :init
  (setq search-default-mode #'char-fold-to-regexp)
  (global-set-key (kbd "M-x") 'counsel-M-x)
  (global-set-key (kbd "C-c C-r") 'ivy-resume)
  (define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history)
  (define-key evil-leader-map (kbd "cg") 'counsel-git))

(use-package avy
  :ensure t
  :bind (("s-." . avy-goto-word-or-subword-1)
         ("s-," . avy-goto-char))
  :config
  (setq avy-background t)
  :pin melpa-stable)

(use-package undo-tree
  :ensure t
  :commands (undo-tree-undo
             undo-tree-redo
             undo-tree-visualize)
  :init
  (setq undo-tree-visualizer-timestamps t
        undo-tree-visualizer-diff t)
  :config
  (global-undo-tree-mode t))

(use-package projectile
  :ensure t
  :commands (projectile-find-file
             projectile-find-dir
             projectile-recentf)
  :init
  (setq projectile-completion-system 'ivy)
  (define-key evil-leader-map (kbd "pf") 'projectile-find-file)
  (define-key evil-leader-map (kbd "pd") 'projectile-find-dir)
  :config
  (projectile-mode t))

(use-package company
  :ensure t
  :config
  (setq company-idle-delay 0.5
        company-show-numbers t
        company-tooltip-limit 10
        company-minimum-prefix-length 2
        company-tooltip-align-annotations t)
  ;; invert the navigation direction if the the completion popup-isearch-match
  ;; is displayed on top (happens near the bottom of windows)
  (setq company-tooltip-flip-when-above t)
  (define-key company-active-map (kbd "M-n") nil)
  (define-key company-active-map (kbd "M-p") nil)
  (define-key company-active-map (kbd "C-j") #'company-select-next)
  (define-key company-active-map (kbd "C-k") #'company-select-previous)
  (define-key company-active-map (kbd "SPC") #'company-abort)
  (global-company-mode t)
  :pin melpa-stable)

(use-package yasnippet
  :ensure t
  :commands (yas-insert-snippet)
  :init
  (define-key evil-leader-map (kbd "yi") 'yas-insert-snippet)
  (yas-global-mode t)
  :config
  (yas-reload-all))

(use-package magit
  :ensure t
  :commands (magit-status
             magit-blame-addition
             magit-blame-quit
             magit-log-other)
  :init
  (define-key evil-leader-map (kbd "gs") 'magit-status)
  (define-key evil-leader-map (kbd "gb") 'magit-blame-addition)
  (define-key evil-leader-map (kbd "gB") 'magit-blame-quit)
  (define-key evil-leader-map (kbd "gl") 'magit-log-other))

(use-package flycheck
  :ensure t
  :commands (flycheck-list-errors
             flycheck-previous-error
             flycheck-next-error)
  :hook (prog-mode . flycheck-mode)
  :config
  (define-key evil-leader-map (kbd "le") 'flycheck-list-errors)
  (define-key evil-leader-map (kbd "lp") 'flycheck-previous-error)
  (define-key evil-leader-map (kbd "ln") 'flycheck-next-error)
  :pin melpa-stable)

(use-package whitespace
  :ensure t
  :init
  (dolist (hook '(prog-mode-hook text-mode-hook))
    (add-hook hook 'whitespace-mode))
  (add-hook 'before-save-hook 'whitespace-cleanup)
  (setq whitespace-line-column 90
        whitespace-style '(face tabs indentation trailing empty lines-trail)))

(use-package which-key
  :ensure t
  :init
  (setq which-key-sort-order 'which-key-key-order-alpha)
  :config
  (which-key-mode t))

(use-package neotree
  :ensure t
  :init
  (setq neo-theme 'ascii)
  (define-key evil-leader-map (kbd "t") 'neotree-toggle)
  :config
  (add-hook 'neotree-mode-hook
            (lambda ()
              (evil-local-set-key 'normal (kbd "q") 'neotree-hide)
              (evil-local-set-key 'normal (kbd "I") 'neotree-hidden-file-toggle)
              (evil-local-set-key 'normal (kbd "z") 'neotree-stretch-toggle)
              (evil-local-set-key 'normal (kbd "RET") 'neotree-enter)
              (evil-local-set-key 'normal (kbd "g") 'neotree-refresh)
              (evil-local-set-key 'normal (kbd "c") 'neotree-create-node)
              (evil-local-set-key 'normal (kbd "d") 'neotree-delete-node)
              (evil-local-set-key 'normal (kbd "r") 'neotree-rename-node)
              (evil-local-set-key 'normal (kbd "s") 'neotree-enter-vertical-split)
              (evil-local-set-key 'normal (kbd "S") 'neotree-enter-horizontal-split))))

(use-package vi-tilde-fringe
  :ensure t
  :hook (prog-mode . vi-tilde-fringe-mode))

(use-package highlight-numbers
  :ensure t
  :hook (prog-mode . highlight-numbers-mode))

(use-package highlight-symbol
  :ensure t
  :commands (highlight-symbol
             highlight-symbol-next
             highlight-symbol-prev
             highlight-symbol-last
             highlight-symbol-first)
  :init
  (define-key evil-leader-map (kbd "hl") 'highlight-symbol))

(use-package rainbow-mode
  :ensure t
  :hook (prog-mde . rainbow-mode))

(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package json-mode
  :ensure t
  :pin melpa-stable)

(use-package yaml-mode
  :ensure t
  :pin melpa-stable)

(use-package markdown-mode
  :ensure t
  :pin melpa-stable)

(use-package tide
  :ensure t
  :pin melpa-stable)

(defun setup-tide-mode ()
  "Set up tide."
  (interactive)
  (tide-setup)
  (flycheck-mode t)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode t)
  (tide-hl-identifier-mode t)
  (company-mode t))

(add-hook 'typescript-mode-hook 'setup-tide-mode)

(use-package js2-mode
  :ensure t
  :pin melpa-stable)

(use-package jsx-mode
  :ensure t
  :after (js2-mode)
  :pin melpa-stable)

(use-package dockerfile-mode
  :ensure t
  :pin melpa-stable)

(use-package haskell-mode
  :ensure t
  :pin melpa-stable)

(use-package intero
  :ensure t
  :after (haskell-mode)
  :config
  (add-hook 'haskell-mode-hook #'intero-mode)
  :pin melpa)

(use-package hindent
  :ensure t
  :after (intero)
  :config
  (add-hook 'haskell-mode-hook #'hindent-mode)
  :pin melpa-stable)

(use-package yalsp
  :load-path "~/workspace/projects/elisp/yalsp")
(require 'yalsp)


;;; new-init.el ends here
