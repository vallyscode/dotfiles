;;; init.el --- vallyscode's emacs config -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;; My personal Emacs configuration.
;;
;;; Code:

(require 'package)

(setq
 package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                    ("org" . "https://orgmode.org/elpa/")
                    ("melpa" . "https://melpa.org/packages/")
                    ("melpa-stable" . "https://stable.melpa.org/packages/"))
 package-archive-priorities '(("melpa-stable" . 1)))

(unless (fboundp 'use-package)
  (require 'package)
  (package-initialize)
  (unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package)))

(setq
 user-full-name "Valerii Lysenko"
 user-mail-address "vallyscode@gmail.com")

;; load newer byte code
(setq load-prefer-newer t)

;; verbose package load
(setq-default use-package-verbose t)

;; display GC message & collect on 50MB allocation
(setq
 garbage-collection-messages t
 gc-cons-threshold (* 50 1024 1024))

;; warn when opening files is > 15MB
(setq large-file-warning-threshold (* 15 1024 1024))

;; auto-save
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save-list/" t)))

;; fix scope for multi-file
(setq-default flycheck-emacs-lisp-load-path 'inherit)

;; backup
(setq
 backup-directory-alist '(("." . "~/.emacs.d/backups"))
 backup-by-copying t
 kept-new-versions 6
 kept-old-versions 4
 delete-old-versions t
 version-control t
 vc-make-backup-files t)

;; disable and remove standard startup messages
(setq
 inhibit-startup-screen t
 inhibit-splash-screen t
 initial-scratch-message "")

;; disable unneeded widgets
(when window-system
  (menu-bar-mode -1)
  (scroll-bar-mode -1)
  (tool-bar-mode -1)
  (tooltip-mode -1))

;; disable visual notification
(setq ring-bell-function 'ignore)

;; set utf-8
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

;; add line number to mode line
(line-number-mode t)
(global-display-line-numbers-mode t)

;; add column number to mode line
(column-number-mode t)

;; no cursor blink
(blink-cursor-mode -1)

;; auto revert buffer when file changed on disk
(global-auto-revert-mode t)

;; disable tab indentation
(setq-default indent-tabs-mode nil)

;; set tab width
(setq-default tab-width 2)

;; line wrapping after 90
(setq-default fill-column 90)

;; resize mini-window
(setq
 resize-mini-windows t
 max-mini-window-height 0.33)

;; sentence end
(setq sentence-end-double-space nil)

;; offset used by + and -
(setq-default c-basic-offset 4)

;; kill ring size
(setq kill-ring-max 50)

;; set exec path
(add-to-list 'exec-path "/usr/local/bin")
(add-to-list 'exec-path "~/.local/bin")
(add-to-list 'exec-path "~/.nvm/versions/node/v8.11.3/bin")

;; disable C-z
(global-unset-key (kbd "C-z"))

;; set font
(cond ((string= system-type "windows-nt")
       (add-to-list 'default-frame-alist '(font . "Iosevka NF-11")))
      ((or (string= system-type "gnu/linux")
           (string= system-type "darwin"))
       (add-to-list 'default-frame-alist '(font . "Iosevka Nerd Font Mono-11")))
      (t (message "Failed to set font based on system-type")))

;; scrolling
(setq
 scroll-margin 0
 scroll-conservatively 100000
 scroll-preserve-screen-position 1)

;; try indent, complete otherwise
(setq tab-always-indent 'complete)

;; change yes-or-no to y-or-n
(fset 'yes-or-no-p 'y-or-n-p)

;; ---
(use-package diminish
  :ensure t
  :pin melpa-stable)

(use-package delight
  :ensure t
  :pin melpa-stable)

;; built-in packages
(use-package paren
  :init
  (setq
   show-paren-delay 0
   blink-matching-paren nil)
  :config
  (show-paren-mode t)
  (electric-pair-mode t)
  (electric-indent-mode t))

(use-package hl-line
  :config
  (global-hl-line-mode t))

(use-package dired
  :init
  (setq
   dired-recursive-deletes 'always
   dired-recursive-copies 'always)
  :config
  (put 'dired-find-alternate-file 'disabled nil))

(use-package eldoc
  :diminish
  (eldoc-mode . "")
  :hook
  (prog-mode . eldoc-mode))

(use-package hideshow
  :diminish (hs-minor-mode . "")
  :hook (prog-mode . hs-minor-mode))

(use-package org
  :init
  (setq org-startup-indented t)
  (setq org-log-done t)
  (setq org-fontify-whole-heading-line t)
  (setq org-fontify-done-headline t)
  (setq org-fontify-quote-and-verse-blocks t))

(use-package org-indent
  :diminish (org-indent-mode . ""))


;; Theme
(use-package cloud-theme
  :load-path "~/.emacs.d/cloud"
  :config
  (load-theme 'cloud t))

;; install evil mode
(use-package evil
  :ensure t
  :init (setq evil-shift-width 2)
  :config (evil-mode t)
  :pin melpa-stable)

(use-package evil-leader
  :ensure t
  :config
  (evil-leader/set-leader "<SPC>")
  (evil-leader/set-key
    "f" 'find-file
    "b" 'switch-to-buffer
    "k" 'kill-buffer
    "gg" 'grep
    "ms" 'bookmark-set
    "md" 'bookmark-delete
    "mr" 'bookmark-rename
    "ml" 'helm-bookmarks
    "li" 'linum-mode)
  (global-evil-leader-mode t)
  :pin melpa-stable)

(use-package evil-nerd-commenter
  :ensure t
  :after (evil-leader)
  :pin melpa-stable)

(evil-leader/set-key
  "ci" 'evilnc-comment-or-uncomment-lines
  "cl" 'evilnc-quick-comment-or-uncomment-to-the-line
  "ll" 'evilnc-quick-comment-or-uncomment-to-the-line
  "cc" 'evilnc-copy-and-comment-lines
  "cp" 'evilnc-comment-or-uncomment-paragraphs
  "cr" 'comment-or-uncomment-region
  "cv" 'evilnc-toggle-invert-comment-line-by-line
  "."  'evilnc-copy-and-comment-operator
  "\\" 'evilnc-comment-operator)

(use-package helm
  :ensure t
  :diminish helm-mode
  :config
  (require 'helm-config)
  (global-set-key (kbd "M-x") 'helm-M-x)
  (global-set-key (kbd "C-x b") 'helm-buffers-list)
  (global-set-key (kbd "C-x r b") 'helm-bookmarks);; making: C-x r m, C-x r b
  (helm-mode 1)
  (evil-leader/set-key
    "hr" 'helm-register
    "ht" 'helm-top
    "hm" 'helm-mini
    "hb" 'helm-buffers-list
    "ho" 'helm-occur
    "hk" 'helm-show-kill-ring)
  :pin melpa-stable)

(use-package projectile
  :ensure t
  :bind* (("M-m SPC d"   . projectile-find-file)
          ("M-m SPC D"   . projectile-switch-project)
          ("M-m SPC TAB" . projectile-find-other-file))
  :init
  (setq-default projectile-file-exists-remote-cache-expire (* 10 60))
  :config
  (projectile-global-mode t))

(use-package helm-projectile
  :ensure t
  :after (projectile helm)
  :bind* (("M-m SPC p" . helm-projectile))
  :init
  (setq projectile-completion-system 'helm)
  (helm-projectile-on)
  :pin melpa-stable)
(evil-leader/set-key
  "pf" 'helm-projectile-find-file
  "pg" 'helm-projectile-grep
  "pp" 'helm-projectile)

(use-package which-key
  :ensure t
  :diminish ""
  :init
  (setq which-key-sort-order 'which-key-key-order-alpha)
  :config
  (which-key-mode t)
  :pin melpa-stable)

(use-package flycheck
  :ensure t
  :hook (prog-mode . flycheck-mode)
  :pin melpa-stable)

(use-package vi-tilde-fringe
  :ensure t
  :diminish (vi-tilde-fringe-mode . "")
  :hook (prog-mode . vi-tilde-fringe-mode))

(use-package company
  :ensure t
  :diminish " Δ"
  :commands (company-select-next
             company-select-previous
             company-abort)
  :hook (prog-mode . company-mode)
  :init
  (setq company-idle-delay 0.3)
  (setq company-show-numbers t)
  (setq company-minimum-prefix-length 4)
  :config
  (define-key company-active-map (kbd "M-n") nil)
  (define-key company-active-map (kbd "M-p") nil)
  (define-key company-active-map (kbd "C-j") #'company-select-next)
  (define-key company-active-map (kbd "C-k") #'company-select-previous)
  (define-key company-active-map (kbd "SPC") #'company-abort)
  :pin melpa-stable)

(use-package undo-tree
  :ensure t
  :diminish ""
  :bind* (("M-m u" . undo-tree-undo)
          ("M-m r" . undo-tree-redo)
          ("M-m U" . undo-tree-visualize))
  :init
  (setq undo-tree-visualizer-timestamps t)
  (setq undo-tree-visualizer-diff t)
  :config
  (global-undo-tree-mode t)
  :pin melpa)

(use-package yasnippet
  :ensure t
  :diminish (yas-minor-mode . "")
  :config
  (yas-global-mode t)
  (yas-reload-all)
  :pin melpa-stable)
(evil-leader/set-key
  "yi" 'yas-insert-snippet)

(use-package whitespace
  :ensure t
  :diminish (whitespace-mode . "")
  :init
  (dolist (hook '(prog-mode-hook text-mode-hook))
    (add-hook hook 'whitespace-mode))
  (add-hook 'before-save-hook 'whitespace-cleanup)
  (setq whitespace-line-column 90) ;; limit line length
  (setq whitespace-style '(face tabs indentation trailing empty lines-tail)))

(use-package rainbow-mode
  :ensure t
  :diminish rainbow-mode
  :hook (prog-mode . rainbow-mode))

(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode)
  :pin melpa-stable)

(use-package highlight-symbol
  :ensure t
  :pin melpa-stable)
(evil-leader/set-key
  "hl" 'highlight-symbol)

(use-package magit
  :ensure t
  :commands (magit-status
             magit-blame
             magit-blame-quit
             magit-log)
  :init
  (evil-leader/set-key
    "gs" 'magit-status
    "gb" 'magit-blame
    "gB" 'magit-blame-quit
    "gl" 'magit-log)
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

(use-package tide
  :ensure t
  :pin melpa-stable)

(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  ;; company is an optional dependency. You have to
  ;; install it separately via package-install
  ;; `M-x package-install [ret] company`
  (company-mode +1))

;; aligns annotation to the right hand side
(setq company-tooltip-align-annotations t)

;; formats the buffer before saving
;; (add-hook 'before-save-hook 'tide-format-before-save)

(add-hook 'typescript-mode-hook 'setup-tide-mode)

(use-package js2-mode
  :ensure t
  :mode ("\\.js$" . js2-mode)
  :config
  :pin melpa-stable)

(use-package jsx-mode
  :ensure t
  :after (js2-mode)
  :pin melpa-stable)

(use-package haskell-mode
  :ensure t
  :mode ("\\.hs$" . haskell-mode)
  :config
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

(use-package dockerfile-mode
  :ensure t
  :pin melpa-stable)


(use-package yalsp
  :load-path "~/workspace/projects/elisp/yalsp")
(require 'yalsp)

;; so that flycheck if works correctly when multiple files
;; (setq-default flycheck-emacs-lisp-load-path 'inherit)

;;; init.el ends here
