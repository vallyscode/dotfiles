(setq garbage-collection-message t)
(setq gc-cons-threshold (* 50 1024 1024)) ;;GC threshold to 50MB

(setq vc-make-backup-files t) ;;backup files covered by version control

(setq
 backup-by-copying t
 delete-old-versions t
 kept-new-versions 6
 kept-old-versions 2
 version-control t ;;numeric backup versions
 backup-directory-alist '(("." . "~/.emacs.d/backups")))

(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save-list/" t)))

(setq use-package-verbose t)

(setq inhibit-startup-screen t) ;;no startup screen
(setq initial-scratch-message "") ;;no scratch message
(setq inhibit-splash-screen t) ;;no splash screen

(setq sentence-end-double-space nil)

(setq large-file-warning-threshold (* 15 1024 1024)) ;;large file warning 15MB

(setq ring-bell-function 'ignore)
(setq visible-bell t)

(setq resize-mini-windows t)
(setq max-mini-window-height 0.33)

(setq blink-matching-paren nil)
(setq show-paren-delay 0)
(setq show-paren-style 'expression)
(show-paren-mode t)

;; always delete and copy recursively
(setq dired-recursive-deletes 'always)
(setq dired-recursive-copies 'always)

(display-time-mode t)

(setq-default fill-column 80) ;;linewrapping after 80
(setq-default indent-tabs-mode nil) ;;do not use tabs for indentation
(setq-default tab-width 2) ;;use 2 spaces instead of tab
(setq-default c-basic-offset 4) ;;offset used by + & -

(setq-default buffer-file-coding-system 'utf-8-auto-unix)
(setq locale-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(prefer-coding-system 'utf-8)

;; (global-linum-mode t) ;;show line numbers everywhere
(line-number-mode t) ;;show line number in modeline
(column-number-mode t) ;;show column nuber in modeline
(scroll-bar-mode -1) ;;no scroll bar
(tool-bar-mode -1) ;;no tool bar
(menu-bar-mode -1) ;;no menu bar
(blink-cursor-mode -1) ;;no cursor blinking
(global-hl-line-mode t) ;; hilight current line
(electric-pair-mode t) ;;insert closing paren automatically
(electric-indent-mode t) ;;disable auto-indent

(setq tab-always-indent 'complete) ;;smart tab behavior - indent or complete

(fset 'yes-or-no-p 'y-or-n-p) ;; y | n instead of yes | no

(if (file-exists-p "~/.emacs.secrets")
    (load-file "~/.emacs.secrets"))

;; set default font
(when (member "SourceCodePro Nerd Font Mono" (font-family-list))
  (set-face-attribute 'default nil :font "SourceCodePro Nerd Font Mono" :height 120))

;; common exec path
(add-to-list 'exec-path "/usr/local/bin")
;; stack bin mac
(add-to-list 'exec-path "/Users/valeriy/.local/bin")
;; stack bin linux
(add-to-list 'exec-path "/home/vagrant/.local/bin")
;; nvm node location
(add-to-list 'exec-path "/home/vagrant/.nvm/versions/node/v6.13.0/bin")

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


(use-package all-the-icons
  :ensure t
  :pin melpa-stable)

(use-package monokai-theme
  :ensure t
  :config
  (load-theme 'monokai t)
  :pin melpa-stable)

;; (use-package solarized-theme
;;  :ensure t
;;  :config
;;  (load-theme 'solarized-light t)
;;  (custom-set-faces
;;     '(mode-line ((t (:background "#eee8d5" :foreground "#657b83" :box (:line-width 1 :color "#eee8d5" :style unspecified) :overline nil :underline nil))))
;;     '(mode-line-inactive ((t (:background "#fdf6e3" :foreground "#93a1a1" :box (:line-width 1 :color "#eee8d5" :style unspecified) :overline nil :underline nil)))))
;;  :pin melpa-stable)

(use-package evil
  :ensure t
  :init
  (setq evil-shift-width 2)
  :config
  (evil-mode t)
  (evilnc-default-hotkeys)
  :pin melpa-stable)

(use-package evil-nerd-commenter
  :ensure t
  :after (evil)
  :pin melpa-stable)

(use-package evil-leader
  :ensure t
  :after (evil)
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
    "gs" 'magit-status
    "gb" 'magit-blame
    "gB" 'magit-blame-quit
    "gl" 'magit-log
    "ho" 'helm-occur
    "hr" 'helm-register
    "ht" 'helm-top
    "hp" 'helm-projectile
    "hm" 'helm-mini
    "hb" 'helm-buffers-list
    "hk" 'helm-show-kill-ring
    "li" 'linum-mode
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
  (global-evil-leader-mode t)
  :pin melpa-stable)

(use-package spaceline
  :ensure t
  :after (evil)
  :init
  (require 'spaceline-config)
  (spaceline-spacemacs-theme)
  (setq spaceline-highlight-face-func 'spaceline-highlight-face-evil-state)
  (set-face-attribute 'spaceline-evil-emacs nil :background "#be84ff")
  (set-face-attribute 'spaceline-evil-insert nil :background "#5fd7ff")
  (set-face-attribute 'spaceline-evil-motion nil :background "#ae81ff")
  (set-face-attribute 'spaceline-evil-normal nil :background "#a6e22e")
  (set-face-attribute 'spaceline-evil-replace nil :background "#f92672")
  (set-face-attribute 'spaceline-evil-visual nil :background "#fd971f")
  (spaceline-helm-mode)
  (setq-default
   powerline-height 20
   powerline-default-separator 'utf-8
   powerline-gui-use-vcs-glyph t
   spaceline-minor-modes-separator " ")
  :pin melpa-stable)

(use-package neotree
  :ensure t
  :init
  (setq neo-theme 'arrow)
  :config
  (evil-leader/set-key
    "t" 'neotree-toggle)
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
        (evil-local-set-key 'normal (kbd "S") 'neotree-enter-horizontal-split)))
  :pin melpa-stable)

(use-package which-key
  :ensure t
  :init
  (setq which-key-sort-order 'which-key-key-order-alpha)
  :config
  (which-key-mode t)
  :pin melpa-stable)

(use-package company
  :ensure t
  :config
  (add-hook 'prog-mode-hook #'company-mode)
  :pin melpa-stable)

(use-package yasnippet
  :ensure t
  :config
  (yas-global-mode t)
  (yas-reload-all)
  (evil-leader/set-key
    "yi" 'yas-insert-snippet)
  :pin melpa-stable)

(use-package goto-chg
  :ensure t
  :commands goto-last-change
  :pin melpa-stable)

(use-package whitespace
  :ensure t
  :init
  (dolist (hook '(prog-mode-hook text-mode-hook))
    (add-hook hook #'whitespace-mode))
  (add-hook 'before-save-hook #'whitespace-cleanup)
  :config
  (setq whitespace-line-column 80) ;; limit line length
  (setq whitespace-style '(face tabs empty trailing lines-tail)))

(use-package rainbow-mode
  :ensure t
  :config
  (add-hook 'prog-mode-hook #'rainbow-mode))

(use-package rainbow-delimiters
  :ensure t
  :config
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
  :pin melpa-stable)

(use-package highlight-symbol
  :ensure t
  :config
  (evil-leader/set-key
    "hl" 'highlight-symbol)
  :pin melpa-stable)

(use-package undo-tree
  :ensure t
  :diminish undo-tree-mode
  :bind* (("M-m u" . undo-tree-undo)
          ("M-m r" . undo-tree-redo)
          ("M-m U" . undo-tree-visualize))
  :config
  (global-undo-tree-mode t)
  (setq undo-tree-visualizer-timestamps t)
  (setq undo-tree-visualizer-diff t)
  :pin melpa)

(use-package magit
  :ensure t
  :commands (magit-status magit-blame magit-blame-quit)
  ;; :bind* (("M-m g s" . magit-status)
  ;;         ("M-m g b" . magit-blame)
  ;;         ("M-m g B" . magit-blame-quit))
  :pin melpa-stable)

(use-package projectile
  :ensure t
  :diminish projectile-mode
  :bind* (("M-m SPC d"   . projectile-find-file)
          ("M-m SPC D"   . projectile-switch-project)
          ("M-m SPC TAB" . projectile-find-other-file))
  :init
  (setq projectile-file-exists-remote-cache-expire (* 10 60))
  :config
  (projectile-global-mode t))

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
  :pin melpa-stable)

(use-package helm-projectile
  :ensure t
  :after (projectile helm)
  :bind* (("M-m SPC p" . helm-projectile))
  :init
  (setq projectile-completion-systtem 'helm)
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

(use-package ensime
    :ensure t
    :pin melpa)

(use-package sbt-mode
  :ensure t
  :pin melpa)

(use-package web-mode
  :ensure t
  :mode ("\\.html$" . web-mode)
  :pin melpa-stable)

(use-package js2-mode
  :ensure t
  :mode ("\\.js$" . js2-mode)
  :config
  :pin melpa-stable)

(use-package jsx-mode
  :ensure t
  :after (js2-mode)
  :pin melpa-stable)

(use-package typescript-mode
  :ensure t
  :mode (("\\.ts$" . typescript-mode)
         ("\\.tsx$" . typescript-mode))
  :config
  :pin melpa-stable)

(use-package tide
  :ensure t
  :after (typescript-mode)
  :pin melpa-stable)

(use-package less-css-mode
  :ensure t
  :mode ("\\.css\\'" "\\.less\\'")
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
