;; global variables
(setq
 inhibit-startup-screen t
 initial-scratch-message ""
 create-lockfiles nil
 make-backup-files nil
 column-number-mode t
 scroll-error-top-bottom t
 show-paren-delay 0.5
 use-package-always-ensure t
 ns-pop-up-frames nil
 sentence-end-double-space nil
 large-file-warning-threshold (* 15 1024 1024))



;; buffer local variables
(setq-default
 indent-tabs-mode nil
 tab-width 4
 c-basic-offset 4
 cursor-type '(bar . 1)
 buffer-file-coding-system 'utf-8-auto-unix)

;; modes
(electric-indent-mode 0)

;; utf-8
(prefer-coding-system 'utf-8)


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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; haskell ide like features
(use-package intero
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; scala ide like features
(use-package ensime
  :ensure t
  :pin melpa)

;; scala build tool
(use-package sbt-mode
  :pin melpa)

;; scala mode
(use-package scala-mode
  :pin melpa)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; JavaScript
(use-package js2-mode
  :ensure t
  :pin melpa-stable)

(use-package jsx-mode
  :ensure t
  :pin melpa-stable)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TypeScript
(use-package typescript-mode
  :ensure t
  :pin melpa-stable)

(use-package tide
  :ensure t
  :pin melpa-stable)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; less/css
(use-package less-css-mode
  :ensure t
  :pin melpa-stable)

(use-package web-mode
  :ensure t
  :pin melpa-stable)

;; git support
(use-package magit
  :ensure t
  :config
  (global-set-key (kbd "C-c m") 'magit-status)
  :pin melpa-stable)

;; key binding display
(use-package which-key
  :ensure t
  :diminish which-key-mode
  :config 
  (which-key-mode)
  :pin melpa-stable)

;; completion
(use-package company
  :ensure t
  :pin melpa-stable)

;; markdown
(use-package markdown-mode
  :ensure t
  :pin melpa-stable)

;; vim emulation
(use-package evil
  :ensure t
  :pin melpa-stable)

;; nerd-tree like explorer
(use-package neotree
  :ensure t
  :bind* (("M-m SPC n". neotree-toggle))
  :init
  (setq neo-smart-open t)
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

;; project management
(use-package projectile
  :ensure t
  :bind* (("M-m SPC d"   . projectile-find-file)
          ("M-m SPC D"   . projectile-switch-project)
          ("M-m SPC TAB" . projectile-find-other-file))
  :init
  (setq projectile-file-exists-remote-cache-expire (* 10 60))
  :config
  (projectile-global-mode))

;; dir comparison
(use-package ztree
  :ensure t
  :bind* (("M-m g v" . ztree-dir)
          ("M-m g V" . ztree-diff))
  :init
  (setq ztree-dir-move-focus t))

;; bookmark support
(use-package bm
  :ensure t
  :bind (("C-c =" . bm-toggle)
         ("C-c [" . bm-previous)
         ("C-c ]" . bm-next))
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

;; rest client
(use-package restclient
  :ensure t
  :pin melpa)

;;;;;;;;;;;; themes ;;;;;;;;;;;;;;;;
;; leuven, great light theme
(use-package leuven-theme
  :ensure t
  :pin melpa)

;; atom one dark theme
;;(use-package atom-one-dark-theme
;;  :ensure t
;;  :pin melpa-stable)

;; heroku theme
;;(use-package heroku-theme
;;  :ensure t
;;  :pin melpa)

;; mode line
;;(use-package spaceline
;;  :ensure t
;;  :demand t
;;  :init
;;  (setq powerline-default-separator 'wave)
;;  :config
;;  (require 'spaceline-config)
;;  (spaceline-emacs-theme)
;;  (spaceline-helm-mode))

;; set theme
(load-theme 'leuven t)



(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(blink-cursor-mode nil)
 '(intero-global-mode t nil (intero))
 '(menu-bar-mode 1)
 '(package-selected-packages
   (quote
    (tern yaml-mode heroku-theme atom-one-dark-theme leuven-theme helm command-log-mode all-the-icons ace-jump-mode bm evil tidy tide web-mode neotree git-gutter glsl-mode auto-complete typescript-mode less-css-mode jsx-mode js2-mode json-mode intero scala-mode use-package)))
 '(scroll-bar-mode nil)
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
