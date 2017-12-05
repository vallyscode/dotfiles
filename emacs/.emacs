;; global variables
(setq
 inhibit-startup-screen t
 create-lockfiles nil
 make-backup-files nil
 column-number-mode t
 scroll-error-top-bottom t
 show-paren-delay 0.5
 use-package-always-ensure t
 sentence-end-double-space nil)

;; buffer local variables
(setq-default
 indent-tabs-mode nil
 tab-width 4
 c-basic-offset 4)

;; modes
(electric-indent-mode 0)

;; global keybindings
(global-unset-key (kbd "C-z"))

;; the package manager
(require 'package)
(setq
 package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                    ("org" . "http://orgmode.org/elpa/")
                    ("melpa" . "http://melpa.org/packages/")
                    ("melpa-stable" . "http://stable.melpa.org/packages/"))
 package-archive-priorities '(("melpa-stable" . 1)))

(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)

;; set common exec path
(add-to-list 'exec-path "/usr/local/bin")

;; packages
(use-package solarized-theme
  :ensure t
  :pin melpa-stable)

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

;; haskell ide like features
(use-package intero
  :ensure t
  :pin melpa-stable)

(use-package json-mode
  :ensure t
  :pin melpa-stable)

;; JavaScript
(use-package js2-mode
  :ensure t
  :pin melpa-stable)

(use-package jsx-mode
  :ensure t
  :pin melpa-stable)

(use-package less-css-mode
  :ensure t
  :pin melpa-stable)

(use-package typescript-mode
  :ensure t
  :pin melpa-stable)

(use-package auto-complete
  :ensure t
  :pin melpa-stable)

(use-package glsl-mode
  :ensure t
  :pin melpa)

;; git support
(use-package magit
  :ensure t
  :config (global-set-key (kbd "C-c m") 'magit-status)
  :pin melpa-stable)

;; displays changes in line column
(use-package git-gutter
  :ensure t
  :pin melpa-stable)

;; nerd-tree like explorer
(use-package neotree
  :ensure t
  :pin melpa-stable)

(use-package web-mode
  :ensure t
  :pin melpa-stable)

(use-package tide
  :ensure t
  :pin melpa-stable)

;; vim emulation
(use-package evil
  :ensure t
  :pin melpa-stable)

;; key binding display
(use-package which-key
  :ensure t
  :config (which-key-mode)
  :pin melpa-stable)

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
  :config (global-set-key (kbd "C-c p") 'ace-window)
  :pin melpa-stable)
(use-package ace-jump-mode
  :ensure t
  :config (define-key global-map (kbd "C-c SPC") 'ace-jump-mode)
  :pin melpa-stable)

;; complete and narrowing selections
(use-package helm
  :ensure t
  :config (require 'helm-config)
  :pin melpa-stable)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(custom-enabled-themes (quote (solarized-light)))
 '(custom-safe-themes
   (quote
    ("d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" default)))
 '(intero-global-mode t nil (intero))
 '(package-selected-packages
   (quote
    (helm command-log-mode all-the-icons ace-jump-mode bm evil tidy tide web-mode neotree git-gutter glsl-mode auto-complete typescript-mode less-css-mode jsx-mode js2-mode json-mode intero scala-mode use-package)))
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 105 :width normal :foundry "ADBO" :family "Source Code Pro")))))
