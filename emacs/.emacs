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
(use-package ensime
  :ensure t
  :pin melpa)

(use-package sbt-mode
  :pin melpa)

(use-package scala-mode
  :pin melpa)

(use-package intero
  :ensure t
  :pin melpa-stable)

(use-package json-mode
  :ensure t
  :pin melpa-stable)

(use-package js2-mode
  :ensure t
  :pin melpa-stable)

(use-package jsx-mode
  :ensure t
  :pin melpa-stable)

(use-package less-css-mode
  :ensure t
  :pin melpa-stable)

(use-package nyan-mode
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

(use-package magit
  :ensure t
  :pin melpa-stable)

(use-package git-gutter
  :ensure t
  :pin melpa-stable)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (leuven)))
 '(nyan-mode t)
 '(package-selected-packages
   (quote
    (git-gutter glsl-mode auto-complete typescript-mode nyan-mode less-css-mode jsx-mode js2-mode json-mode intero scala-mode use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "#FFFFFF" :foreground "#333333" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 128 :width normal :foundry "adobe" :family "Source Code Pro")))))
