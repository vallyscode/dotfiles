;;; init.el --- My emacs config
;;; -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;; My Emacs configuration.
;;
;;; Code:

(setq
 package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                    ("org" . "https://orgmode.org/elpa/")
                    ("melpa" . "https://melpa.org/packages/")
                    ("melpa-stable" . "https://stable.melpa.org/packages/"))
 package-archive-priorities '(("melpa-stable" . 1)))

(let ((package-dir (concat user-emacs-directory "elpa")))
  (unless (file-directory-p package-dir)
    (make-directory package-dir)))

(unless (fboundp 'use-package)
  (require 'package)
  (package-initialize)
  (unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package)))

(require 'bind-key)

;; gather statistics for `use-package'
(setq-default use-package-compute-statistics t)

(setq-default flycheck-emacs-lisp-load-path 'inherit)

(let ((config-file (concat user-emacs-directory "cfg.org")))
  (when (file-readable-p config-file)
    (org-babel-load-file (expand-file-name "~/.emacs.d/cfg.org"))))

;;; init.el ends here
