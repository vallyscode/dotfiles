;;; espresso.el --- Java LSP client -*- coding: utf-8; lexical-binding: t; -*-

;; Copyright (C) 2018 Valerii Lysenko
;; License: http://www.gnu.org/licenses/gpl.html

;; Author: Valerii Lysenko

;;; Commentary:

;; java LSP client implementation.

;;; Code:

(require 'lsp)

;; (defun photon-java-version()
;;   "Get Java version available in current environment."
;;   (interactive)
;;   (start-process "java-version" "*java-version*" "java" "--version")
;;   (message "xxx"))

;; example start command
;; java -agentlib:jdwp=transport=dt_socket,server=y,suspend=n,address=1044 -Declipse.application=org.eclipse.jdt.ls.core.id1 -Dosgi.bundles.defaultStartLevel=4 -Declipse.product=org.eclipse.jdt.ls.core.product -Dlog.level=ALL -noverify -Xmx1G -jar ./plugins/org.eclipse.equinox.launcher_1.4.0.v20161219-1356.jar -configuration ./config_linux -data /home/user/.emacs.d/photon-workspace --add-modules=ALL-SYSTEM --add-opens java.base/java.util=ALL-UNNAMED --add-opens java.base/java.lang=ALL-UNNAMED

;;
;; Constants
;;

(defconst espresso--windows "windows-nt"
  "System name for MS Windows.")
(defconst espresso--osx "darwin"
  "System type for OSX.")
(defconst espresso--linux "gnu/linux"
  "System type for Linux.")

;;
;; Customization
;;

(setq espresso-lsp-jdt-dir "espresso-download/")
(setq espresso-lsp-server-command
      '("java"
        "-agentlib:jdwp=transport=dt_socket,server=y,suspend=n,address=1044"
        "-Declipse.application=org.eclipse.jdt.ls.core.id1"
        "-Dosgi.bundles.defaultStartLevel=4"
        "-Declipse.product=org.eclipse.jdt.ls.core.product"
        "-Dlog.level=ALL"
        "-noverify"
        "-Xmx1G"
        "-jar"
        "/home/user/.emacs.d/espresso-download/plugins/org.eclipse.equinox.launcher_1.5.100.v20180611-1436.jar"
        "-configuration"
        "/home/user/.emacs.d/espresso-download/config_linux"
        "-data"
        "/home/user/.emacs.d/espresso-workspace"
        "--add-modules=ALL-SYSTEM"
        "--add-opens"
        "java.base/java.util=ALL-UNNAMED"
        "--add-opens"
        "java.base/java.lang=ALL-UNNAMED"))

;;
;; Utils
;;

(defun espresso-get-config-dir()
  "Return configuration directory name for current system."
  (interactive)
  (let ((cfg (cond ((string= system-type photon--windows) "config_win")
                   ((string= system-type photon--linux) "config_linux")
                   ((string= system-type photon--osx) "config_mac"))))
    cfg))

(defun espresso-test-server()
  "Test function."
  (interactive)
  (boson-start-server))

;;;###autoload
(define-minor-mode espresso-mode
  "Java client for language server."
  :lighter " E"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c f") 'photon-test-server)
            (define-key map (kbd "C-c d") 'boson-download-test)
            (define-key map (kbd "C-c k") 'boson-stop-server)
            map)
  (make-local-variable 'say-count))

;;;###autoload
(add-hook 'text-mode-hook 'espresso-mode)

(provide 'espresso-mode)

;;; espresso-mode.el ends here
