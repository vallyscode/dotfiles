;;; lsp.el --- LSP client -*- coding: utf-8; lexical-binding: t; -*-

;; Copyright (C) 2018 Valerii Lysenko
;; License: http://www.gnu.org/licenses/gpl.html

;; Author: Valerii Lysenko

;;; Commentary:

;; yet another one LSP client implementation for Emacs.

;;; Code:

(require 'json)

;;
;; Constants
;;

(defconst boson-log-buffer-name "*lsp*"
  "Logging buffer name.")

;;
;; Customization
;;

(defvar lsp-lsp-jdt-url "http://download.eclipse.org/jdtls/snapshots/jdt-language-server-latest.tar.gz"
  "URL for JDT server download.")

(defvar lsp-lsp-jdt-dir "lsp-jdt-dir/"
  "JDT server location.")

(defvar lsp-lsp-jdt-file "jdt-language-server-latest.tar.gz"
  "JDT server file name.")

(defvar lsp-lsp-server-command '("ls" "-la" "/bin")
  "A command to start a server.")

(defvar lsp-process nil
  "Contains an object describing server process.")

(defvar lsp-process-name "lsp-server"
  "Process name.")

;;
;; Public
;;

(defun lsp-log(message)
  "Logging MESSAGE to dedicated log buffer *lsp*."
  (let ((log-buffer (get-buffer-create boson-log-buffer-name)))
    (with-current-buffer log-buffer
      (insert (format "%s" message)))))

(defun lsp-download(url directory file)
  "Download FILE to DIRECTORY from URL."
  (unless (file-exists-p directory)
    (make-directory directory t))
  (message
   (format "Downloading from: %s to: %s. Please wait..."
           url
           directory))
  (url-handler-mode t)
  (if (file-exists-p url)
      (progn
        (url-copy-file url (concat directory file))
        (message (format "Download complete from: %s to %s"
                         url
                         directory)))
    (error "Not found: %s" url)))

(defun lsp-download-test()
  "Test function."
  (interactive)
  (lsp-download lsp-lsp-jdt-url (locate-user-emacs-file lsp-lsp-jdt-dir) lsp-lsp-jdt-file))

(defun boson-start-server()
  "Start LSP server."
  (interactive)
  (setq
   lsp-process (make-process
                         :name lsp-process-name
                         :connection-type 'pipe
                         :coding 'no-conversion
                         :command lsp-lsp-server-command
                         :filter 'lsp--process-filter
                         :sentinel 'lsp--process-sentinel
                         ;; :buffer "lsp-server-buffer"
                         :noquery t)))

(defun lsp-stop-server()
  "Stops server process."
  (interactive)
  (kill-process lsp-process))

(defun lsp-send-wait(message)
  "Send MESSAGE to server process and simulate waiting."
  (message (format "Send: %s %s" message lsp-process))
  (process-send-string lsp-process message)
  (with-local-quit
    (let* ((send-time (time-to-seconds (current-time)))
           (expected-time (+ send-time 10)))
      (accept-process-output lsp-process (- expected-time (time-to-seconds (current-time))))
      (when (< expected-time (time-to-seconds (current-time)))
        (error "WAIT FAIL")))))

;;
;; Private
;;

(defun lsp--process-filter(process output)
  "Get OUTPUT from PROCESS."
  (lsp-log (format "FILTER: %s" output)))

(defun lsp--process-sentinel(process event)
  "Get EVENT related to PROCESS."
  (lsp-log (format "SENTINEL: %s" event)))

(provide 'lsp)

;;; lsp.el ends here