;;; package -- Summary
;;; Commentary:
;;; Code:

(require 'json)

;; (defun start-server()
;;   (interactive)
;;   (start-process "my-process" "foo" "ls" "-la" "/bin"))

;; (defconst start-command '("ls" "-la" "/bin")
;;   "A command to start server instance.")

(defconst jdt-lsp-url "http://download.eclipse.org/jdtls/snapshots/jdt-language-server-latest.tar.gz"
  "Eclipse JDT LSP server.")

(defconst jdt-lsp-file "jdt-language-server-latest.tar.gz"
  "JDT file name.")

(defconst lsp-download-dir (locate-user-emacs-file "lsp-download/")
  "Download directory.")

(defun lsp-download()
  "Download files."
  (interactive)
  (let ((destination lsp-download-dir))
    (unless (file-exists-p destination)
      (make-directory destination t))
    (message (format "Downloading module from: %s to: %s. Please wait..."
                     jdt-lsp-url
                     destination))
    (url-handler-mode t)
    (if (file-exists-p jdt-lsp-url)
        (progn
          (url-copy-file jdt-lsp-url (format "%s%s" destination jdt-lsp-file ))
          (message (format "Download complete from: %s to: %s"
                           jdt-lsp-url
                           destination)))
      (error "Not found: %s" jdt-lsp-url))
    )
  (lsp-log "Downloading...."))

(defvar server-process nil
  "Contains an object describing server process.")

(defconst lsp-start-command '("echo-hs-exe")
  "A command to start a server.")

(defun lsp-log(message)
  "Log to dedicated notifications buffer."
  (let* ((log-buffer (get-buffer-create "*lsp*")))
    (with-current-buffer log-buffer
      (insert (format "%s" message)))))

(defun lsp--filter(process output)
  "LSP process filter."
  (lsp-log (format "LSP: %s" output)))

(defun lsp--sentinel(process event)
  "LSP process sentinel."
  (lsp-log event))

(defun lsp-start-server()
  "Start LSP server."
  (interactive)
  (setq server-process
        (make-process
         :name "my-process"
         :connection-type 'pipe
         :coding 'no-conversion
         :command lsp-start-command
         :filter 'lsp--filter
         :sentinel 'lsp--sentinel
         ;; :buffer "lsp-server-buffer"
         :noquery t)))

(provide 'lsp)

;;; lsp.el ends here
