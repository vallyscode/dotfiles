;;; package -- Summary
;;; Commentary:
;;; Code:
(require 'lsp)

(defun m()
  "some function"
  (interactive)
  (message (format "%s" "yoo hoo")))

;;;###autoload
(define-minor-mode emacs-java-mode
  "Java client for language server."
  :lighter "Emacs java"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c f") 'm)
            map)
  (make-local-variable 'say-count))

;;;###autoload
(add-hook 'text-mode-hook 'emacs-java-mode)

(provide 'emacs-java-mode)

;;; emacs-java-mode.el ends here
