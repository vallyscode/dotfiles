;; my test mode
(require 'popup)


(defvar salo-mode-hook nil)
(defvar salo-mode-map
  (let ((salo-mode-map (make-keymap)))
    (define-key salo-mode-map "\C-j" 'newline-and-indent)
    (define-key salo-mode-map "\C-k" 'show-popup)
    salo-mode-map)
  "keymap for salo major mode")

(defun show-popup ()
  (interactive)
  (popup-menu* '("Foo" "Bar")))

(defconst salo-font-lock-keywords-1
  (list
   '("class\\|interface\\|type" . font-lock-type-face)
   '("function\\|class" . font-lock-builtin-face)
   '("var\\|const" . font-lock-variable-name-face))
  "Minimal highlighting")

(defvar salo-font-lock-keywords salo-font-lock-keywords-1
  "Default highlighting for testmode")

(defvar salo-mode-syntax-table
  (let ((salo-mode-syntax-table (make-syntax-table)))
    (modify-syntax-entry ?/ ". 124b" salo-mode-syntax-table)
    (modify-syntax-entry ?* ". 23" salo-mode-syntax-table)
    (modify-syntax-entry ?\n "> b" salo-mode-syntax-table)
    salo-mode-syntax-table)
  "Syntax table for salo")

(defun salo-mode ()
  (interactive)
  (kill-all-local-variables)
  (use-local-map salo-mode-map)
  (set-syntax-table salo-mode-syntax-table)
  ;; set up fontlock
  (set (make-local-variable 'font-lock-defaults) '(salo-font-lock-keywords))
  (setq major-mode 'salo-mode)
  (setq mode-name "SALO MODE")
  (run-hooks 'salo-mode-hook))

(provide 'salo-mode)

;; (defconst my-js-mode-syntax-table
;;   (let ((table (make-syntax-table)))
;;     ;; ' is a string delimiter
;;     (modify-syntax-entry ?' "\"" table)
;;     ;; " is a string delimiter too
;;     (modify-syntax-entry ?\" "\"" table)

;;     ;; / is punctuation, but // is a comment starter
;;     (modify-syntax-entry ?/ ". 12" table)
;;     ;; \n is a comment ender
;;     (modify-syntax-entry ?\n ">" table)
;;     table))

;; (define-derived-mode testmode prog-mode "Test Mode"
;;   :syntax-table my-js-mode-syntax-table
;;   (font-lock-fontify-buffer))

;; (provide 'testmode)
