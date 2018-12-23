;;; cloud-theme.el --- A light theme based on Material

;;; Commentary:

;; Cloud light theme based on Material

;;; Code:

(deftheme cloud "Cloud light theme")

(let ((class '((class color) (min-colors 89))))
  (custom-theme-set-faces
   'cloud

   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;;
   ;; Mode line

   `(mode-line ((,class (:background "#005f87" :foreground "#eeeeee" :box (:line-width 1 :color "#005f87")))))
   `(mode-line-inactive ((,class (:background "#d0d0d0" :foreground "#444444" :box (:line-width 1 :color "#d0d0d0")))))
   `(mode-line-emphasis ((,class (:foreground "#eeeeee" :slant italic))))
   `(mode-line-highlight ((,class (:foreground "#8700af"))))
   `(mode-line-buffer-id ((,class (:weight bold))))

   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;;
   ;; Escape and prompt

   `(minibuffer-prompt ((,class (:foreground "#005f87"))))
   `(escape-glyph ((,class (:weight bold :foreground "#fff59d"))))
   `(error ((,class (:foreground "#af0000"))))
   `(warning ((,class (:foreground "#d75f00"))))
   `(success ((,class (:foreground "#008000"))))

   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;;
   ;; Font lock

   `(font-lock-builtin-face ((,class (:foreground "#3e999f"))))
   `(font-lock-comment-delimiter-face ((,class (:foreground "#878787"))))
   `(font-lock-comment-face ((,class (:slant italic :foreground "#878787"))))
   `(font-lock-constant-face ((,class (:foreground "#af0000"))))
   `(font-lock-doc-face ((,class (:foreground "#008000"))))
   `(font-lock-function-name-face ((,class (:foreground "#005f87"))))
   `(font-lock-keyword-face ((,class (:bold nil :foreground "#df0087"))))
   `(font-lock-preprocessor-face ((,class (:foreground "#878787"))))
   `(font-lock-regexp-grouping-backslash ((,class (:weight bold :inherit nil))))
   `(font-lock-regexp-grouping-construct ((,class (:weight bold :inherit nil))))
   `(font-lock-string-face ((,class (:foreground "#718c00"))))
   `(font-lock-type-face ((,class (:weight normal :foreground "#6434a3"))))
   `(font-lock-variable-name-face ((,class (:weight normal :foreground "#BA36a5"))))
   `(font-lock-warning-face ((,class (:weight bold :foreground "#d75f00"))))


   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;;
   ;; Button and link

   `(link ((,class (:foreground "#0087af"))))
   `(link-visited ((,class (:underline t :foreground "#e5786d"))))

   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;;
   ;; Highlight

   `(default ((,class (:background "#eeeeee" :foreground "#444444"))))
   `(cursor ((,class (:background "#005f87" :foreground "#eeeeee"))))
   `(highlight ((,class (:background "#e4e4e4"))))
   `(fringe ((,class (:background "#eeeeee"))))
   `(match ((,class (:weight bold :background "#fff59d"))))
   `(secondary-selection ((,class (:weight bold :background "#fff59d"))))
   `(region ((,class (:background "#0087af" :foreground "#eeeeee"))))
   `(isearch ((,class (:underline "#444444" :foreground "#eeeeee" :background "#0087af"))))
   `(isearch-fail ((,class (:weight bold :foreground "#eeeeee" :background "#af0000"))))
   `(lazy-highlight ((,class (:foreground "#444444" :background "#fff59d")))) ; Isearch others (see `match').
   `(query-replace ((,class (:inherit isearch))))
   `(whitespace-hspace ((,class (:foreground "#d0d0d0"))))
   `(whitespace-line ((,class (:foreground "#af0000" :background "#eeeeee"))))
   `(whitespace-indentation ((,class (:background nil :foreground "#444444"))))
   `(whitespace-tab ((,class (:background nil :foreground "#444444"))))
   `(whitespace-trailing ((,class (:background nil :foreground "#af0000"))))
   ))

;;;###autoload
(and load-file-name
     (boundp 'custom-theme-load-path)
     (add-to-list 'custom-theme-load-path
                  (file-name-as-directory
                   (file-name-directory load-file-name))))

(provide-theme 'cloud)

;;; cloud-theme.el ends here
