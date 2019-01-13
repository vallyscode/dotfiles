;;; cloud-theme.el --- A light theme based on Material

;;; Commentary:

;; Cloud light theme based on Material

;;; Code:

(deftheme cloud
  "Cloud light color theme")

;; color palette
;; #005f87
;; #006daf
;; #0087af

(let ((class '((class color) (min-colors 89))))

  (custom-theme-set-faces
   'cloud

   `(default ((,class (:foreground "#444444" :background "#eeeeee"))))

   `(cursor ((,class (:foreground "#eeeeee" :background "#005f87"))))

   ;; Highlighting
   `(fringe ((,class (:background "#eeeeee"))))
   `(highlight ((,class (:background "#bdbdbd"))))
   `(region ((,class (:background "#0087af" :foreground "#eeeeee"))))
   `(secondary-selection ((,class (:weight bold :background "#ffff8d"))))
   `(isearch ((,class (:foreground "#eeeeee" :background "#0087af"))))
   `(isearch-fail ((,class (:weight bold :foreground "#eeeeee" :background "#af0000"))))
   `(lazy-highlight ((,class (:foreground "#444444" :background "#fff59d")))) ; Isearch others (see `match').
   `(trailing-whitespace ((,class (:foreground "#bdbdbd" :background "#ffff8d"))))
   `(query-replace ((,class (:inherit isearch))))
   `(whitespace-hspace ((,class (:foreground "#bdbdbd"))))
   `(whitespace-indentation ((,class (:background nil :foreground "#444444"))))
   `(whitespace-line ((,class (:foreground "#af0000" :background "#eeeeee"))))
   `(whitespace-tab ((,class (:background nil :foreground "#444444"))))
   `(whitespace-trailing ((,class (:background nil :foreground "#af0000"))))

   ;; Mode line
   `(mode-line ((,class (:background "#005f87" :foreground "#eeeeee" :box (:line-width 1 :color "#005f87")))))
   `(mode-line-inactive ((,class (:background "#d0d0d0" :foreground "#444444" :box (:line-width 1 :color "#d0d0d0")))))
   `(mode-line-buffer-id ((,class (:weight bold))))
   `(mode-line-emphasis ((,class (:foreground "#eeeeee" :slant italic))))
   `(mode-line-highlight ((,class (:foreground "#8700af"))))

   ;; Escape and prompt
   `(minibuffer-prompt ((,class (:weight bold :foreground "#005f87"))))
   `(minibuffer-noticeable-prompt ((,class (:weight bold :foreground "#005f87"))))
   `(escape-glyph ((,class (:weight bold :foreground "#fff59d"))))
   `(error ((,class (:foreground "#af0000"))))
   `(warning ((,class (:foreground "#d75f00"))))
   `(success ((,class (:foreground "#388E3C"))))

   ;; Font lock
   `(font-lock-builtin-face ((,class (:foreground "#3e999f"))))
   `(font-lock-comment-delimiter-face ((,class (:foreground "#878787"))))
   `(font-lock-comment-face ((,class (:slant italic :foreground "#878787"))))
   `(font-lock-constant-face ((,class (:foreground "#af0000"))))
   `(font-lock-doc-face ((,class (:foreground "#878787"))))
   `(font-lock-function-name-face ((,class (:foreground "#005f87"))))
   `(font-lock-keyword-face ((,class (:bold nil :foreground "#df0087"))))
   `(font-lock-preprocessor-face ((,class (:foreground "#878787"))))
   `(font-lock-regexp-grouping-backslash ((,class (:weight bold :inherit nil))))
   `(font-lock-regexp-grouping-construct ((,class (:weight bold :inherit nil))))
   `(font-lock-string-face ((,class (:foreground "#388E3C"))))
   `(font-lock-type-face ((,class (:weight normal :foreground "#6434a3"))))
   `(font-lock-variable-name-face ((,class (:weight normal :foreground "#005f87"))))
   `(font-lock-warning-face ((,class (:weight bold :foreground "#d75f00"))))

   ;; Button and link
   `(link ((,class (:foreground "#0087af"))))
   `(link-visited ((,class (:underline t :foreground "#e5786d"))))
   `(button ((,class (:underline t :foreground "#006daf"))))
   `(header-line ((,class (:background "#005f87" :foreground "#eeeeee" :box (:line-width 1 :color "#005f87")))))

   `(company-tooltip-common-selection ((,class (:weight normal :foreground "#eeeeee"))))
   `(company-tooltip-selection ((,class (:weight normal :foreground "#eeeeee" :background "#0087af"))))
   `(company-tooltip-annotation-selection ((,class (:weight normal :foreground "eeeeee"))))
   `(company-tooltip-common ((,class (:weight bold))))
   `(company-tooltip ((,class (:foreground "#424242" :background "#cfd8dc"))))
   `(company-tooltip-annotation ((,class (:weight normal :foreground "#651FFF"))))
   `(company-preview-common ((,class (:weight normal :foreground "#0087af" :inherit hl-line))))
   `(company-scrollbar-bg ((,class (:background "#cfd8dc"))))
   `(company-scrollbar-fg ((,class (:background "#607D8B"))))

   `(dired-directory ((,class (:weight normal :foreground "#005f87" :background "#eeeeee"))))
   `(dired-header ((,class (:weight normal :foreground "#005f87" :background "#eeeeee"))))
   `(dired-ignored ((,class (:strike-through t :foreground "#af0000"))))
   `(dired-mark ((,class (:foreground "#af0000" :background "#e5786d"))))
   `(dired-marked ((,class (:foreground "#af0000" :background "#e5786d"))))
   `(dired-symlink ((,class (:foreground "#ba36a5"))))
   `(diredp-compressed-file-suffix ((,class (:foreground "#af0000"))))
   `(diredp-date-time ((,class (:foreground "#6434a3"))))
   `(diredp-dir-heading ((,class (:weight normal :foreground "#005f87" :background "#eeeeee"))))
   `(diredp-dir-name ((,class (:weight normal :foreground "#005f87" :background "#eeeeee"))))
   `(diredp-dir-priv ((,class (:weight normal :foreground "#005f87" :background "#eeeeee"))))
   `(diredp-exec-priv ((,class (:background "#03C03C"))))
   `(diredp-executable-tag ((,class (:foreground "ForestGreen" :background "white"))))
   `(diredp-file-name ((,class (:foreground "#424242"))))
   `(diredp-file-suffix ((,class (:foreground "#c0c0c0"))))
   `(diredp-flag-mark-line ((,class (:foreground "#af0000" :background "#e5786d"))))
   `(diredp-ignored-file-name ((,class (:foreground "#7F7F7F"))))
   `(diredp-read-priv ((,class (:background "#0A99FF"))))
   `(diredp-write-priv ((,class (:foreground "white" :background "#FF4040"))))
   `(eldoc-highlight-function-argument ((,class (:foreground "#689f38" :weight bold))))

   `(file-name-shadow ((,class (:foreground "#7F7F7F"))))

   `(flycheck-info ((,class (:underline (:color "#158A15" :style wave) :weight bold))))
   `(flycheck-warning ((,class (:underline (:color "#F4A939" :style wave) :weight bold :background "#FFFFBE"))))
   `(flycheck-error ((,class (:underline (:color "#af0000" :style wave) :weight bold :background "#FFE1E1"))))
   `(flycheck-fringe-info ((,class (:foreground "#158A15"))))
   `(flycheck-fringe-warning ((,class (:foreground "#F4A939"))))
   `(flycheck-fringe-error ((,class (:foreground "#af0000"))))
   `(flycheck-error-list-line-number ((,class (:foreground "#A535AE"))))


   `(linum ((,class (:background "#eeeeee" :foreground "#9E9E9E"))))
   `(hl-line ((,class (:background "#e4e4e4"))));; highlight current line
   `(match ((,class (:weight bold :background "#fff59d"))))


   `(show-paren-match ((,class (:background "#d0d0d0" :foreground "#005f87"))))
   `(show-paren-mismatch ((,class (:background "#d0d0d0" :foreground "#af0000"))))

   ;; eldoc

   ;; helm
   `(helm-M-x-key ((,class (:foreground "#008000"))))
   `(helm-action ((,class (:foreground "#444444"))))
   `(helm-header ((,class (:foreground "#eeeeee" :background "#0087af"))))
   `(helm-moccur-buffer ((,class (:foreground "#444444" :background "#cfd8dc"))));;highlight moccur buffer name
   `(helm-source-header ((,class (:foreground "#444444" :background "#cfd8dc" :height 1.3 :bold t))))
   `(helm-match ((,class (:foreground "#444444" :background "#fff59d"))))
   `(helm-selection ((,class (:background "#e4e4e4"))))
   `(helm-selection-line ((,class (:background "#e4e4e4"))))
   `(helm-separator ((,class (:foreground "#444444"))))
   `(helm-visible-mark ((,class (:foreground "#aa0000" :background "#ffaaaa"))))
   `(helm-buffer-directory ((,class (:foreground "#0087af" :weight bold))))
   `(helm-buffer-file ((,class (:foreground "#444444"))))
   `(helm-buffer-not-saved ((,class (:foreground "#d75f00"))))
   `(helm-buffer-process ((,class (:foreground "#e5786d"))))
   `(helm-buffer-saved-out ((,class (:foreground "#af0000"))))
   `(helm-buffer-size ((,class (:foreground "#d75f00"))))
   `(helm-candidate-number ((,class (:foreground "#eeeeee" :background "#0087af"))))
   `(helm-ff-directory ((,class (:foreground "#0087af" :weight bold))))
   `(helm-ff-executable ((,class (:foreground "#008000"))))
   `(helm-ff-file ((,class (:foreground "#444444"))))
   `(helm-ff-invalid-symlink ((,class (:foreground "#8700af" :background "#af0000"))))
   `(helm-ff-symlink ((,class (:foreground "#8700af"))))

   ;; js2
   `(js2-error ((,class (:box (:line-width 1 :color "#af0000") :background "#ffaaaa"))))
   `(js2-warning ((,class (:underline "orange"))))
   `(js2-external-variable ((,class (:foreground "#FF0000"))))
   `(js2-function-param ((,class (:foreground "#247284"))))
   `(js2-instance-member ((,class (:foreground "#8700af"))))
   `(js2-jsdoc-html-tag-delimiter ((,class (:foreground "#D0372D"))))
   `(js2-jsdoc-html-tag-name ((,class (:foreground "#D0372D"))))
   `(js2-jsdoc-tag ((,class (:weight normal :foreground "#6434A3"))))
   `(js2-jsdoc-type ((,class (:foreground "#0087af"))))
   `(js2-jsdoc-value ((,class (:weight normal :foreground "#BA36A5")))) ; #800080
   `(js2-magic-paren ((,class (:underline t))))
   `(js2-private-function-call ((,class (:foreground "goldenrod"))))
   `(js2-private-member ((,class (:foreground "PeachPuff3"))))
   ))

;;;###autoload
(and load-file-name
     (boundp 'custom-theme-load-path)
     (add-to-list 'custom-theme-load-path
                  (file-name-as-directory
                   (file-name-directory load-file-name))))

(provide-theme 'cloud)

;;; cloud-theme.el ends here
