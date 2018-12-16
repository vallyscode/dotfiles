;; Base16 PaperColor-Light (https://github.com/chriskempson/base16)
;; Scheme: Nguyen Nguyen (http://github.com/NLKNguyen)

;;; papercolor-light-theme.el

;;; Code:

(deftheme papercolor-light)

(let ((base00 "#262626")
      (base01 "#262626")
      (base02 "#4d4d4c")
      (base03 "#808080")
      (base04 "#949494")
      (base05 "#d0d0d0")
      (base06 "#eeeeee")
      (base07 "#eeeeee")
      (base08 "#8959a8")
      (base09 "#d75f00")
      (base0A "#4271ae")
      (base0B "#718c00")
      (base0C "#3e999f")
      (base0D "#005f87")
      (base0E "#d7005f")
      (base0F "#d70000")
      (border "#0087af")
      (default-fg "#4d4d4c")
      (default-bg "#eeeeee")
      (fringe-bg "#eeeeee")
      (cursor-fg "#eeeeee")
      (cursor-bg "#005f87")
      (cursor-line "#e4e4e4")
      (cursor-column "#e4e4e4")
      (cursor-line-nr-fg "#af5f00")
      (cursor-line-nr-bg "#eeeeee")
      (line-number-fg "#b2b2b2")
      (line-number-bg "#eeeeee")
      (vert-split-fg "#005f87")
      (vert-split-bg "#eeeeee")
      (mode-line-active-fg "#e4e4e4")
      (mode-line-active-bg "#005f87")
      (mode-line-inactive-fg "#444444")
      (mode-line-inactive-bg "#d0d0d0")
      (mode-line-buffer-id-fg "#eeeeee")
      (mode-line-emphasis-fg "#eeeeee")
      (mode-line-highlight-fg "#d7005f")
      (error-fg "#af0000")
      (error-bg "#ffd7ff")
      (visual-fg "#eeeeee")
      (visual-bg "#0087af")
      (link-fg "#0087af")
      (link-visited-fg "#df0000")
      (minibuffer-prompt-fg "#005f87")
      (secondary-selection-bg "#fff59d")
      (warning-fg "#d75f00")
      (success-fg "#718c00"))

  (unless (display-graphic-p)
    (setq base00 "black"
          base01 "color-18"
          base02 "color-19"
          base03 "brightblack"
          base04 "color-20"
          base05 "white"
          base06 "color-21"
          base07 "brightwhite"
          base08 "red"
          base09 "color-16"
          base0A "yellow"
          base0B "green"
          base0C "cyan"
          base0D "blue"
          base0E "magenta"
          base0F "color-17"))

  (custom-theme-set-faces
   'papercolor-light

   ;; Built-in stuff (Emacs 23)
   ;; This face is used for the vertical divider between windows on text terminals.
   `(vertical-border ((t (:background "#eeeeee" :foreground "#005f87"))))
   `(border-glyph ((t (nil))))
   `(cursor ((t (:background "#005f87" :foreground "#eeeeee"))))
   ;; default is used for text that does not specify any faces.
   ;; Its background color is used as frames background color.
   `(default ((t (:background "#eeeeee" :foreground "#4d4d4c"))))
   `(fringe ((t (:background "#eeeeee"))))
   `(gui-element ((t (:background "#808080" :foreground "#eeeeee"))))
   `(highlight ((t (:background "#e4e4e4"))))
   `(link ((t (:foreground "#0087af"))))
   `(link-visited ((t (:foreground "#df0000"))))
   `(minibuffer-prompt ((t (:foreground "#005f87"))))
   `(mode-line ((t (:background "#005f87" :foreground "#e4e4e4" :box (:line-width 1 :color "#005f87")))))
   `(mode-line-buffer-id ((t (:foreground nil :background nil :weight bold))))
   `(mode-line-emphasis ((t (:foreground "#eeeeee" :slant italic))))
   `(mode-line-highlight ((t (:foreground "#d7005f" :box nil :weight bold))))
   `(mode-line-inactive ((t (:background "#d0d0d0" :foreground "#444444" :box (:line-width 1 :color "#4E4E4C")))))
   `(region ((t (:background "#0087af" :foreground "#e4e4e4"))))
   `(secondary-selection ((t (:background "#fff59d"))))
   `(error ((t (:foreground "#af0000" :background "#ffd7ff" :weight bold))))
   `(warning ((t (:foreground "#d75f00" :weight bold))))
   `(success ((t (:foreground "#718c00" :weight bold))))

   `(header-line ((t (:inherit mode-line :foreground "#d7005f" :background nil))))

   ;; Font-lock stuff
   `(font-lock-builtin-face ((t (:foreground "#3e999f"))))
   `(font-lock-comment-delimiter-face ((t (:foreground "#4d4d4c"))))
   `(font-lock-comment-face ((t (:foreground "#808080"))))
   `(font-lock-constant-face ((t (:foreground "#005f87"))))
   `(font-lock-doc-face ((t (:foreground "#949494"))))
   `(font-lock-doc-string-face ((t (:foreground "#808080"))))
   `(font-lock-function-name-face ((t (:foreground "#005f87"))))
   `(font-lock-keyword-face ((t (:foreground "#d7005f"))))
   `(font-lock-negation-char-face ((t (:foreground "#718c00"))))
   `(font-lock-preprocessor-face ((t (:foreground "#005f87"))))
   `(font-lock-regexp-grouping-backslash ((t (:foreground "#4271ae"))))
   `(font-lock-regexp-grouping-construct ((t (:foreground "#d7005f"))))
   `(font-lock-string-face ((t (:foreground "#718c00"))))
   `(font-lock-type-face ((t (:foreground "#4271ae"))))
   `(font-lock-variable-name-face ((t (:foreground "#3e999f"))))
   `(font-lock-warning-face ((t (:foreground "#8959a8"))))

   ;; linum-mode
   `(linum ((t (:background ,line-number-bg :foreground ,line-number-fg))))

   ;; Search
   `(match ((t (:foreground "#005f87" :background "#262626" :inverse-video t))))
   `(isearch ((t (:foreground "#4271ae" :background "#262626" :inverse-video t))))
   `(isearch-lazy-highlight-face ((t (:foreground "#3e999f" :background "#262626" :inverse-video t))))
   `(isearch-fail ((t (:background "#262626" :inherit font-lock-warning-face :inverse-video t))))
   `(evil-search-highlight-persist-highlight-face ((t (:background "#262626" :inherit font-lock-warning-face :inverse-video t))))

   ;; Popups
   `(popup-face ((t (:foreground "#4d4d4c" :background "#d0d0d0"))))
   `(popup-isearch-match ((t (:foreground "#eeeeee" :background "#718c00"))))
   `(popup-scroll-bar-background-face ((t (:background "#949494"))))
   `(popup-scroll-bar-foreground-face ((t (:background "#4d4d4c"))))
   `(popup-summary-face ((t (:foreground "#808080"))))
   `(popup-tip-face ((t (:foreground "#eeeeee" :background "#4271ae"))))
   `(popup-menu-mouse-face ((t (:foreground "#eeeeee" :background "#005f87"))))
   `(popup-menu-selection-face ((t (:foreground "#eeeeee" :background "#3e999f"))))

   ;; Flymake
   `(flymake-warnline ((t (:underline "#d75f00" :background "#262626"))))
   `(flymake-errline ((t (:underline "#8959a8" :background "#262626"))))

   ;; Clojure errors
   `(clojure-test-failure-face ((t (:background nil :inherit flymake-warnline))))
   `(clojure-test-error-face ((t (:background nil :inherit flymake-errline))))
   `(clojure-test-success-face ((t (:background nil :foreground nil :underline "#718c00"))))

   ;; For Brian Carper's extended clojure syntax table
   `(clojure-keyword ((t (:foreground "#4271ae"))))
   `(clojure-parens ((t (:foreground "#eeeeee"))))
   `(clojure-braces ((t (:foreground "#718c00"))))
   `(clojure-brackets ((t (:foreground "#4271ae"))))
   `(clojure-double-quote ((t (:foreground "#3e999f" :background nil))))
   `(clojure-special ((t (:foreground "#005f87"))))
   `(clojure-java-call ((t (:foreground "#d7005f"))))

   ;; MMM-mode
   `(mmm-code-submode-face ((t (:background "#808080"))))
   `(mmm-comment-submode-face ((t (:inherit font-lock-comment-face))))
   `(mmm-output-submode-face ((t (:background "#808080"))))

   ;; rainbow-delimiters
   `(rainbow-delimiters-depth-1-face ((t (:foreground "#0087af"))))
   `(rainbow-delimiters-depth-2-face ((t (:foreground "#d75f00"))))
   `(rainbow-delimiters-depth-3-face ((t (:foreground "#4271ae"))))
   `(rainbow-delimiters-depth-4-face ((t (:foreground "#8959a8"))))
   `(rainbow-delimiters-depth-5-face ((t (:foreground "#005f87"))))
   `(rainbow-delimiters-depth-6-face ((t (:foreground "#718c00"))))
   `(rainbow-delimiters-depth-7-face ((t (:foreground "#3e999f"))))
   `(rainbow-delimiters-depth-8-face ((t (:foreground "#808080"))))
   `(rainbow-delimiters-depth-9-face ((t (:foreground "#d7005f"))))

   ;; IDO
   `(ido-subdir ((t (:foreground "#949494"))))
   `(ido-first-match ((t (:foreground "#d75f00" :weight bold))))
   `(ido-only-match ((t (:foreground "#8959a8" :weight bold))))
   `(ido-indicator ((t (:foreground "#8959a8" :background "#262626"))))
   `(ido-virtual ((t (:foreground "#949494"))))

   ;; which-function
   `(which-func ((t (:foreground "#005f87" :background nil :weight bold))))

   `(trailing-whitespace ((t (:background "#3e999f" :foreground "#4271ae"))))
   `(whitespace-empty ((t (:foreground "#8959a8" :background "#4271ae"))))
   `(whitespace-hspace ((t (:background "#949494" :foreground "#949494"))))
   `(whitespace-indentation ((t (:background "#4271ae" :foreground "#8959a8"))))
   `(whitespace-line ((t (:background "#ffffaf" :foreground "#cc0000"))))
   `(whitespace-newline ((t (:foreground "#949494"))))
   `(whitespace-space ((t (:background "#262626" :foreground "#949494"))))
   `(whitespace-space-after-tab ((t (:background "#4271ae" :foreground "#8959a8"))))
   `(whitespace-space-before-tab ((t (:background "#d75f00" :foreground "#8959a8"))))
   `(whitespace-tab ((t (:background "#949494" :foreground "#949494"))))
   `(whitespace-trailing ((t (:background "#8959a8" :foreground "#4271ae"))))

   ;; Parenthesis matching (built-in)
   `(show-paren-match ((t (:background "#949494" :foreground "#005f87"))))
   `(show-paren-mismatch ((t (:background "#949494" :foreground "#d75f00"))))

   ;; Parenthesis matching (mic-paren)
   `(paren-face-match ((t (:foreground nil :background nil :inherit show-paren-match))))
   `(paren-face-mismatch ((t (:foreground nil :background nil :inherit show-paren-mismatch))))
   `(paren-face-no-match ((t (:foreground nil :background nil :inherit show-paren-mismatch))))

   ;; Parenthesis dimming (parenface)
   `(paren-face ((t (:foreground "#949494" :background nil))))

   `(sh-heredoc ((t (:foreground nil :inherit font-lock-string-face :weight normal))))
   `(sh-quoted-exec ((t (:foreground nil :inherit font-lock-preprocessor-face))))
   `(slime-highlight-edits-face ((t (:weight bold))))
   `(slime-repl-input-face ((t (:weight normal :underline nil))))
   `(slime-repl-prompt-face ((t (:underline nil :weight bold :foreground "#d7005f"))))
   `(slime-repl-result-face ((t (:foreground "#718c00"))))
   `(slime-repl-output-face ((t (:foreground "#005f87" :background "#262626"))))

   `(csv-separator-face ((t (:foreground "#d75f00"))))

   `(diff-added ((t (:foreground "#718c00"))))
   `(diff-changed ((t (:foreground "#4271ae"))))
   `(diff-removed ((t (:foreground "#8959a8"))))
   `(diff-header ((t (:background "#262626"))))
   `(diff-file-header ((t (:background "#4d4d4c"))))
   `(diff-hunk-header ((t (:background "#262626" :foreground "#d7005f"))))

   `(diff-hl-change ((t (:foreground "#4271ae"))))
   `(diff-hl-delete ((t (:foreground "#8959a8"))))
   `(diff-hl-insert ((t (:foreground "#718c00"))))

   `(ediff-even-diff-A ((t (:foreground nil :background nil :inverse-video t))))
   `(ediff-even-diff-B ((t (:foreground nil :background nil :inverse-video t))))
   `(ediff-odd-diff-A  ((t (:foreground "#949494" :background nil :inverse-video t))))
   `(ediff-odd-diff-B  ((t (:foreground "#949494" :background nil :inverse-video t))))

   `(eldoc-highlight-function-argument ((t (:foreground "#718c00" :weight bold))))

   ;; undo-tree
   `(undo-tree-visualizer-default-face ((t (:foreground "#eeeeee"))))
   `(undo-tree-visualizer-current-face ((t (:foreground "#718c00" :weight bold))))
   `(undo-tree-visualizer-active-branch-face ((t (:foreground "#8959a8"))))
   `(undo-tree-visualizer-register-face ((t (:foreground "#4271ae"))))

   ;; auctex
   `(font-latex-bold-face ((t (:foreground "#718c00"))))
   `(font-latex-doctex-documentation-face ((t (:background "#808080"))))
   `(font-latex-italic-face ((t (:foreground "#718c00"))))
   `(font-latex-math-face ((t (:foreground "#d75f00"))))
   `(font-latex-sectioning-0-face ((t (:foreground "#4271ae"))))
   `(font-latex-sectioning-1-face ((t (:foreground "#4271ae"))))
   `(font-latex-sectioning-2-face ((t (:foreground "#4271ae"))))
   `(font-latex-sectioning-3-face ((t (:foreground "#4271ae"))))
   `(font-latex-sectioning-4-face ((t (:foreground "#4271ae"))))
   `(font-latex-sectioning-5-face ((t (:foreground "#4271ae"))))
   `(font-latex-sedate-face ((t (:foreground "#3e999f"))))
   `(font-latex-string-face ((t (:foreground "#4271ae"))))
   `(font-latex-verbatim-face ((t (:foreground "#d75f00"))))
   `(font-latex-warning-face ((t (:foreground "#8959a8"))))

   ;; dired+
   `(diredp-compressed-file-suffix ((t (:foreground "#005f87"))))
   `(diredp-dir-heading ((t (:foreground nil :background nil :inherit heading))))
   `(diredp-dir-priv ((t (:foreground "#3e999f" :background nil))))
   `(diredp-exec-priv ((t (:foreground "#005f87" :background nil))))
   `(diredp-executable-tag ((t (:foreground "#8959a8" :background nil))))
   `(diredp-file-name ((t (:foreground "#4271ae"))))
   `(diredp-file-suffix ((t (:foreground "#718c00"))))
   `(diredp-flag-mark-line ((t (:background nil :inherit highlight))))
   `(diredp-ignored-file-name ((t (:foreground "#949494"))))
   `(diredp-link-priv ((t (:background nil :foreground "#d7005f"))))
   `(diredp-mode-line-flagged ((t (:foreground "#8959a8"))))
   `(diredp-mode-line-marked ((t (:foreground "#718c00"))))
   `(diredp-no-priv ((t (:background nil))))
   `(diredp-number ((t (:foreground "#4271ae"))))
   `(diredp-other-priv ((t (:background nil :foreground "#d7005f"))))
   `(diredp-rare-priv ((t (:foreground "#8959a8" :background nil))))
   `(diredp-read-priv ((t (:foreground "#718c00" :background nil))))
   `(diredp-symlink ((t (:foreground "#d7005f"))))
   `(diredp-write-priv ((t (:foreground "#4271ae" :background nil))))

   ;; term and ansi-term
   `(term-color-black ((t (:foreground "#4d4d4c" :background "#262626"))))
   `(term-color-white ((t (:foreground "#d0d0d0" :background "#eeeeee"))))
   `(term-color-red ((t (:foreground "#8959a8" :background "#8959a8"))))
   `(term-color-yellow ((t (:foreground "#4271ae" :background "#4271ae"))))
   `(term-color-green ((t (:foreground "#718c00" :background "#718c00"))))
   `(term-color-cyan ((t (:foreground "#3e999f" :background "#3e999f"))))
   `(term-color-blue ((t (:foreground "#005f87" :background "#005f87"))))
   `(term-color-magenta ((t (:foreground "#d7005f" :background "#d7005f"))))

   `(link ((t (:foreground nil :underline t))))
   `(widget-button ((t (:underline t))))
   `(widget-field ((t (:background "#808080" :box (:line-width 1 :color "#eeeeee")))))

   ;; Compilation (most faces politely inherit from 'success, 'error, 'warning etc.)
   `(compilation-column-number ((t (:foreground "#4271ae"))))
   `(compilation-line-number ((t (:foreground "#4271ae"))))
   `(compilation-message-face ((t (:foreground "#005f87"))))
   `(compilation-mode-line-exit ((t (:foreground "#718c00"))))
   `(compilation-mode-line-fail ((t (:foreground "#8959a8"))))
   `(compilation-mode-line-run ((t (:foreground "#005f87"))))

   ;; Grep
   `(grep-context-face ((t (:foreground "#949494"))))
   `(grep-error-face ((t (:foreground "#8959a8" :weight bold :underline t))))
   `(grep-hit-face ((t (:foreground "#005f87"))))
   `(grep-match-face ((t (:foreground nil :background nil :inherit match))))

   `(regex-tool-matched-face ((t (:foreground nil :background nil :inherit match))))

   ;; mark-multiple
   `(mm/master-face ((t (:inherit region :foreground nil :background nil))))
   `(mm/mirror-face ((t (:inherit region :foreground nil :background nil))))

   ;; org-mode
   `(org-agenda-structure ((t (:foreground "#d7005f"))))
   `(org-agenda-date ((t (:foreground "#005f87" :underline nil))))
   `(org-agenda-done ((t (:foreground "#718c00"))))
   `(org-agenda-dimmed-todo-face ((t (:foreground "#949494"))))
   `(org-block ((t (:foreground "#d75f00"))))
   `(org-code ((t (:foreground "#4271ae"))))
   `(org-column ((t (:background "#808080"))))
   `(org-column-title ((t (:inherit org-column :weight bold :underline t))))
   `(org-date ((t (:foreground "#d7005f" :underline t))))
   `(org-document-info ((t (:foreground "#3e999f"))))
   `(org-document-info-keyword ((t (:foreground "#718c00"))))
   `(org-document-title ((t (:weight bold :foreground "#d75f00" :height 1.44))))
   `(org-done ((t (:foreground "#718c00"))))
   `(org-ellipsis ((t (:foreground "#949494"))))
   `(org-footnote ((t (:foreground "#3e999f"))))
   `(org-formula ((t (:foreground "#8959a8"))))
   `(org-hide ((t (:foreground "#808080"))))
   `(org-link ((t (:foreground "#005f87"))))
   `(org-scheduled ((t (:foreground "#718c00"))))
   `(org-scheduled-previously ((t (:foreground "#d75f00"))))
   `(org-scheduled-today ((t (:foreground "#718c00"))))
   `(org-special-keyword ((t (:foreground "#d75f00"))))
   `(org-table ((t (:foreground "#d7005f"))))
   `(org-todo ((t (:foreground "#8959a8"))))
   `(org-upcoming-deadline ((t (:foreground "#d75f00"))))
   `(org-warning ((t (:weight bold :foreground "#8959a8"))))

   `(markdown-url-face ((t (:inherit link))))
   `(markdown-link-face ((t (:foreground "#005f87" :underline t))))

   `(hl-sexp-face ((t (:background "#808080"))))
   `(highlight-80+ ((t (:background "#808080"))))

   ;; Python-specific overrides
   `(py-builtins-face ((t (:foreground "#d75f00" :weight normal))))

   ;; js2-mode
   `(js2-warning-face ((t (:underline "#d75f00"))))
   `(js2-error-face ((t (:foreground nil :underline "#8959a8"))))
   `(js2-external-variable-face ((t (:foreground "#d7005f"))))
   `(js2-function-param-face ((t (:foreground "#005f87"))))
   `(js2-instance-member-face ((t (:foreground "#005f87"))))
   `(js2-private-function-call-face ((t (:foreground "#8959a8"))))

   ;; js3-mode
   `(js3-warning-face ((t (:underline "#d75f00"))))
   `(js3-error-face ((t (:foreground nil :underline "#8959a8"))))
   `(js3-external-variable-face ((t (:foreground "#d7005f"))))
   `(js3-function-param-face ((t (:foreground "#005f87"))))
   `(js3-jsdoc-tag-face ((t (:foreground "#d75f00"))))
   `(js3-jsdoc-type-face ((t (:foreground "#3e999f"))))
   `(js3-jsdoc-value-face ((t (:foreground "#4271ae"))))
   `(js3-jsdoc-html-tag-name-face ((t (:foreground "#005f87"))))
   `(js3-jsdoc-html-tag-delimiter-face ((t (:foreground "#718c00"))))
   `(js3-instance-member-face ((t (:foreground "#005f87"))))
   `(js3-private-function-call-face ((t (:foreground "#8959a8"))))

   ;; nxml
   `(nxml-name-face ((t (:foreground unspecified :inherit font-lock-constant-face))))
   `(nxml-attribute-local-name-face ((t (:foreground unspecified :inherit font-lock-variable-name-face))))
   `(nxml-ref-face ((t (:foreground unspecified :inherit font-lock-preprocessor-face))))
   `(nxml-delimiter-face ((t (:foreground unspecified :inherit font-lock-keyword-face))))
   `(nxml-delimited-data-face ((t (:foreground unspecified :inherit font-lock-string-face))))
   `(rng-error-face ((t (:underline "#8959a8"))))

   ;; RHTML
   `(erb-delim-face ((t (:background "#808080"))))
   `(erb-exec-face ((t (:background "#808080" :weight bold))))
   `(erb-exec-delim-face ((t (:background "#808080"))))
   `(erb-out-face ((t (:background "#808080" :weight bold))))
   `(erb-out-delim-face ((t (:background "#808080"))))
   `(erb-comment-face ((t (:background "#808080" :weight bold :slant italic))))
   `(erb-comment-delim-face ((t (:background "#808080"))))

   ;; Message-mode
   `(message-header-other ((t (:foreground nil :background nil :weight normal))))
   `(message-header-subject ((t (:inherit message-header-other :weight bold :foreground "#4271ae"))))
   `(message-header-to ((t (:inherit message-header-other :weight bold :foreground "#d75f00"))))
   `(message-header-cc ((t (:inherit message-header-to :foreground nil))))
   `(message-header-name ((t (:foreground "#005f87" :background nil))))
   `(message-header-newsgroups ((t (:foreground "#3e999f" :background nil :slant normal))))
   `(message-separator ((t (:foreground "#d7005f"))))

   ;; Jabber
   `(jabber-chat-prompt-local ((t (:foreground "#4271ae"))))
   `(jabber-chat-prompt-foreign ((t (:foreground "#d75f00"))))
   `(jabber-chat-prompt-system ((t (:foreground "#4271ae" :weight bold))))
   `(jabber-chat-text-local ((t (:foreground "#4271ae"))))
   `(jabber-chat-text-foreign ((t (:foreground "#d75f00"))))
   `(jabber-chat-text-error ((t (:foreground "#8959a8"))))

   `(jabber-roster-user-online ((t (:foreground "#718c00"))))
   `(jabber-roster-user-xa ((t :foreground "#949494")))
   `(jabber-roster-user-dnd ((t :foreground "#4271ae")))
   `(jabber-roster-user-away ((t (:foreground "#d75f00"))))
   `(jabber-roster-user-chatty ((t (:foreground "#d7005f"))))
   `(jabber-roster-user-error ((t (:foreground "#8959a8"))))
   `(jabber-roster-user-offline ((t (:foreground "#949494"))))

   `(jabber-rare-time-face ((t (:foreground "#949494"))))
   `(jabber-activity-face ((t (:foreground "#d7005f"))))
   `(jabber-activity-personal-face ((t (:foreground "#3e999f"))))

   ;; Gnus
   `(gnus-cite-1 ((t (:inherit outline-1 :foreground nil))))
   `(gnus-cite-2 ((t (:inherit outline-2 :foreground nil))))
   `(gnus-cite-3 ((t (:inherit outline-3 :foreground nil))))
   `(gnus-cite-4 ((t (:inherit outline-4 :foreground nil))))
   `(gnus-cite-5 ((t (:inherit outline-5 :foreground nil))))
   `(gnus-cite-6 ((t (:inherit outline-6 :foreground nil))))
   `(gnus-cite-7 ((t (:inherit outline-7 :foreground nil))))
   `(gnus-cite-8 ((t (:inherit outline-8 :foreground nil))))
   ;; there are several more -cite- faces...
   `(gnus-header-content ((t (:inherit message-header-other))))
   `(gnus-header-subject ((t (:inherit message-header-subject))))
   `(gnus-header-from ((t (:inherit message-header-other-face :weight bold :foreground "#d75f00"))))
   `(gnus-header-name ((t (:inherit message-header-name))))
   `(gnus-button ((t (:inherit link :foreground nil))))
   `(gnus-signature ((t (:inherit font-lock-comment-face))))

   `(gnus-summary-normal-unread ((t (:foreground "#005f87" :weight normal))))
   `(gnus-summary-normal-read ((t (:foreground "#eeeeee" :weight normal))))
   `(gnus-summary-normal-ancient ((t (:foreground "#3e999f" :weight normal))))
   `(gnus-summary-normal-ticked ((t (:foreground "#d75f00" :weight normal))))
   `(gnus-summary-low-unread ((t (:foreground "#949494" :weight normal))))
   `(gnus-summary-low-read ((t (:foreground "#949494" :weight normal))))
   `(gnus-summary-low-ancient ((t (:foreground "#949494" :weight normal))))
   `(gnus-summary-high-unread ((t (:foreground "#4271ae" :weight normal))))
   `(gnus-summary-high-read ((t (:foreground "#718c00" :weight normal))))
   `(gnus-summary-high-ancient ((t (:foreground "#718c00" :weight normal))))
   `(gnus-summary-high-ticked ((t (:foreground "#d75f00" :weight normal))))
   `(gnus-summary-cancelled ((t (:foreground "#8959a8" :background nil :weight normal))))

   `(gnus-group-mail-low ((t (:foreground "#949494"))))
   `(gnus-group-mail-low-empty ((t (:foreground "#949494"))))
   `(gnus-group-mail-1 ((t (:foreground nil :weight normal :inherit outline-1))))
   `(gnus-group-mail-2 ((t (:foreground nil :weight normal :inherit outline-2))))
   `(gnus-group-mail-3 ((t (:foreground nil :weight normal :inherit outline-3))))
   `(gnus-group-mail-4 ((t (:foreground nil :weight normal :inherit outline-4))))
   `(gnus-group-mail-5 ((t (:foreground nil :weight normal :inherit outline-5))))
   `(gnus-group-mail-6 ((t (:foreground nil :weight normal :inherit outline-6))))
   `(gnus-group-mail-1-empty ((t (:inherit gnus-group-mail-1 :foreground "#949494"))))
   `(gnus-group-mail-2-empty ((t (:inherit gnus-group-mail-2 :foreground "#949494"))))
   `(gnus-group-mail-3-empty ((t (:inherit gnus-group-mail-3 :foreground "#949494"))))
   `(gnus-group-mail-4-empty ((t (:inherit gnus-group-mail-4 :foreground "#949494"))))
   `(gnus-group-mail-5-empty ((t (:inherit gnus-group-mail-5 :foreground "#949494"))))
   `(gnus-group-mail-6-empty ((t (:inherit gnus-group-mail-6 :foreground "#949494"))))
   `(gnus-group-news-1 ((t (:foreground nil :weight normal :inherit outline-5))))
   `(gnus-group-news-2 ((t (:foreground nil :weight normal :inherit outline-6))))
   `(gnus-group-news-3 ((t (:foreground nil :weight normal :inherit outline-7))))
   `(gnus-group-news-4 ((t (:foreground nil :weight normal :inherit outline-8))))
   `(gnus-group-news-5 ((t (:foreground nil :weight normal :inherit outline-1))))
   `(gnus-group-news-6 ((t (:foreground nil :weight normal :inherit outline-2))))
   `(gnus-group-news-1-empty ((t (:inherit gnus-group-news-1 :foreground "#949494"))))
   `(gnus-group-news-2-empty ((t (:inherit gnus-group-news-2 :foreground "#949494"))))
   `(gnus-group-news-3-empty ((t (:inherit gnus-group-news-3 :foreground "#949494"))))
   `(gnus-group-news-4-empty ((t (:inherit gnus-group-news-4 :foreground "#949494"))))
   `(gnus-group-news-5-empty ((t (:inherit gnus-group-news-5 :foreground "#949494"))))
   `(gnus-group-news-6-empty ((t (:inherit gnus-group-news-6 :foreground "#949494"))))

   `(erc-direct-msg-face ((t (:foreground "#d75f00"))))
   `(erc-error-face ((t (:foreground "#8959a8"))))
   `(erc-header-face ((t (:foreground "#eeeeee" :background "#949494"))))
   `(erc-input-face ((t (:foreground "#718c00"))))
   `(erc-keyword-face ((t (:foreground "#4271ae"))))
   `(erc-current-nick-face ((t (:foreground "#718c00"))))
   `(erc-my-nick-face ((t (:foreground "#718c00"))))
   `(erc-nick-default-face ((t (:weight normal :foreground "#d7005f"))))
   `(erc-nick-msg-face ((t (:weight normal :foreground "#4271ae"))))
   `(erc-notice-face ((t (:foreground "#949494"))))
   `(erc-pal-face ((t (:foreground "#d75f00"))))
   `(erc-prompt-face ((t (:foreground "#005f87"))))
   `(erc-timestamp-face ((t (:foreground "#3e999f"))))

   ;; helm
   `(helm-M-x-key ((t (:foreground "#3e999f"))))
   `(helm-action ((t (:foreground "#d0d0d0"))))
   `(helm-buffer-directory ((t (:foreground "#4271ae" :background nil :weight bold))))
   `(helm-buffer-file ((t (:foreground "#3e999f"))))
   `(helm-buffer-not-saved ((t (:foreground "#8959a8"))))
   `(helm-buffer-process ((t (:foreground "#808080"))))
   `(helm-buffer-saved-out ((t (:foreground "#d70000"))))
   `(helm-buffer-size ((t (:foreground "#d75f00"))))
   `(helm-candidate-number ((t (:foreground "#262626" :background "#d75f00"))))
   `(helm-ff-directory ((t (:foreground "#4271ae" :background nil :weight bold))))
   `(helm-ff-executable ((t (:foreground "#718c00"))))
   `(helm-ff-file ((t (:foreground "#3e999f"))))
   `(helm-ff-invalid-symlink ((t (:foreground "#262626" :background "#8959a8"))))
   `(helm-ff-prefix ((t (:foreground nil :background nil))))
   `(helm-ff-symlink ((t (:foreground "#262626" :background "#3e999f"))))
   `(helm-grep-cmd-line ((t (:foreground "#718c00"))))
   `(helm-grep-file ((t (:foreground "#3e999f"))))
   `(helm-grep-finish ((t (:foreground "#262626" :background "#d75f00"))))
   `(helm-grep-lineno ((t (:foreground "#808080"))))
   `(helm-grep-match ((t (:foreground "#4271ae"))))
   `(helm-grep-running ((t (:foreground "#d75f00"))))
   `(helm-header ((t (:foreground "#e4e4e4" :background "#8959a8" :underline nil))))
   `(helm-match ((t (:foreground "#4271ae"))))
   `(helm-moccur-buffer ((t (:foreground "#e4e4e4"))))
   `(helm-selection ((t (:foreground nil :background "#e4e4e4" :underline nil))))
   `(helm-selection-line ((t (:foreground nil :background "#4d4d4c"))))
   `(helm-separator ((t (:foreground "#4d4d4c"))))
   `(helm-source-header ((t (:foreground "#e4e4e4" :background "#3e999f" :weight bold))))
   `(helm-visible-mark ((t (:foreground "#262626" :background "#718c00"))))

   `(custom-variable-tag ((t (:foreground "#005f87"))))
   `(custom-group-tag ((t (:foreground "#005f87"))))
   `(custom-state ((t (:foreground "#718c00")))))


  (custom-theme-set-variables
   'papercolor-light

   `(ansi-color-names-vector
     ;; black, base08, base0B, base0A, base0D, magenta, cyan, white
     ["#262626" "#8959a8" "#718c00" "#4271ae" "#005f87" "#d7005f" "#005f87" "#d0d0d0"])
   `(ansi-term-color-vector
     ;; black, base08, base0B, base0A, base0D, magenta, cyan, white
     [unspecified "#262626" "#8959a8" "#718c00" "#4271ae" "#005f87" "#d7005f" "#005f87" "#d0d0d0"])))

;;;###autoload
(and load-file-name
     (boundp 'custom-theme-load-path)
     (add-to-list 'custom-theme-load-path
                  (file-name-as-directory
                   (file-name-directory load-file-name))))

(provide-theme 'papercolor-light)

;;; papercolor-light-theme.el ends here
