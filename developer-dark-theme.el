;;; developer-dark-theme.el --- A balanced dark theme for C, Python, and QML -*- lexical-binding: t -*-

;; Copyright (C) 2024

;; Author: Developer
;; Version: 1.0
;; Package-Requires: ((emacs "24.3"))
;; Keywords: faces, themes

;;; Commentary:

;; A carefully crafted dark theme with balanced contrast.
;; Optimized for C/C++, Python, and QML development.
;; Not too faded, not too extreme - just right.

;;; Code:

(deftheme developer-dark
  "A balanced dark theme for programming.")

(let ((class '((class color) (min-colors 89)))
      ;; Base colors
      (bg-main     "#1a1d23")      ; Dark blue-grey background
      (bg-dim      "#14171c")      ; Darker for modeline inactive
      (bg-active   "#252930")      ; Lighter for selections
      (bg-region   "#2d3640")      ; Selection background
      (bg-hl-line  "#222630")      ; Current line highlight
      
      ;; Foreground colors
      (fg-main     "#d4d8df")      ; Main text - good readability
      (fg-dim      "#969ba7")      ; Comments, less important
      (fg-bright   "#e8ecf2")      ; Emphasized text
      
      ;; Syntax colors - balanced, not too bright
      (red         "#e06c75")      ; Errors, important warnings
      (red-bright  "#ff7a85")      ; Bright red for critical
      (green       "#98c379")      ; Strings, success
      (green-bright "#b5e890")     ; Bright green
      (yellow      "#e5c07b")      ; Warnings, special keywords
      (yellow-bright "#ffd787")    ; Bright yellow
      (blue        "#61afef")      ; Functions, primary keywords
      (blue-bright "#74c7ff")      ; Bright blue
      (magenta     "#c678dd")      ; Constants, special syntax
      (magenta-bright "#e198ff")   ; Bright magenta
      (cyan        "#56b6c2")      ; Types, classes
      (cyan-bright "#7fd8e8")      ; Bright cyan
      (orange      "#d19a66")      ; Numbers, preprocessor
      (purple      "#a984d4")      ; Special identifiers
      
      ;; UI colors
      (border      "#3a4049")      ; Borders, separators
      (cursor      "#61afef")      ; Cursor color
      (fringe      "#1e2228")      ; Fringe background
      (modeline-bg "#232830")      ; Active modeline
      (modeline-fg "#b8bcc5"))     ; Modeline text

  (custom-theme-set-faces
   'developer-dark
   
   ;; Basic faces
   `(default ((,class (:background ,bg-main :foreground ,fg-main))))
   `(cursor ((,class (:background ,cursor))))
   `(region ((,class (:background ,bg-region :extend t))))
   `(highlight ((,class (:background ,bg-active))))
   `(hl-line ((,class (:background ,bg-hl-line :extend t))))
   `(fringe ((,class (:background ,fringe :foreground ,fg-dim))))
   `(vertical-border ((,class (:foreground ,border))))
   `(window-divider ((,class (:foreground ,border))))
   `(window-divider-first-pixel ((,class (:foreground ,border))))
   `(window-divider-last-pixel ((,class (:foreground ,border))))
   
   ;; Font lock faces - syntax highlighting
   `(font-lock-builtin-face ((,class (:foreground ,blue))))
   `(font-lock-comment-face ((,class (:foreground ,fg-dim :slant italic))))
   `(font-lock-comment-delimiter-face ((,class (:foreground ,fg-dim))))
   `(font-lock-constant-face ((,class (:foreground ,magenta))))
   `(font-lock-doc-face ((,class (:foreground ,green))))
   `(font-lock-function-name-face ((,class (:foreground ,blue :weight medium))))
   `(font-lock-keyword-face ((,class (:foreground ,purple :weight medium))))
   `(font-lock-negation-char-face ((,class (:foreground ,red))))
   `(font-lock-preprocessor-face ((,class (:foreground ,orange))))
   `(font-lock-regexp-grouping-backslash ((,class (:foreground ,yellow))))
   `(font-lock-regexp-grouping-construct ((,class (:foreground ,purple))))
   `(font-lock-string-face ((,class (:foreground ,green))))
   `(font-lock-type-face ((,class (:foreground ,cyan))))
   `(font-lock-variable-name-face ((,class (:foreground ,fg-main))))
   `(font-lock-warning-face ((,class (:foreground ,yellow :weight bold))))
   
   ;; C/C++ specific
   `(c-annotation-face ((,class (:foreground ,magenta))))
   
   ;; Line numbers
   `(line-number ((,class (:background ,bg-main :foreground ,fg-dim))))
   `(line-number-current-line ((,class (:background ,bg-hl-line :foreground ,yellow :weight bold))))
   
   ;; Mode line
   `(mode-line ((,class (:background ,modeline-bg :foreground ,modeline-fg :box (:line-width 1 :color ,border)))))
   `(mode-line-inactive ((,class (:background ,bg-dim :foreground ,fg-dim :box (:line-width 1 :color ,border)))))
   `(mode-line-buffer-id ((,class (:weight bold :foreground ,blue))))
   `(mode-line-emphasis ((,class (:foreground ,fg-bright :weight bold))))
   `(mode-line-highlight ((,class (:background ,bg-active))))
   
   ;; Search and replace
   `(isearch ((,class (:background ,yellow :foreground ,bg-main :weight bold))))
   `(isearch-fail ((,class (:background ,red :foreground ,fg-bright))))
   `(lazy-highlight ((,class (:background ,bg-active :foreground ,yellow))))
   `(match ((,class (:background ,green :foreground ,bg-main))))
   
   ;; Diff-hl (git changes in fringe)
   `(diff-hl-change ((,class (:foreground ,blue :background ,blue))))
   `(diff-hl-delete ((,class (:foreground ,red :background ,red))))
   `(diff-hl-insert ((,class (:foreground ,green :background ,green))))
   
   ;; Diff mode
   `(diff-added ((,class (:background "#1e3a28" :foreground ,green :extend t))))
   `(diff-removed ((,class (:background "#3a1e28" :foreground ,red :extend t))))
   `(diff-changed ((,class (:background "#2a2a3a" :extend t))))
   `(diff-header ((,class (:background ,bg-dim :foreground ,fg-bright :weight bold))))
   `(diff-file-header ((,class (:background ,bg-dim :foreground ,blue :weight bold))))
   `(diff-hunk-header ((,class (:background ,bg-active :foreground ,purple))))
   
   ;; Company (auto-completion)
   `(company-tooltip ((,class (:background ,bg-active :foreground ,fg-main))))
   `(company-tooltip-selection ((,class (:background ,blue :foreground ,bg-main))))
   `(company-tooltip-common ((,class (:foreground ,yellow :weight bold))))
   `(company-tooltip-common-selection ((,class (:foreground ,bg-main :weight bold))))
   `(company-tooltip-annotation ((,class (:foreground ,cyan))))
   `(company-tooltip-annotation-selection ((,class (:foreground ,bg-main))))
   `(company-scrollbar-bg ((,class (:background ,bg-dim))))
   `(company-scrollbar-fg ((,class (:background ,border))))
   `(company-preview ((,class (:background ,bg-active :foreground ,green))))
   `(company-preview-common ((,class (:background ,bg-active :foreground ,yellow))))
   
   ;; Helm
   `(helm-header ((,class (:background ,bg-dim :foreground ,fg-main :weight bold :extend t))))
   `(helm-source-header ((,class (:background ,bg-active :foreground ,fg-bright :weight bold :extend t))))
   `(helm-selection ((,class (:background ,bg-region :weight bold :extend t))))
   `(helm-visible-mark ((,class (:background ,blue :foreground ,bg-main))))
   `(helm-candidate-number ((,class (:foreground ,green :weight bold))))
   `(helm-separator ((,class (:foreground ,red))))
   `(helm-match ((,class (:foreground ,yellow :weight bold))))
   `(helm-M-x-key ((,class (:foreground ,orange :weight bold))))
   `(helm-buffer-not-saved ((,class (:foreground ,red))))
   `(helm-buffer-process ((,class (:foreground ,magenta))))
   `(helm-buffer-saved-out ((,class (:foreground ,yellow))))
   `(helm-buffer-size ((,class (:foreground ,fg-dim))))
   `(helm-buffer-directory ((,class (:foreground ,blue :weight bold))))
   `(helm-buffer-file ((,class (:foreground ,fg-main))))
   `(helm-ff-directory ((,class (:foreground ,blue :weight bold))))
   `(helm-ff-file ((,class (:foreground ,fg-main))))
   `(helm-ff-executable ((,class (:foreground ,green))))
   `(helm-ff-symlink ((,class (:foreground ,cyan))))
   `(helm-ff-prefix ((,class (:background ,yellow :foreground ,bg-main))))
   
   ;; Treemacs
   `(treemacs-directory-face ((,class (:foreground ,blue))))
   `(treemacs-file-face ((,class (:foreground ,fg-main))))
   `(treemacs-root-face ((,class (:foreground ,green :weight bold))))
   `(treemacs-git-modified-face ((,class (:foreground ,orange))))
   `(treemacs-git-added-face ((,class (:foreground ,green))))
   `(treemacs-git-untracked-face ((,class (:foreground ,fg-dim))))
   `(treemacs-git-renamed-face ((,class (:foreground ,yellow))))
   `(treemacs-git-deleted-face ((,class (:foreground ,red))))
   
   ;; Magit
   `(magit-branch-current ((,class (:foreground ,green :weight bold))))
   `(magit-branch-local ((,class (:foreground ,blue))))
   `(magit-branch-remote ((,class (:foreground ,magenta))))
   `(magit-diff-added ((,class (:background "#1e3a28" :foreground ,green :extend t))))
   `(magit-diff-added-highlight ((,class (:background "#2a4a34" :foreground ,green :extend t))))
   `(magit-diff-removed ((,class (:background "#3a1e28" :foreground ,red :extend t))))
   `(magit-diff-removed-highlight ((,class (:background "#4a2a34" :foreground ,red :extend t))))
   `(magit-diff-context ((,class (:foreground ,fg-dim :extend t))))
   `(magit-diff-context-highlight ((,class (:background ,bg-hl-line :foreground ,fg-main :extend t))))
   `(magit-section-heading ((,class (:foreground ,blue :weight bold))))
   `(magit-section-highlight ((,class (:background ,bg-hl-line :extend t))))
   `(magit-hash ((,class (:foreground ,fg-dim))))
   
   ;; LSP
   `(lsp-face-highlight-textual ((,class (:background ,bg-active))))
   `(lsp-face-highlight-read ((,class (:background ,bg-active))))
   `(lsp-face-highlight-write ((,class (:background ,bg-region))))
   `(lsp-ui-doc-background ((,class (:background ,bg-active))))
   `(lsp-ui-doc-border ((,class (:foreground ,border))))
   `(lsp-ui-doc-header ((,class (:background ,blue :foreground ,bg-main :weight bold))))
   `(lsp-ui-peek-peek ((,class (:background ,bg-active))))
   `(lsp-ui-peek-list ((,class (:background ,bg-dim))))
   `(lsp-ui-peek-filename ((,class (:foreground ,orange :weight bold))))
   `(lsp-ui-peek-selection ((,class (:background ,bg-region))))
   `(lsp-ui-peek-highlight ((,class (:background ,yellow :foreground ,bg-main))))
   `(lsp-ui-sideline-code-action ((,class (:foreground ,yellow))))
   
   ;; Flycheck
   `(flycheck-error ((,class (:underline (:style wave :color ,red)))))
   `(flycheck-warning ((,class (:underline (:style wave :color ,yellow)))))
   `(flycheck-info ((,class (:underline (:style wave :color ,cyan)))))
   `(flycheck-fringe-error ((,class (:foreground ,red))))
   `(flycheck-fringe-warning ((,class (:foreground ,yellow))))
   `(flycheck-fringe-info ((,class (:foreground ,cyan))))
   
   ;; Rainbow delimiters
   `(rainbow-delimiters-depth-1-face ((,class (:foreground ,blue))))
   `(rainbow-delimiters-depth-2-face ((,class (:foreground ,green))))
   `(rainbow-delimiters-depth-3-face ((,class (:foreground ,yellow))))
   `(rainbow-delimiters-depth-4-face ((,class (:foreground ,cyan))))
   `(rainbow-delimiters-depth-5-face ((,class (:foreground ,magenta))))
   `(rainbow-delimiters-depth-6-face ((,class (:foreground ,orange))))
   `(rainbow-delimiters-depth-7-face ((,class (:foreground ,blue-bright))))
   `(rainbow-delimiters-depth-8-face ((,class (:foreground ,green-bright))))
   `(rainbow-delimiters-depth-9-face ((,class (:foreground ,red-bright))))
   
   ;; Org mode
   `(org-level-1 ((,class (:foreground ,blue :weight bold))))
   `(org-level-2 ((,class (:foreground ,green :weight bold))))
   `(org-level-3 ((,class (:foreground ,yellow :weight bold))))
   `(org-level-4 ((,class (:foreground ,cyan :weight bold))))
   `(org-level-5 ((,class (:foreground ,magenta :weight bold))))
   `(org-level-6 ((,class (:foreground ,orange :weight bold))))
   `(org-level-7 ((,class (:foreground ,purple :weight bold))))
   `(org-level-8 ((,class (:foreground ,fg-main :weight bold))))
   `(org-link ((,class (:foreground ,blue :underline t))))
   `(org-code ((,class (:background ,bg-active :foreground ,orange))))
   `(org-block ((,class (:background ,bg-dim :extend t))))
   `(org-block-begin-line ((,class (:background ,bg-dim :foreground ,fg-dim :extend t))))
   `(org-block-end-line ((,class (:background ,bg-dim :foreground ,fg-dim :extend t))))
   
   ;; Markdown
   `(markdown-header-face-1 ((,class (:foreground ,blue :weight bold))))
   `(markdown-header-face-2 ((,class (:foreground ,green :weight bold))))
   `(markdown-header-face-3 ((,class (:foreground ,yellow :weight bold))))
   `(markdown-header-face-4 ((,class (:foreground ,cyan :weight bold))))
   `(markdown-header-face-5 ((,class (:foreground ,magenta :weight bold))))
   `(markdown-header-face-6 ((,class (:foreground ,orange :weight bold))))
   `(markdown-code-face ((,class (:background ,bg-active :foreground ,orange))))
   `(markdown-inline-code-face ((,class (:background ,bg-active :foreground ,orange))))
   `(markdown-link-face ((,class (:foreground ,blue :underline t))))
   `(markdown-url-face ((,class (:foreground ,cyan :underline t))))
   
   ;; Which-key
   `(which-key-key-face ((,class (:foreground ,green :weight bold))))
   `(which-key-separator-face ((,class (:foreground ,fg-dim))))
   `(which-key-note-face ((,class (:foreground ,fg-dim :slant italic))))
   `(which-key-command-description-face ((,class (:foreground ,fg-main))))
   `(which-key-group-description-face ((,class (:foreground ,purple))))
   
   ;; Minibuffer
   `(minibuffer-prompt ((,class (:foreground ,blue :weight bold))))
   `(completions-common-part ((,class (:foreground ,yellow :weight bold))))
   `(completions-first-difference ((,class (:foreground ,orange :weight bold))))
   
   ;; Messages
   `(success ((,class (:foreground ,green :weight bold))))
   `(warning ((,class (:foreground ,yellow :weight bold))))
   `(error ((,class (:foreground ,red :weight bold))))
   
   ;; Links
   `(link ((,class (:foreground ,blue :underline t))))
   `(link-visited ((,class (:foreground ,purple :underline t))))
   
   ;; Buttons
   `(button ((,class (:foreground ,cyan :underline t))))
   `(custom-button ((,class (:background ,bg-active :foreground ,fg-main :box (:line-width 1 :color ,border)))))
   `(custom-button-mouse ((,class (:background ,bg-region :foreground ,fg-bright))))
   `(custom-button-pressed ((,class (:background ,bg-region :foreground ,fg-bright))))
   
   ;; Widgets
   `(widget-field ((,class (:background ,bg-active :foreground ,fg-main))))
   `(widget-single-line-field ((,class (:background ,bg-active :foreground ,fg-main))))
   
   ;; Origami (code folding)
   `(origami-fold-header-face ((,class (:foreground ,fg-dim :box nil))))
   `(origami-fold-fringe-face ((,class (:foreground ,fg-dim))))
   `(origami-fold-replacement-face ((,class (:foreground ,fg-dim))))
   
   ;; QML mode specific
   `(qml-operator-face ((,class (:foreground ,orange))))
   
   ;; Python mode specific
   `(python-info-docstring-face ((,class (:foreground ,green :slant italic))))
   
   ;; Show paren
   `(show-paren-match ((,class (:background ,bg-region :foreground ,yellow :weight bold))))
   `(show-paren-mismatch ((,class (:background ,red :foreground ,fg-bright :weight bold))))
   
   ;; Highlight indentation
   `(highlight-indentation-face ((,class (:background ,bg-dim))))
   `(highlight-indentation-current-column-face ((,class (:background ,bg-active))))
   
   ;; Whitespace
   `(whitespace-space ((,class (:foreground ,bg-active))))
   `(whitespace-tab ((,class (:foreground ,bg-active))))
   `(whitespace-trailing ((,class (:background ,red :foreground ,yellow))))
   `(whitespace-line ((,class (:background ,bg-active :foreground ,red))))
   
   ;; Terminal
   `(term-color-black ((,class (:foreground ,bg-main :background ,bg-main))))
   `(term-color-red ((,class (:foreground ,red :background ,red))))
   `(term-color-green ((,class (:foreground ,green :background ,green))))
   `(term-color-yellow ((,class (:foreground ,yellow :background ,yellow))))
   `(term-color-blue ((,class (:foreground ,blue :background ,blue))))
   `(term-color-magenta ((,class (:foreground ,magenta :background ,magenta))))
   `(term-color-cyan ((,class (:foreground ,cyan :background ,cyan))))
   `(term-color-white ((,class (:foreground ,fg-main :background ,fg-main))))
   
   ;; Ansi colors
   `(ansi-color-black ((,class (:foreground ,bg-main :background ,bg-main))))
   `(ansi-color-red ((,class (:foreground ,red :background ,red))))
   `(ansi-color-green ((,class (:foreground ,green :background ,green))))
   `(ansi-color-yellow ((,class (:foreground ,yellow :background ,yellow))))
   `(ansi-color-blue ((,class (:foreground ,blue :background ,blue))))
   `(ansi-color-magenta ((,class (:foreground ,magenta :background ,magenta))))
   `(ansi-color-cyan ((,class (:foreground ,cyan :background ,cyan))))
   `(ansi-color-white ((,class (:foreground ,fg-main :background ,fg-main))))
   
   ;; mu4e - Email client
   `(mu4e-header-face ((,class (:foreground ,fg-main))))
   `(mu4e-header-highlight-face ((,class (:background ,bg-region :weight bold :extend t))))
   `(mu4e-header-marks-face ((,class (:foreground ,yellow :weight bold))))
   `(mu4e-header-title-face ((,class (:foreground ,blue :weight bold))))
   `(mu4e-header-key-face ((,class (:foreground ,green :weight bold))))
   `(mu4e-header-value-face ((,class (:foreground ,fg-main))))
   `(mu4e-unread-face ((,class (:foreground ,cyan :weight bold))))
   `(mu4e-flagged-face ((,class (:foreground ,red :weight bold))))
   `(mu4e-replied-face ((,class (:foreground ,green))))
   `(mu4e-forwarded-face ((,class (:foreground ,blue))))
   `(mu4e-draft-face ((,class (:foreground ,orange))))
   `(mu4e-trashed-face ((,class (:foreground ,fg-dim :strike-through t))))
   `(mu4e-cited-1-face ((,class (:foreground ,blue))))
   `(mu4e-cited-2-face ((,class (:foreground ,green))))
   `(mu4e-cited-3-face ((,class (:foreground ,yellow))))
   `(mu4e-cited-4-face ((,class (:foreground ,cyan))))
   `(mu4e-cited-5-face ((,class (:foreground ,magenta))))
   `(mu4e-cited-6-face ((,class (:foreground ,orange))))
   `(mu4e-cited-7-face ((,class (:foreground ,purple))))
   `(mu4e-compose-separator-face ((,class (:foreground ,fg-dim :strike-through t))))
   `(mu4e-compose-header-face ((,class (:foreground ,fg-dim))))
   `(mu4e-contact-face ((,class (:foreground ,blue))))
   `(mu4e-context-face ((,class (:foreground ,purple :weight bold))))
   `(mu4e-highlight-face ((,class (:background ,bg-active))))
   `(mu4e-link-face ((,class (:foreground ,blue :underline t))))
   `(mu4e-modeline-face ((,class (:foreground ,yellow :weight bold))))
   `(mu4e-moved-face ((,class (:foreground ,magenta))))
   `(mu4e-ok-face ((,class (:foreground ,green :weight bold))))
   `(mu4e-region-code ((,class (:background ,bg-active))))
   `(mu4e-special-header-value-face ((,class (:foreground ,cyan))))
   `(mu4e-system-face ((,class (:foreground ,fg-dim :slant italic))))
   `(mu4e-title-face ((,class (:foreground ,blue :weight bold))))
   `(mu4e-url-number-face ((,class (:foreground ,orange :weight bold))))
   `(mu4e-view-body-face ((,class (:foreground ,fg-main))))
   `(mu4e-warning-face ((,class (:foreground ,yellow :weight bold))))
   `(mu4e-attach-number-face ((,class (:foreground ,orange :weight bold))))
   
   ;; mu4e headers specific
   `(mu4e-headers-date-face ((,class (:foreground ,fg-dim))))
   `(mu4e-headers-from-face ((,class (:foreground ,blue))))
   `(mu4e-headers-subject-face ((,class (:foreground ,fg-main))))
   `(mu4e-headers-size-face ((,class (:foreground ,fg-dim))))
   `(mu4e-headers-seen-face ((,class (:foreground ,fg-dim))))
   `(mu4e-headers-unread-face ((,class (:foreground ,cyan :weight bold))))
   `(mu4e-headers-flagged-face ((,class (:foreground ,red :weight bold))))
   `(mu4e-headers-new-face ((,class (:foreground ,green :weight bold))))
   `(mu4e-headers-passed-face ((,class (:foreground ,blue))))
   `(mu4e-headers-replied-face ((,class (:foreground ,green))))
   `(mu4e-headers-trashed-face ((,class (:foreground ,fg-dim :strike-through t))))
   `(mu4e-headers-draft-face ((,class (:foreground ,orange))))
   
   ;; elfeed - RSS/News reader
   `(elfeed-log-date-face ((,class (:foreground ,fg-dim))))
   `(elfeed-log-debug-level-face ((,class (:foreground ,fg-dim))))
   `(elfeed-log-error-level-face ((,class (:foreground ,red))))
   `(elfeed-log-info-level-face ((,class (:foreground ,blue))))
   `(elfeed-log-warn-level-face ((,class (:foreground ,yellow))))
   
   ;; elfeed search buffer
   `(elfeed-search-date-face ((,class (:foreground ,purple))))
   `(elfeed-search-feed-face ((,class (:foreground ,cyan))))
   `(elfeed-search-filter-face ((,class (:foreground ,green :weight bold))))
   `(elfeed-search-last-update-face ((,class (:foreground ,blue))))
   `(elfeed-search-tag-face ((,class (:foreground ,yellow))))
   `(elfeed-search-title-face ((,class (:foreground ,fg-main))))
   `(elfeed-search-unread-count-face ((,class (:foreground ,green :weight bold))))
   `(elfeed-search-unread-title-face ((,class (:foreground ,fg-bright :weight bold))))
   
   ;; Custom elfeed tag faces (override hardcoded colors from elfeed-config.el)
   `(elfeed-face-tag-news ((,class (:foreground ,orange))))          ; Was "#8B4513" (saddle brown)
   `(elfeed-face-tag-tech ((,class (:foreground ,blue))))            ; Was "#4682B4" (steel blue)
   `(elfeed-face-tag-security ((,class (:foreground ,red))))         ; Was "#DC143C" (crimson)
   `(elfeed-face-tag-programming ((,class (:foreground ,green))))    ; Was "#228B22" (forest green)
   `(elfeed-face-tag-opensource ((,class (:foreground ,cyan))))
   
   ;; elfeed show buffer (article view)
   `(elfeed-show-author-face ((,class (:foreground ,blue))))
   `(elfeed-show-feed-face ((,class (:foreground ,cyan))))
   `(elfeed-show-tag-face ((,class (:foreground ,yellow))))
   `(elfeed-show-title-face ((,class (:foreground ,fg-bright :weight bold :height 1.3))))
   
   ;; Special tags - customize these for your own tags
   `(elfeed-search-starred-title-face ((,class (:foreground ,yellow :weight bold))))
   `(elfeed-search-mustread-title-face ((,class (:foreground ,red :weight bold))))
   `(elfeed-search-later-title-face ((,class (:foreground ,orange))))
   `(elfeed-search-important-title-face ((,class (:foreground ,magenta :weight bold))))
   
   ;; Gnus (sometimes used with mu4e)
   `(gnus-header-content ((,class (:foreground ,fg-main))))
   `(gnus-header-from ((,class (:foreground ,blue :weight bold))))
   `(gnus-header-subject ((,class (:foreground ,green))))
   `(gnus-header-name ((,class (:foreground ,purple))))
   `(gnus-header-newsgroups ((,class (:foreground ,yellow))))
   
   ;; Message mode (email composition)
   `(message-header-name ((,class (:foreground ,purple :weight bold))))
   `(message-header-cc ((,class (:foreground ,blue))))
   `(message-header-newsgroups ((,class (:foreground ,yellow))))
   `(message-header-other ((,class (:foreground ,cyan))))
   `(message-header-subject ((,class (:foreground ,green :weight bold))))
   `(message-header-to ((,class (:foreground ,blue :weight bold))))
   `(message-header-xheader ((,class (:foreground ,fg-dim))))
   `(message-mml ((,class (:foreground ,orange :weight bold))))
   `(message-separator ((,class (:foreground ,fg-dim :strike-through t))))
   `(message-cited-text ((,class (:foreground ,fg-dim :slant italic))))
   `(message-cited-text-1 ((,class (:foreground ,blue))))
   `(message-cited-text-2 ((,class (:foreground ,green))))
   `(message-cited-text-3 ((,class (:foreground ,yellow))))
   `(message-cited-text-4 ((,class (:foreground ,cyan))))
   
   ;; Notmuch (alternative email client)
   `(notmuch-crypto-decryption ((,class (:foreground ,green))))
   `(notmuch-crypto-signature-bad ((,class (:foreground ,red))))
   `(notmuch-crypto-signature-good ((,class (:foreground ,green))))
   `(notmuch-crypto-signature-good-key ((,class (:foreground ,green :weight bold))))
   `(notmuch-crypto-signature-unknown ((,class (:foreground ,yellow))))
   `(notmuch-search-date ((,class (:foreground ,purple))))
   `(notmuch-search-count ((,class (:foreground ,fg-dim))))
   `(notmuch-search-flagged-face ((,class (:foreground ,red))))
   `(notmuch-search-matching-authors ((,class (:foreground ,blue))))
   `(notmuch-search-non-matching-authors ((,class (:foreground ,fg-dim))))
   `(notmuch-search-subject ((,class (:foreground ,fg-main))))
   `(notmuch-search-unread-face ((,class (:foreground ,cyan :weight bold))))
   `(notmuch-tag-added ((,class (:foreground ,green :weight bold))))
   `(notmuch-tag-deleted ((,class (:foreground ,red :strike-through t))))
   `(notmuch-tag-face ((,class (:foreground ,yellow))))
   `(notmuch-tag-flagged ((,class (:foreground ,red :weight bold))))
   `(notmuch-tag-unread ((,class (:foreground ,cyan :weight bold))))
   `(notmuch-tree-match-author-face ((,class (:foreground ,blue))))
   `(notmuch-tree-match-date-face ((,class (:foreground ,purple))))
   `(notmuch-tree-match-subject-face ((,class (:foreground ,fg-main))))
   `(notmuch-tree-match-tag-face ((,class (:foreground ,yellow))))
   `(notmuch-tree-no-match-face ((,class (:foreground ,fg-dim))))))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

;;;###autoload
(defun developer-dark-theme-reload ()
  "Reload the developer-dark theme."
  (interactive)
  (disable-theme 'developer-dark)
  (load-theme 'developer-dark t))

(provide-theme 'developer-dark)

;;; developer-dark-theme.el ends here