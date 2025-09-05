;;; -*- lexical-binding: t -*-
;;; Commentary:

;;; Improved Emacs Configuration
;;; Keeping: treemacs, helm, diff-hl, magit

;;; Package Management - Consolidated
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; Core package list - consolidated and cleaned
(setq package-selected-packages
      '(;; Core utilities
        use-package which-key

        ;; Themes
        modus-themes

        ;; Version control
        diff-hl

        ;; File management
        treemacs treemacs-projectile treemacs-all-the-icons
        neotree all-the-icons all-the-icons-dired diredfl

        ;; Helm ecosystem
        helm helm-xref helm-projectile

        ;; Core project management
        projectile

        ;; Markdown & Notes
        markdown-mode markdown-toc grip-mode
        obsidian olivetti

        ;; Search and navigation
        deadgrep ripgrep wgrep anzu
        ibuffer-sidebar ibuffer-projectile

        ;; Required for some functionality
        org dash s f ht spinner lv hydra avy

        ;; RSS/News reader
        elfeed elfeed-org

        ;; Email (mu4e installed separately)
        ))

;; Auto-install missing packages
(when (cl-find-if-not #'package-installed-p package-selected-packages)
  (package-refresh-contents)
  (mapc #'package-install package-selected-packages))

;; Ensure use-package is loaded
(eval-when-compile
  (require 'use-package))
(setq use-package-always-ensure t)

;;; Custom Settings (preserved from original)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(before-save-hook '(delete-trailing-whitespace))
 '(column-number-mode t)
 '(cua-mode t)
 '(custom-enabled-themes '(developer-dark))
 '(custom-safe-themes
   '("2974c2d5ffede4f111b02701ccdfadfbcde3d158ad9b09dade627b3ce3049ea1"
     "f8859f15bd0089a85d8d14e21dd774a9a8b391dac076e3ff17a13529b3d16576"
     "de2f009a49f8eaf2f323519d86016849cd1716d979bc3f7e4afb58899e52ddb7"
     "9fb69436c074b82a62b78b8d733e6274d0bd16d156f7b094e2afe4345c040c49"
     "004f174754c688f358fa2afc4f8699b5db647fbfaa0d6b55ff39f63e05bfbbf5"
     "ca1b398ceb1b61709197478dc7f705b8337a0a9631e399948e643520c5557382"
     "75eef60308d7328ed14fa27002e85de255c2342e73275173a14ed3aa1643d545"
     "77f281064ea1c8b14938866e21c4e51e4168e05db98863bd7430f1352cab294a"
     "242e6f00c98aa43539b41c505ef094d21cbc981203809a82949efaa2bc6cb194"
     "c9e63647d90e7dc59d52c176fbfd46fd2cfed275cd2bad9b29b6cf620d3389b6"
     "ad7d874d137291e09fe2963babc33d381d087fa14928cb9d34350b67b6556b6d"
     default))
 '(diff-hl-global-modes t)
 '(display-line-numbers t)
 '(display-line-numbers-type 'relative)
 '(global-display-line-numbers-mode t)
 '(ls-lisp-dirs-first t)
 '(neo-show-hidden-files t)
 '(neo-window-width 40)
 '(package-selected-packages
   '(all-the-icons all-the-icons-dired anzu avy clang-format
                   clang-format+ commenter company company-box
                   company-qml cyberpunk-theme dap-mode dash deadgrep
                   diff-hl diredfl expand-region f flycheck ggtags
                   grip-mode helm helm-lsp helm-projectile helm-xref
                   hl-todo ht hydra ibuffer-projectile ibuffer-sidebar
                   lsp-mode lsp-treemacs lsp-ui lv magit magit-delta
                   markdown-mode markdown-toc modus-themes
                   multiple-cursors neotree obsidian olivetti org
                   origami projectile qml-mode rainbow-delimiters
                   ripgrep s spinner treemacs treemacs-all-the-icons
                   treemacs-magit treemacs-projectile use-package
                   wgrep which-key yasnippet))
 '(safe-local-variable-values
   '((company-backends
      (company-qml company-capf company-files company-yasnippet))
     (lsp-clients-qml-server-executable . "/usr/lib/qt6/bin/qmlls")
     (company-minimum-prefix-length . 1) (company-idle-delay . 0.2)
     (lsp-clangd-binary-path . "clangd")
     (lsp-clients-clangd-args
      "--compile-commands-dir=/home/jens/sources/thulio"
      "--background-index" "--clang-tidy"
      "--completion-style=detailed" "--header-insertion=iwyu"
      "--pch-storage=memory")
     (projectile-project-root . "/home/jens/sources/thulio")))
 '(save-place-mode t)
 '(show-trailing-whitespace t)
 '(tool-bar-mode nil)
 '(url-proxy-services
   '(("https" . "eudewerepo001:3128") ("http" . "eudewerepo001:3128"))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "0xProto Nerd Font" :foundry "0x  " :slant normal :weight regular :height 120 :width normal))))
 '(diff-hl-change ((t (:background "blue3" :foreground "blue3"))))
 '(diff-hl-delete ((t (:background "red3" :foreground "red3"))))
 '(diff-hl-insert ((t (:background "green3" :foreground "green3")))))

;;; Helm Configuration (Primary interface)
(use-package helm
  :ensure t
  :demand t
  :bind (("M-x" . helm-M-x)
         ("C-x C-f" . helm-find-files)
         ("C-x b" . helm-buffers-list)
         ("C-x r b" . helm-bookmarks)
         ("M-y" . helm-show-kill-ring)
         ("C-h SPC" . helm-all-mark-rings))
  :config
  (helm-mode 1)
  (setq helm-split-window-in-side-p t
        helm-move-to-line-cycle-in-source t
        helm-ff-search-library-in-sexp t
        helm-scroll-amount 8
        helm-ff-file-name-history-use-recentf t
        helm-echo-input-in-header-line t)
  ;; Make helm more responsive
  (setq helm-input-idle-delay 0.01
        helm-cycle-resume-delay 2
        helm-follow-input-idle-delay 0.01)
  ;; Prevent helm from opening new frames
  (setq helm-always-two-windows nil)
  (setq helm-display-buffer-default-height 15)
  (setq helm-default-display-buffer-functions '(display-buffer-in-side-window)))

(use-package helm-xref
  :ensure t
  :after helm)

(use-package helm-projectile
  :ensure t
  :after (helm projectile)
  :config
  (helm-projectile-on)
  ;; Prevent helm-projectile from opening new frames
  (setq helm-projectile-sources-list
        '(helm-source-projectile-buffers-list
          helm-source-projectile-recentf-list
          helm-source-projectile-files-list
          helm-source-projectile-projects))
  ;; Use current window for file actions
  (setq helm-display-function 'helm-default-display-buffer))

;;; Load development configuration (optional - use M-x enable-dev-mode)
(let ((dev-config (expand-file-name "emacs-dev-config.el" user-emacs-directory)))
  (when (file-exists-p dev-config)
    (load-file dev-config)))

;;; Version Control (basic) - Magit moved to development mode

;;; Diff-hl Configuration
(use-package diff-hl
  :ensure t
  :hook ((prog-mode . diff-hl-mode)
         (dired-mode . diff-hl-dired-mode)
         (text-mode . diff-hl-mode)
         (conf-mode . diff-hl-mode))
  :bind (("M-n" . diff-hl-next-hunk)
         ("M-p" . diff-hl-previous-hunk)
         ("C-c v r" . diff-hl-revert-hunk)
         ("C-c v s" . diff-hl-diff-goto-hunk)
         ("C-c v u" . diff-hl-refresh))
  :init
  ;; Set fringe width before diff-hl loads
  (setq-default left-fringe-width 8)
  (setq-default right-fringe-width 8)
  ;; Ensure fringes are visible
  (fringe-mode 8)
  :config
  ;; Configure VC backend for Git
  (setq vc-handled-backends '(Git))
  (setq vc-git-diff-switches '("--histogram"))

  ;; IMPORTANT: Tell diff-hl to use VC backend, not Magit
  (setq diff-hl-reference-revision nil) ; Use working tree, not index
  (setq diff-hl-disable-on-remote nil)  ; Work even on remote files

  ;; Make diff-hl use the left fringe
  (setq diff-hl-side 'left)

  ;; Ensure diff-hl draws in fringe, not margin
  (setq diff-hl-draw-borders nil)
  (setq diff-hl-margin-mode nil)

  ;; Set diff-hl fringe bitmaps (ensure they're visible)
  (setq diff-hl-fringe-bmp-function 'diff-hl-fringe-bmp-from-type)

  ;; Enable flydiff for real-time updates using VC
  (diff-hl-flydiff-mode 1)

  ;; Update immediately when visiting a file
  (setq diff-hl-flydiff-delay 0.3)

  ;; Make sure diff-hl updates on various events
  (add-hook 'after-save-hook 'diff-hl-update)
  (add-hook 'after-revert-hook 'diff-hl-update)
  (add-hook 'find-file-hook 'diff-hl-update)
  (add-hook 'vc-checkin-hook 'diff-hl-update)

  ;; Enable globally
  (global-diff-hl-mode 1)

  ;; Explicitly disable Magit integration in base config
  ;; (Magit hooks will only be added when dev-mode is enabled)
  (setq diff-hl-disable-on-remote nil)

  ;; Manual refresh command
  (defun diff-hl-refresh ()
    "Manually refresh diff-hl indicators in all buffers."
    (interactive)
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (when diff-hl-mode
          (diff-hl-update)))))

  ;; Troubleshooting function
  (defun diff-hl-diagnose ()
    "Diagnose diff-hl issues."
    (interactive)
    (let ((diagnosis '()))
      (push (format "diff-hl-mode: %s" (if diff-hl-mode "enabled" "disabled")) diagnosis)
      (push (format "Buffer file: %s" (or buffer-file-name "none")) diagnosis)
      (push (format "VC backend: %s" (or (vc-backend buffer-file-name) "none")) diagnosis)
      (push (format "VC responsible backend: %s" (or (vc-responsible-backend default-directory) "none")) diagnosis)
      (push (format "Modified: %s" (if (and buffer-file-name (buffer-modified-p)) "yes" "no")) diagnosis)
      (push (format "VC state: %s" (when buffer-file-name (vc-state buffer-file-name))) diagnosis)
      (push (format "Left fringe width: %s" left-fringe-width) diagnosis)
      (push (format "Right fringe width: %s" right-fringe-width) diagnosis)
      (push (format "diff-hl-side: %s" diff-hl-side) diagnosis)
      (push (format "diff-hl-margin-mode: %s" diff-hl-margin-mode) diagnosis)
      (push (format "diff-hl-reference-revision: %s" diff-hl-reference-revision) diagnosis)
      (push (format "Magit loaded: %s" (if (fboundp 'magit-status) "yes" "no")) diagnosis)
      (message (mapconcat 'identity (nreverse diagnosis) "\n"))))

  ;; Force VC to refresh its state
  (defun diff-hl-force-vc-refresh ()
    "Force VC to refresh state and then update diff-hl."
    (interactive)
    (when buffer-file-name
      (vc-refresh-state)
      (diff-hl-update)
      (message "VC state refreshed and diff-hl updated")))

  ;; Disable Magit integration if causing issues
  (defun diff-hl-disable-magit ()
    "Disable Magit integration with diff-hl."
    (interactive)
    (setq diff-hl-disable-magit-integration t)
    (remove-hook 'magit-pre-refresh-hook 'diff-hl-magit-pre-refresh)
    (remove-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
    (message "Magit integration with diff-hl disabled. Using pure VC backend."))

  ;; Ensure we're using VC backend
  (defun diff-hl-ensure-vc-backend ()
    "Ensure diff-hl is using VC backend."
    (interactive)
    (setq diff-hl-reference-revision nil)
    (diff-hl-disable-magit)
    (diff-hl-refresh)
    (message "diff-hl configured to use VC backend only")))

;;; Treemacs Configuration
(use-package treemacs
  :ensure t
  :defer t
  :bind (("M-0"       . treemacs-select-window)
         ("C-x t t"   . treemacs)
         ("C-x t 1"   . treemacs-delete-other-windows)
         ("C-x t d"   . treemacs-select-directory)
         ("C-x t B"   . treemacs-bookmark)
         ("<f8>"      . treemacs)
         :map treemacs-mode-map
         ("/" . treemacs-search-file)
         ("C-s" . helm-projectile-find-file)
         ("s" . helm-projectile-grep))
  :config
  (setq treemacs-collapse-dirs                   (if treemacs-python-executable 3 0)
        treemacs-deferred-git-apply-delay        0.5
        treemacs-directory-name-transformer      #'identity
        treemacs-display-in-side-window          t
        treemacs-eldoc-display                   'simple
        treemacs-file-event-delay                2000
        treemacs-file-follow-delay                0.2
        treemacs-follow-after-init                t
        treemacs-expand-after-init               t
        treemacs-find-workspace-method           'find-for-file-or-pick-first
        treemacs-hide-dot-git-directory          t
        treemacs-indentation                     2
        treemacs-indentation-string              " "
        treemacs-is-never-other-window           nil
        treemacs-max-git-entries                  5000
        treemacs-missing-project-action          'ask
        treemacs-move-forward-on-expand          nil
        treemacs-no-delete-other-windows         t
        treemacs-persist-file                    (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
        treemacs-position                        'left
        treemacs-recenter-distance               0.1
        treemacs-recenter-after-file-follow      nil
        treemacs-recenter-after-tag-follow       nil
        treemacs-recenter-after-project-jump     'always
        treemacs-recenter-after-project-expand   'on-distance
        treemacs-show-cursor                     nil
        treemacs-show-hidden-files               t
        treemacs-silent-filewatch                nil
        treemacs-silent-refresh                  nil
        treemacs-sorting                         'alphabetic-asc
        treemacs-space-between-root-nodes        t
        treemacs-tag-follow-cleanup              t
        treemacs-tag-follow-delay                1.5
        treemacs-width                           35
        treemacs-width-is-initially-locked       t
        treemacs-workspace-switch-cleanup        nil)

  (treemacs-follow-mode t)
  (treemacs-filewatch-mode t)
  (treemacs-fringe-indicator-mode 'always)
  (when treemacs-python-executable
    (treemacs-git-commit-diff-mode t))

  (pcase (cons (not (null (executable-find "git")))
               (not (null treemacs-python-executable)))
    (`(t . t) (treemacs-git-mode 'deferred))
    (`(t . _) (treemacs-git-mode 'simple)))

  (treemacs-hide-gitignored-files-mode nil))

(use-package treemacs-projectile
  :ensure t
  :after (treemacs projectile))

(use-package treemacs-all-the-icons
  :ensure t
  :after (treemacs all-the-icons)
  :config
  (treemacs-load-theme "all-the-icons"))

;; Treemacs helper functions
(defun treemacs-toggle-and-focus ()
  "Toggle treemacs and focus on it if it's visible."
  (interactive)
  (if (treemacs-get-local-window)
      (treemacs-toggle)
    (progn
      (treemacs)
      (treemacs-select-window))))

(defun treemacs-search-file ()
  "Search for a file in the current project using helm."
  (interactive)
  (if (fboundp 'helm-projectile-find-file)
      (helm-projectile-find-file)
    (helm-find-files)))

(defun treemacs-open-marked-files ()
  "Open all marked files in treemacs."
  (interactive)
  (when (eq major-mode 'treemacs-mode)
    (treemacs-bulk-file-actions
     :actions '(treemacs-visit-node-no-split))))

(defun treemacs-mark-visible-files ()
  "Mark all visible files in the current directory."
  (interactive)
  (when (eq major-mode 'treemacs-mode)
    (save-excursion
      (treemacs-goto-parent-node)
      (treemacs-TAB-action)
      (forward-line 1)
      (while (and (not (eobp))
                  (> (treemacs--get-depth-of-item) 0))
        (when (treemacs-is-node-file?)
          (treemacs-do-mark))
        (forward-line 1)))))

(global-set-key (kbd "C-c t f") 'treemacs-toggle-and-focus)
(global-set-key (kbd "C-c t s") 'treemacs-search-file)

;;; Search Configuration (with Helm)
(use-package deadgrep
  :ensure t
  :bind (("C-c r" . deadgrep)))

(use-package wgrep
  :ensure t
  :config
  (setq wgrep-auto-save-buffer t)
  (setq wgrep-enable-key "r"))

(use-package anzu
  :ensure t
  :config
  (global-anzu-mode 1)
  :bind (([remap query-replace] . anzu-query-replace)
         ([remap query-replace-regexp] . anzu-query-replace-regexp)))

;;; Project Management
(use-package projectile
  :ensure t
  :init
  (projectile-mode +1)
  :bind-keymap ("C-c p" . projectile-command-map)
  :config
  (setq projectile-completion-system 'helm)
  (setq projectile-switch-project-action #'projectile-dired))

;;; Development Tools - Moved to emacs-dev-config.el
;;; Use M-x enable-dev-mode to activate development features

;;; Editor Enhancements
(use-package which-key
  :ensure t
  :init (which-key-mode)
  :config
  (setq which-key-idle-delay 0.3)
  (setq which-key-popup-type 'side-window))

;;; Editor Enhancements - Development-specific features moved to emacs-dev-config.el

(use-package all-the-icons-dired
  :ensure t
  :hook (dired-mode . all-the-icons-dired-mode))

(use-package diredfl
  :ensure t
  :config (diredfl-global-mode 1))

;; Enhanced Dired configuration for multi-file operations
(use-package dired
  :ensure nil
  :bind (("C-c d" . dired-jump)
         ("C-c D" . projectile-dired))
  :config
  (setq dired-dwim-target t)  ; Guess target directory
  (setq dired-recursive-copies 'always)
  (setq dired-recursive-deletes 'always)
  (setq dired-listing-switches "-alh --group-directories-first")

  ;; Enable multiple file marking with mouse
  (define-key dired-mode-map (kbd "<mouse-2>") 'dired-find-file)
  (define-key dired-mode-map (kbd "C-<mouse-1>") 'dired-mouse-mark)

  ;; Quick marking shortcuts
  (define-key dired-mode-map (kbd "* .") 'dired-mark-extension)
  (define-key dired-mode-map (kbd "* /") 'dired-mark-directories))

  ;; Custom sorting: directories first (dotted first), then files (dotted first)
  (defun dired-sort-dotfiles-first ()
    "Sort dired: dirs first (dots first within), then files (dots first within)."
    (save-excursion
      (let (buffer-read-only)
        (goto-char (point-min))
        ;; Skip past the directory header
        (while (and (not (eobp))
                    (not (looking-at "^  \\|^\\s-*$")))
          (forward-line 1))
        (let ((start (point))
              dirs dotdirs files dotfiles special-entries)
          ;; Collect all entries
          (while (not (eobp))
            (when (looking-at "^  \\(.*\\)$")
              (let* ((line (match-string 0))
                     (filename (ignore-errors (dired-get-filename 'no-dir t))))
                (cond
                 ;; Keep . and .. entries separate to put at top
                 ((member filename '("." ".."))
                  (push line special-entries))
                 ;; Process other entries
                 (filename
                  (let ((fullpath (ignore-errors (dired-get-filename t))))
                    (when fullpath
                      (cond
                       ;; Dot directory
                       ((and (file-directory-p fullpath)
                             (string-prefix-p "." filename))
                        (push line dotdirs))
                       ;; Regular directory
                       ((file-directory-p fullpath)
                        (push line dirs))
                       ;; Dotfile
                       ((string-prefix-p "." filename)
                        (push line dotfiles))
                       ;; Regular file
                       (t
                        (push line files)))))))))
            (forward-line 1))
          ;; Delete old content and insert sorted
          (when (or special-entries dirs dotdirs files dotfiles)
            (delete-region start (point-max))
            (goto-char start)
            ;; Insert in order: . and .., dot dirs, regular dirs, dotfiles, regular files
            (dolist (line (nreverse special-entries))
              (insert line "\n"))
            (dolist (line (sort (nreverse dotdirs) 'string<))
              (insert line "\n"))
            (dolist (line (sort (nreverse dirs) 'string<))
              (insert line "\n"))
            (dolist (line (sort (nreverse dotfiles) 'string<))
              (insert line "\n"))
            (dolist (line (sort (nreverse files) 'string<))
              (insert line "\n")))))
      (set-buffer-modified-p nil)))

  ;; Apply custom sorting after dired reads directory
  (add-hook 'dired-after-readin-hook 'dired-sort-dotfiles-first)

;;; Markdown Support
(use-package markdown-mode
  :ensure t
  :mode (("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :hook (markdown-mode . (lambda ()
                          (visual-line-mode 1)
                          (flyspell-mode 1)
                          (auto-fill-mode -1)))
  :bind (:map markdown-mode-map
              ("C-c C-l" . markdown-insert-link)
              ("C-c C-i" . markdown-insert-image)
              ("C-c C-c p" . markdown-preview)
              ("C-c C-c l" . markdown-live-preview-mode))
  :config
  (setq markdown-command "markdown")
  (setq markdown-enable-wiki-links t)
  (setq markdown-italic-underscore t)
  (setq markdown-asymmetric-header t)
  (setq markdown-make-gfm-checkboxes-buttons t)
  (setq markdown-gfm-uppercase-checkbox t)
  (setq markdown-fontify-code-blocks-natively t))

(use-package markdown-toc
  :ensure t
  :after markdown-mode
  :bind (:map markdown-mode-map
              ("C-c C-t" . markdown-toc-generate-or-refresh-toc)))

(use-package grip-mode
  :ensure t
  :after markdown-mode
  :bind (:map markdown-mode-map
              ("C-c C-c g" . grip-mode)))


;;; UI Configuration
(setq-default display-fill-column-indicator-column 80)
(setq-default display-fill-column-indicator-character ?\u2502)
(global-display-fill-column-indicator-mode 1)
(set-face-attribute 'fill-column-indicator nil :foreground "red")

;; Enable mouse window resizing
(setq mouse-autoselect-window nil)  ; Don't auto-select windows on mouse hover
(setq window-divider-default-places t)  ; Show dividers everywhere
(setq window-divider-default-bottom-width 1)  ; Bottom divider width
(setq window-divider-default-right-width 1)  ; Right divider width
(window-divider-mode 1)  ; Enable window dividers for easier mouse dragging

;; CUA mode for rectangles
(setq cua-auto-tabify-rectangles nil)
(setq cua-keep-region-after-copy t)
(global-set-key (kbd "C-<return>") 'cua-set-rectangle-mark)

;;; Session Management
(desktop-save-mode 1)
(setq desktop-save t)
(setq desktop-auto-save-timeout 300)
(setq desktop-path '("~/.emacs.d/"))
(setq desktop-dirname "~/.emacs.d/")
(setq desktop-base-file-name "emacs-desktop")
(setq desktop-restore-eager 10)
(setq desktop-restore-frames t)

(savehist-mode 1)
(setq savehist-file "~/.emacs.d/savehist")
(setq history-length 1000)

(save-place-mode 1)
(setq save-place-file "~/.emacs.d/saveplace")

(recentf-mode 1)
(setq recentf-max-menu-items 50)
(setq recentf-max-saved-items 200)

;;; Performance Optimizations
(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 1024 1024))

;;; General Settings
(setq-default indent-tabs-mode nil)
(setq tab-width 4)
(setq inhibit-startup-screen t)
(global-auto-revert-mode t)
(electric-pair-mode 1)
(show-paren-mode 1)
(setq show-paren-delay 0)
(global-hl-line-mode 1)

;; Use system ls for better performance and features
(setq ls-lisp-use-insert-directory-program t)  ; Use system's ls command
(setq insert-directory-program "ls")  ; Explicitly set to use ls

;; Auto-save and backup settings
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))
(setq delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)
(setq auto-save-file-name-transforms
      '((".*" "~/.emacs.d/auto-save-list/" t)))

;;; Custom Functions
(defun kill-current-buffer-no-confirm ()
  "Kill the current buffer without confirmation, unless it has unsaved changes."
  (interactive)
  (kill-buffer (current-buffer)))

(defun package-refresh-without-proxy ()
  "Temporarily disable proxy and refresh packages."
  (interactive)
  (let ((url-proxy-services nil))
    (package-refresh-contents)
    (message "Package list refreshed without proxy")))

(defun package-install-without-proxy (package)
  "Install PACKAGE without using proxy."
  (interactive
   (list (intern (completing-read "Install package: "
                                 (mapcar #'car package-archive-contents)))))
  (let ((url-proxy-services nil))
    (package-install package)
    (message "Package %s installed without proxy" package)))

(defun install-dev-packages ()
  "Install development packages without proxy."
  (interactive)
  (let ((url-proxy-services nil)
        (dev-packages '(lsp-mode lsp-ui lsp-treemacs
                       company company-box yasnippet
                       flycheck magit forge)))
    (package-refresh-contents)
    (dolist (pkg dev-packages)
      (unless (package-installed-p pkg)
        (condition-case err
            (progn
              (package-install pkg)
              (message "Installed %s" pkg))
          (error
           (message "Failed to install %s: %s" pkg err)))))
    (message "Development packages installation complete")))

(defvar url-proxy-services-backup nil
  "Backup of proxy settings.")

(defun toggle-proxy ()
  "Toggle proxy settings on/off."
  (interactive)
  (if url-proxy-services
      (progn
        (setq url-proxy-services-backup url-proxy-services)
        (setq url-proxy-services nil)
        (message "Proxy disabled"))
    (progn
      (setq url-proxy-services (or url-proxy-services-backup
                                   '(("https" . "eudewerepo001:3128")
                                     ("http" . "eudewerepo001:3128"))))
      (message "Proxy enabled: %s" (cdr (assoc "http" url-proxy-services))))))

(defun reload-emacs-config ()
  "Reload the Emacs configuration file and all dependent configs."
  (interactive)
  ;; First reload the main init.el
  (load-file (expand-file-name "init.el" user-emacs-directory))

  ;; Reload development config if it exists
  (let ((dev-config (expand-file-name "emacs-dev-config.el" user-emacs-directory)))
    (when (file-exists-p dev-config)
      (load-file dev-config)))

  ;; Reload elfeed config if it exists
  (let ((elfeed-config (expand-file-name "elfeed-config.el" user-emacs-directory)))
    (when (file-exists-p elfeed-config)
      (load-file elfeed-config)))

  ;; Reload mu4e config if it exists
  (let ((mu4e-config (expand-file-name "mu4e-config.el" user-emacs-directory)))
    (when (file-exists-p mu4e-config)
      (condition-case err
          (load-file mu4e-config)
        (error
         (message "mu4e config available but mu4e not installed")))))

  (message "Emacs configuration fully reloaded!"))

;;; Final Keybindings
(global-set-key (kbd "C-x k") 'kill-current-buffer-no-confirm)
(global-set-key (kbd "C-c C-r") 'reload-emacs-config)
(global-set-key (kbd "C-x C-b") 'helm-buffers-list)

;;; RSS Reader Configuration (Elfeed)
(let ((elfeed-config (expand-file-name "elfeed-config.el" user-emacs-directory)))
  (when (file-exists-p elfeed-config)
    (load-file elfeed-config)
    (message "Elfeed RSS reader configuration loaded.")))

;;; Email Configuration (mu4e)
(let ((mu4e-config (expand-file-name "mu4e-config.el" user-emacs-directory)))
  (when (file-exists-p mu4e-config)
    (condition-case err
        (progn
          (load-file mu4e-config)
          (message "mu4e email configuration loaded."))
      (error
       (message "mu4e configuration available but mu4e not installed. Install mu4e package to enable email.")))))

;;; Theme Management
(defun load-jens-dark-theme ()
  "Load the custom jens-dark theme."
  (interactive)
  (load-theme 'jens-dark t)
  (message "Jens Dark theme loaded"))

(defun switch-theme (theme)
  "Switch to a different theme interactively."
  (interactive
   (list (intern (completing-read "Load theme: "
                                  (mapcar #'symbol-name (custom-available-themes))))))
  (mapc #'disable-theme custom-enabled-themes)
  (load-theme theme t)
  (message "Switched to %s theme" theme))

;;; Development Mode Information
(defun show-dev-mode-info ()
  "Show information about development mode."
  (interactive)
  (message "Development mode is available. Use M-x enable-dev-mode to activate LSP, company-mode, flycheck, and other development tools."))

;; Show info on startup if dev config is loaded
(when (featurep 'emacs-dev-config)
  (add-hook 'emacs-startup-hook
            (lambda ()
              (run-with-timer 1 nil
                              (lambda ()
                                (message "Development mode available. Use M-x enable-dev-mode to activate."))))))

(provide 'init)
;;; init.el ends here
