;;; -*- lexical-binding: t -*-
;;; emacs-dev-config.el --- Development configuration for Emacs
;;; Commentary:
;;; Development-related configurations including LSP, languages, debugging, etc.
;;; Use M-x enable-dev-mode to activate this configuration
;;; Use M-x disable-dev-mode to deactivate this configuration

;;; Code:

(defvar dev-mode-enabled nil
  "Flag indicating whether development mode is enabled.")

(defvar dev-mode-packages
  `(;; Development tools
    lsp-mode lsp-ui lsp-treemacs
    company company-box
    flycheck yasnippet
    projectile
    ggtags multiple-cursors expand-region
    hl-todo rainbow-delimiters
    origami  ;; Code folding

    ;; Version control
    magit 
    ,@(when (executable-find "delta") '(magit-delta))  ;; Only if delta is installed
    treemacs-magit

    ;; Helm integration for development
    helm-lsp helm-xref helm-projectile

    ;; Languages
    clang-format qml-mode company-qml

    ;; Debugging
    dap-mode)
  "List of packages required for development mode.")

(defun dev-mode-ensure-packages ()
  "Ensure all development packages are installed."
  (dolist (package dev-mode-packages)
    (unless (package-installed-p package)
      (package-refresh-contents)
      (package-install package))))

(defun dev-mode-setup-lsp ()
  "Configure LSP mode for development."
  (use-package lsp-mode
    :ensure t
    :hook ((c-mode c++-mode python-mode qml-mode) . lsp-deferred)
    :commands (lsp lsp-deferred)
    :config
    (setq lsp-keymap-prefix "C-c l")
    (setq lsp-idle-delay 0.5)
    (setq lsp-log-io nil)
    (setq lsp-completion-enable t)
    (setq lsp-headerline-breadcrumb-enable t)
    (setq lsp-enable-snippet t)
    (setq lsp-enable-semantic-highlighting t))

  (use-package lsp-ui
    :ensure t
    :commands lsp-ui-mode
    :config
    (setq lsp-ui-doc-enable t)
    (setq lsp-ui-doc-position 'bottom)
    (setq lsp-ui-doc-delay 1)
    (setq lsp-ui-sideline-enable t)
    (setq lsp-ui-sideline-show-hover t)
    (setq lsp-ui-sideline-show-diagnostics t)
    (setq lsp-ui-sideline-show-code-actions t))

  (use-package helm-lsp
    :ensure t
    :commands helm-lsp-workspace-symbol)

  (use-package lsp-treemacs
    :ensure t
    :commands lsp-treemacs-errors-list))

(defun dev-mode-setup-company ()
  "Configure company mode for auto-completion."
  (use-package company
    :ensure t
    :hook (after-init . global-company-mode)
    :bind (:map company-active-map
                ("C-n" . company-select-next)
                ("C-p" . company-select-previous)
                ("TAB" . company-complete-selection))
    :config
    (setq company-idle-delay 0.2)
    (setq company-minimum-prefix-length 1)
    (setq company-selection-wrap-around t)
    (setq company-show-numbers t)
    (setq company-tooltip-align-annotations t))

  (use-package company-box
    :ensure t
    :hook (company-mode . company-box-mode)))

(defun dev-mode-setup-flycheck ()
  "Configure flycheck for syntax checking."
  (use-package flycheck
    :ensure t
    :init (global-flycheck-mode)
    :config
    (setq flycheck-display-errors-delay 0.3)
    (setq flycheck-gcc-language-standard "c++17")
    (setq flycheck-clang-language-standard "c++17")))

(defun dev-mode-setup-yasnippet ()
  "Configure yasnippet for code snippets."
  (use-package yasnippet
    :ensure t
    :config
    (yas-global-mode 1)))

(defun dev-mode-setup-projectile ()
  "Configure projectile for project management."
  (use-package projectile
    :ensure t
    :init
    (projectile-mode +1)
    :bind-keymap ("C-c p" . projectile-command-map)
    :config
    (setq projectile-completion-system 'helm)
    (setq projectile-switch-project-action #'projectile-dired)
    (setq projectile-enable-caching t))

  (use-package helm-projectile
    :ensure t
    :after (helm projectile)
    :config
    (helm-projectile-on)))

(defun dev-mode-setup-ggtags ()
  "Configure ggtags for code navigation."
  (use-package ggtags
    :ensure t
    :hook ((c-mode c++-mode python-mode) . ggtags-mode)
    :bind (:map ggtags-mode-map
                ("C-c g s" . ggtags-find-other-symbol)
                ("C-c g h" . ggtags-view-tag-history)
                ("C-c g r" . ggtags-find-reference)
                ("C-c g f" . ggtags-find-file)
                ("C-c g c" . ggtags-create-tags))
    :config
    (setq ggtags-completing-read-function nil)
    (setq ggtags-navigation-mode-lighter nil)
    (setq ggtags-mode-line-project-name nil)))

(defun dev-mode-setup-origami ()
  "Configure Origami for code folding."
  (use-package origami
    :ensure t
    :config
    ;; Define global keybindings for origami
    (global-set-key (kbd "C-c f f") 'origami-toggle-node)
    (global-set-key (kbd "C-c f o") 'origami-open-node)
    (global-set-key (kbd "C-c f c") 'origami-close-node)
    (global-set-key (kbd "C-c f a") 'origami-close-all-nodes)
    (global-set-key (kbd "C-c f A") 'origami-open-all-nodes)
    (global-set-key (kbd "C-c f t") 'origami-toggle-all-nodes)
    (global-set-key (kbd "C-c f r") 'origami-recursively-toggle-node)
    (global-set-key (kbd "C-c f R") 'origami-open-node-recursively)
    (global-set-key (kbd "C-c f n") 'origami-next-fold)
    (global-set-key (kbd "C-c f p") 'origami-previous-fold)
    (global-set-key (kbd "C-c f s") 'origami-show-only-node)
    (global-set-key (kbd "C-c f u") 'origami-undo)
    (global-set-key (kbd "C-c f d") 'origami-redo)
    
    ;; Setup origami
    (setq origami-show-fold-header t)
    
    ;; Enable origami mode globally for programming modes
    (global-origami-mode 1)
    
    ;; Add hook to ensure origami works in prog-mode buffers
    (add-hook 'prog-mode-hook 'origami-mode)
    
    ;; Initialize parsers
    (origami-mode 1)
    
    ;; Face customization for fold markers (only if faces exist)
    (when (facep 'origami-fold-header-face)
      (set-face-attribute 'origami-fold-header-face nil
                          :box nil
                          :foreground "dim gray"))
    (when (facep 'origami-fold-fringe-face)
      (set-face-attribute 'origami-fold-fringe-face nil
                          :foreground "dim gray"))))

(defun dev-mode-setup-editing-tools ()
  "Configure advanced editing tools."
  (use-package multiple-cursors
    :ensure t
    :bind (("C-S-l" . mc/edit-lines)
           ("C-S-d" . mc/mark-all-like-this)
           ("C->" . mc/mark-next-like-this)
           ("C-<" . mc/mark-previous-like-this)
           ("C-c m n" . mc/skip-to-next-like-this)
           ("C-c m p" . mc/skip-to-previous-like-this)
           ("C-S-<mouse-1>" . mc/add-cursor-on-click)))

  (use-package expand-region
    :ensure t
    :bind ("C-=" . er/expand-region))

  (use-package hl-todo
    :ensure t
    :hook (prog-mode . hl-todo-mode)
    :config
    (setq hl-todo-keyword-faces
          '(("TODO" . "#FF0000")
            ("FIXME" . "#FF0000")
            ("DEBUG" . "#A020F0")
            ("GOTCHA" . "#FF4500")
            ("STUB" . "#1E90FF"))))

  (use-package rainbow-delimiters
    :ensure t
    :hook (prog-mode . rainbow-delimiters-mode)))

(defun dev-mode-setup-languages ()
  "Configure language-specific settings."
  ;; C/C++ Configuration
  (use-package cc-mode
    :ensure nil
    :mode (("\\.cpp\\'" . c++-mode)
           ("\\.hpp\\'" . c++-mode)
           ("\\.cc\\'" . c++-mode)
           ("\\.hh\\'" . c++-mode)
           ("\\.cxx\\'" . c++-mode)
           ("\\.hxx\\'" . c++-mode))
    :config
    (setq c-default-style "linux"
          c-basic-offset 4)
    (add-hook 'c++-mode-hook 'electric-pair-local-mode)
    (add-hook 'c++-mode-hook
              (lambda ()
                (setq compile-command
                      (concat "g++ -Wall -Wextra -std=c++17 -g -o "
                              (file-name-sans-extension (buffer-name))
                              " "
                              (buffer-name)))
                (local-set-key (kbd "C-c c") 'compile)
                (local-set-key (kbd "C-c r") 'gdb)
                (local-set-key (kbd "C-c C-c") 'recompile))))

  (use-package clang-format
    :ensure t
    :bind (:map c++-mode-map
                ("C-c C-f" . clang-format-buffer)
                :map c-mode-map
                ("C-c C-f" . clang-format-buffer)))

  ;; Python Configuration
  (use-package python
    :ensure nil
    :mode ("\\.py\\'" . python-mode)
    :config
    (setq python-indent-offset 4)
    (setq python-shell-interpreter "python3")
    (add-hook 'python-mode-hook
              (lambda ()
                (setq compile-command (concat "python3 " (buffer-name)))
                (local-set-key (kbd "C-c c") 'compile)
                (local-set-key (kbd "C-c r") 'run-python)
                (local-set-key (kbd "C-c C-c") 'python-shell-send-buffer))))

  ;; QML Configuration - Always use qml-mode for .qml files (not JavaScript mode)
  (use-package qml-mode
    :ensure t
    :mode ("\\.qml\\'" . qml-mode)
    :init
    ;; Remove any potential js-mode associations for .qml files
    (setq auto-mode-alist (delete '("\\.qml\\'" . js-mode) auto-mode-alist))
    (setq auto-mode-alist (delete '("\\.qml\\'" . javascript-mode) auto-mode-alist))
    (setq auto-mode-alist (delete '("\\.qml\\'" . js2-mode) auto-mode-alist))
    (setq auto-mode-alist (delete '("\\.qml\\'" . js3-mode) auto-mode-alist))
    (setq auto-mode-alist (delete '("\\.qml\\'" . rjsx-mode) auto-mode-alist))
    :config
    ;; Ensure qml-mode is at the front of auto-mode-alist
    (add-to-list 'auto-mode-alist '("\\.qml\\'" . qml-mode))
    (message "QML mode configured for .qml files"))

  (use-package company-qml
    :ensure t
    :after (company qml-mode)
    :config
    (add-to-list 'company-backends 'company-qml)))

(defun dev-mode-setup-magit ()
  "Configure Magit for version control."
  (use-package magit
    :ensure t
    :bind (("C-x g" . magit-status)
           ("C-x M-g" . magit-dispatch)
           ("C-c g" . magit-file-dispatch))
    :config
    ;; Configure Magit integration
    (setq vc-follow-symlinks t)
    (setq vc-display-status t)  ; Enable to show VC status in mode line

    ;; Configure Magit status sections
    (setq magit-status-sections-hook
          '(magit-insert-status-headers
            magit-insert-merge-log
            magit-insert-rebase-sequence
            magit-insert-am-sequence
            magit-insert-sequencer-sequence
            magit-insert-bisect-output
            magit-insert-bisect-rest
            magit-insert-bisect-log
            magit-insert-untracked-files
            magit-insert-unstaged-changes
            magit-insert-staged-changes
            magit-insert-stashes
            magit-insert-unpushed-to-pushremote
            magit-insert-unpushed-to-upstream-or-recent
            magit-insert-unpulled-from-pushremote
            magit-insert-unpulled-from-upstream
            magit-insert-recent-commits))

    (setq magit-log-section-commit-count 10)
    (setq magit-log-arguments '("--graph" "--color" "--decorate" "--all"))
    (setq magit-log-section-arguments '("--graph" "--color" "--decorate" "-n256" "--all"))
    (setq magit-log-margin '(t "%Y-%m-%d %H:%M " magit-log-margin-width t 18))
    (setq magit-log-show-refname-after-summary t)
    (setq magit-diff-refine-hunk 'all))

  ;; Only enable magit-delta if delta is installed
  (when (executable-find "delta")
    (use-package magit-delta
      :ensure t
      :hook (magit-mode . magit-delta-mode)))

  (use-package treemacs-magit
    :ensure t
    :after (treemacs magit))

  ;; Custom Magit functions
  (defun magit-save-commit-as-patch ()
    "Save the commit at point as a patch file."
    (interactive)
    (let* ((commit (or (magit-commit-at-point)
                       (error "No commit at point")))
           (default-name (format "%s.patch"
                                (substring commit 0 (min 8 (length commit)))))
           (file (read-file-name "Save patch to file: "
                                default-directory
                                default-name
                                nil
                                default-name))
           (default-directory (magit-toplevel)))
      (if (zerop (shell-command
                  (format "git format-patch -1 %s --stdout > %s"
                          (shell-quote-argument commit)
                          (shell-quote-argument (expand-file-name file)))))
          (message "Patch saved to %s" file)
        (error "Failed to save patch"))))

  (with-eval-after-load 'magit
    (define-key magit-revision-mode-map (kbd "C-c C-p") 'magit-save-commit-as-patch)
    (define-key magit-log-mode-map (kbd "C-c C-p") 'magit-save-commit-as-patch)
    (define-key magit-log-select-mode-map (kbd "C-c C-p") 'magit-save-commit-as-patch))
  
  ;; Optional: Integrate diff-hl with Magit when both are available
  ;; Only enable if you want Magit to control diff-hl updates
  ;; Comment out these lines if diff-hl has issues with Magit
  (when (and (fboundp 'diff-hl-mode) 
             (not (bound-and-true-p diff-hl-disable-magit-integration)))
    (add-hook 'magit-pre-refresh-hook 'diff-hl-magit-pre-refresh)
    (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh))
  
  ;; Fix common Magit issues
  (defun magit-fix-refresh ()
    "Fix Magit refresh issues."
    (interactive)
    (setq magit-refresh-verbose t)
    (setq magit-git-executable (executable-find "git"))
    (when (and buffer-file-name (vc-backend buffer-file-name))
      (vc-refresh-state))
    (magit-refresh)
    (message "Magit refresh fixed. Git executable: %s" magit-git-executable))
  
  ;; Diagnose Magit issues  
  (defun magit-diagnose ()
    "Diagnose Magit configuration."
    (interactive)
    (let ((diagnosis '()))
      (push (format "Git executable: %s" (executable-find "git")) diagnosis)
      (push (format "Magit git executable: %s" magit-git-executable) diagnosis)
      (push (format "Delta installed: %s" (if (executable-find "delta") "yes" "no")) diagnosis)
      (push (format "Magit-delta mode: %s" (if (bound-and-true-p magit-delta-mode) "enabled" "disabled")) diagnosis)
      (push (format "Default directory: %s" default-directory) diagnosis)
      (push (format "In git repo: %s" (magit-toplevel)) diagnosis)
      (push (format "Git version: %s" (magit-git-version)) diagnosis)
      (push (format "Magit version: %s" (magit-version)) diagnosis)
      (message (mapconcat 'identity (nreverse diagnosis) "\n"))))
  
  ;; Disable magit-delta if causing issues
  (defun magit-disable-delta ()
    "Disable magit-delta mode."
    (interactive)
    (when (fboundp 'magit-delta-mode)
      (magit-delta-mode -1)
      (remove-hook 'magit-mode-hook 'magit-delta-mode)
      (message "Magit-delta disabled. Using standard git diff."))))

(defun dev-mode-setup-debugging ()
  "Configure debugging support."
  (use-package dap-mode
    :ensure t
    :commands dap-debug
    :config
    (require 'dap-python)
    (require 'dap-gdb-lldb)
    (dap-auto-configure-mode 1)
    (setq dap-auto-configure-features '(sessions locals controls tooltip))))

(defun dev-mode-custom-functions ()
  "Define custom development functions."
  (defun generate-cpp-tags ()
    "Generate TAGS file for C++ project."
    (interactive)
    (let ((project-root (or (projectile-project-root) default-directory)))
      (shell-command
       (format "cd %s && find . -name '*.cpp' -o -name '*.hpp' -o -name '*.cc' -o -name '*.hh' -o -name '*.c' -o -name '*.h' | etags -"
               project-root))
      (visit-tags-table (concat project-root "TAGS"))
      (message "C++ TAGS file generated for project: %s" project-root)))

  (defun generate-python-tags ()
    "Generate TAGS file for Python project."
    (interactive)
    (let ((project-root (or (projectile-project-root) default-directory)))
      (shell-command
       (format "cd %s && find . -name '*.py' | etags -"
               project-root))
      (visit-tags-table (concat project-root "TAGS"))
      (message "Python TAGS file generated for project: %s" project-root)))

  (defun generate-all-tags ()
    "Generate TAGS file for both C++ and Python files."
    (interactive)
    (let ((project-root (or (projectile-project-root) default-directory)))
      (shell-command
       (format "cd %s && find . -name '*.cpp' -o -name '*.hpp' -o -name '*.cc' -o -name '*.hh' -o -name '*.c' -o -name '*.h' -o -name '*.py' | etags -"
               project-root))
      (visit-tags-table (concat project-root "TAGS"))
      (message "TAGS file generated for all supported files in: %s" project-root)))

  (defun quick-compile-and-run ()
    "Quick compile and run current file."
    (interactive)
    (save-buffer)
    (cond
     ((derived-mode-p 'c++-mode)
      (shell-command
       (format "g++ -std=c++17 -o %s %s && ./%s"
               (file-name-sans-extension (buffer-name))
               (buffer-name)
               (file-name-sans-extension (buffer-name)))))
     ((derived-mode-p 'python-mode)
      (shell-command (format "python3 %s" (buffer-name))))
     (t (message "Unsupported file type for quick compile and run")))))

(defun dev-mode-setup-keybindings ()
  "Set up development-specific keybindings."
  (global-set-key (kbd "C-c t c") 'generate-cpp-tags)
  (global-set-key (kbd "C-c t p") 'generate-python-tags)
  (global-set-key (kbd "C-c t a") 'generate-all-tags)
  (global-set-key (kbd "C-c q") 'quick-compile-and-run)
  (global-set-key (kbd "M-.") 'xref-find-definitions)
  (global-set-key (kbd "M-,") 'xref-pop-marker-stack)
  (global-set-key (kbd "C-M-.") 'xref-find-references)
  
  ;; Helper function to refresh origami in current buffer
  (defun origami-reset-buffer ()
    "Reset origami in the current buffer."
    (interactive)
    (when (bound-and-true-p origami-mode)
      (origami-mode -1)
      (origami-mode 1)
      (message "Origami reset in buffer")))
  
  (global-set-key (kbd "C-c f 0") 'origami-reset-buffer))

(defun show-dev-mode-help ()
  "Show key bindings for development configuration."
  (interactive)
  (with-output-to-temp-buffer "*Development Mode Help*"
    (princ "=== Development Mode Key Bindings ===\n\n")
    (princ "LSP COMMANDS:\n")
    (princ "  C-c l      : LSP prefix for all LSP commands\n")
    (princ "  M-.        : Go to definition\n")
    (princ "  M-,        : Return from definition\n")
    (princ "  C-M-.      : Find references\n\n")
    (princ "TAGS & NAVIGATION:\n")
    (princ "  C-c t c    : Generate C++ TAGS file\n")
    (princ "  C-c t p    : Generate Python TAGS file\n")
    (princ "  C-c t a    : Generate TAGS for all files\n\n")
    (princ "COMPILATION & EXECUTION:\n")
    (princ "  C-c c      : Compile current file\n")
    (princ "  C-c r      : Run (GDB for C++, Python REPL for Python)\n")
    (princ "  C-c q      : Quick compile and run\n")
    (princ "  C-c C-c    : Recompile (C++) or Send buffer to Python\n\n")
    (princ "PROJECT MANAGEMENT:\n")
    (princ "  C-c p      : Projectile commands prefix\n\n")
    (princ "VERSION CONTROL (MAGIT):\n")
    (princ "  C-x g      : Magit status\n")
    (princ "  C-x M-g    : Magit dispatch\n")
    (princ "  C-c g      : Magit file dispatch\n")
    (princ "  C-c C-p    : Save commit as patch (in Magit buffers)\n\n")
    (princ "EDITING:\n")
    (princ "  C-=        : Expand region\n")
    (princ "  C->        : Mark next like this (multiple cursors)\n")
    (princ "  C-<        : Mark previous like this\n")
    (princ "  C-S-d      : Mark all like this\n\n")
    (princ "CODE FOLDING (ORIGAMI):\n")
    (princ "  C-c f f    : Toggle fold at point\n")
    (princ "  C-c f o    : Open fold\n")
    (princ "  C-c f c    : Close fold\n")
    (princ "  C-c f a    : Close all folds\n")
    (princ "  C-c f A    : Open all folds\n")
    (princ "  C-c f t    : Toggle all folds\n")
    (princ "  C-c f r    : Recursively toggle fold\n")
    (princ "  C-c f n/p  : Next/Previous fold\n")
    (princ "  C-c f s    : Show only current fold\n\n")
    (princ "LANGUAGE MODES:\n")
    (princ "  .qml files : Always use qml-mode (not JavaScript mode)\n")
    (princ "  .py files  : Python mode with LSP support\n")
    (princ "  .cpp/.hpp  : C++ mode with LSP support\n\n")
    (princ "DEVELOPMENT MODE:\n")
    (princ "  M-x enable-dev-mode  : Enable development mode\n")
    (princ "  M-x disable-dev-mode : Disable development mode\n")
    (princ "  C-c h                : Show this help\n")))

;;;###autoload
(defun enable-dev-mode ()
  "Enable development mode with all programming tools."
  (interactive)
  (if dev-mode-enabled
      (message "Development mode is already enabled")
    (message "Enabling development mode...")
    ;; Ensure packages are installed
    (dev-mode-ensure-packages)
    ;; Set up all development features
    (dev-mode-setup-lsp)
    (dev-mode-setup-company)
    (dev-mode-setup-flycheck)
    (dev-mode-setup-yasnippet)
    (dev-mode-setup-projectile)
    (dev-mode-setup-ggtags)
    (dev-mode-setup-origami)
    (dev-mode-setup-editing-tools)
    (dev-mode-setup-languages)
    (dev-mode-setup-magit)
    (dev-mode-setup-debugging)
    (dev-mode-custom-functions)
    (dev-mode-setup-keybindings)
    ;; Ensure QML files use qml-mode (override any JS mode associations)
    (add-to-list 'auto-mode-alist '("\\.qml\\'" . qml-mode))
    ;; Set up help command
    (global-set-key (kbd "C-c h") 'show-dev-mode-help)
    (setq dev-mode-enabled t)
    (message "Development mode enabled! Press C-c h for help. QML files will use qml-mode.")))

;;;###autoload
(defun disable-dev-mode ()
  "Disable development mode."
  (interactive)
  (if (not dev-mode-enabled)
      (message "Development mode is not enabled")
    (message "Disabling development mode...")
    ;; Disable major modes that can be toggled
    (global-company-mode -1)
    (global-flycheck-mode -1)
    (yas-global-mode -1)
    (projectile-mode -1)
    (global-hl-todo-mode -1)
    ;; Note: Some modes might require restarting Emacs to fully disable
    (setq dev-mode-enabled nil)
    (message "Development mode disabled. Some features may require restarting Emacs to fully disable.")))

;;;###autoload
(defun dev-mode-status ()
  "Check if development mode is enabled."
  (interactive)
  (if dev-mode-enabled
      (message "Development mode is ENABLED")
    (message "Development mode is DISABLED")))

(provide 'emacs-dev-config)
;;; emacs-dev-config.el ends here