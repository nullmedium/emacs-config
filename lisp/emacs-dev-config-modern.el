;;; emacs-dev-config-modern.el --- Modern development configuration using Eglot -*- lexical-binding: t -*-
;;; Commentary:
;;; Development configuration using built-in Eglot instead of lsp-mode
;;; This is a lighter, faster alternative to the original dev config

;;; Code:

(defvar dev-mode-modern-enabled nil
  "Flag indicating whether modern development mode is enabled.")

(defvar dev-mode-modern-packages
  '(;; Core development tools
    eglot  ; Only needed for Emacs < 29
    corfu corfu-terminal cape  ; Modern completion
    consult-eglot  ; Consult integration with Eglot
    flycheck  ; Can still use alongside Flymake
    yasnippet
    ggtags
    multiple-cursors expand-region
    hl-todo rainbow-delimiters
    origami  ; Code folding
    
    ;; Version control
    magit
    forge  ; GitHub/GitLab integration
    magit-delta  ; Better diffs if delta is installed
    treemacs-magit
    
    ;; Languages
    clang-format
    qml-mode
    
    ;; Debugging
    dap-mode)
  "List of packages for modern development mode.")

(defun dev-mode-modern-ensure-packages ()
  "Ensure all modern development packages are installed."
  (dolist (package dev-mode-modern-packages)
    (unless (or (package-installed-p package)
                (and (eq package 'eglot) (fboundp 'eglot)))  ; Eglot is built-in for Emacs 29+
      (package-refresh-contents)
      (package-install package))))

(defun dev-mode-modern-setup-eglot ()
  "Setup Eglot for modern development."
  ;; Load eglot configuration
  (require 'init-eglot)
  
  ;; Additional Eglot configuration for development
  (with-eval-after-load 'eglot
    ;; Enable format on save for specific modes
    (dolist (mode '(c-mode-hook c++-mode-hook python-mode-hook))
      (add-hook mode #'eglot-format-buffer-on-save))
    
    ;; Configure Consult-Eglot if available
    (when (fboundp 'consult-eglot-symbols)
      (define-key eglot-mode-map (kbd "C-c l s") 'consult-eglot-symbols))))

(defun dev-mode-modern-setup-completion ()
  "Setup modern completion with Corfu."
  ;; Corfu is already configured in init-completion.el
  ;; Add development-specific configurations here
  (with-eval-after-load 'corfu
    ;; More aggressive completion in programming modes
    (add-hook 'prog-mode-hook
              (lambda ()
                (setq-local corfu-auto-delay 0.1)
                (setq-local corfu-auto-prefix 1)))))

(defun dev-mode-modern-setup-yasnippet ()
  "Configure yasnippet for code snippets."
  (use-package yasnippet
    :ensure t
    :config
    (yas-global-mode 1)
    ;; Load snippets from the snippets directory if it exists
    (let ((snippets-dir (expand-file-name "snippets" user-emacs-directory)))
      (when (file-directory-p snippets-dir)
        (add-to-list 'yas-snippet-dirs snippets-dir)))))

(defun dev-mode-modern-setup-flycheck ()
  "Configure Flycheck alongside Flymake."
  (use-package flycheck
    :ensure t
    :init
    ;; Use Flycheck for modes not well-supported by Flymake
    (add-hook 'sh-mode-hook 'flycheck-mode)
    (add-hook 'json-mode-hook 'flycheck-mode)
    (add-hook 'yaml-mode-hook 'flycheck-mode)
    :config
    (setq flycheck-display-errors-delay 0.3)))

(defun dev-mode-modern-setup-project ()
  "Configure project.el for project management."
  ;; Already configured in init-project.el
  ;; Add development-specific configurations here
  (with-eval-after-load 'project
    ;; Add test and compile commands to project prefix
    (define-key project-prefix-map (kbd "t") 'project-compile)
    (define-key project-prefix-map (kbd "T") 'recompile)))

(defun dev-mode-modern-setup-magit ()
  "Configure Magit for version control."
  (use-package magit
    :ensure t
    :bind (("C-x g" . magit-status)
           ("C-x M-g" . magit-dispatch)
           ("C-c g" . magit-file-dispatch))
    :config
    (setq magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))
  
  ;; Forge for GitHub/GitLab integration
  (use-package forge
    :ensure t
    :after magit)
  
  ;; Magit-delta for better diffs (if delta is installed)
  (when (executable-find "delta")
    (use-package magit-delta
      :ensure t
      :hook (magit-mode . magit-delta-mode))))

(defun dev-mode-modern-setup-debugging ()
  "Configure debugging with dap-mode."
  (use-package dap-mode
    :ensure t
    :commands (dap-debug dap-debug-edit-template)
    ;; Don't auto-enable - only load when explicitly needed for debugging
    :config
    ;; Python debugging
    (require 'dap-python)
    ;; C/C++ debugging
    (require 'dap-gdb-lldb)
    ;; Don't auto-configure globally - causes severe performance issues
    ;; Enable manually when debugging: M-x dap-auto-configure-mode
    ;; (dap-auto-configure-mode 1)  ; Commented out for performance
    ))

(defun dev-mode-modern-setup-languages ()
  "Configure language-specific settings."
  ;; C/C++ formatting
  (use-package clang-format
    :ensure t
    :bind (:map c-mode-map
                ("C-c C-f" . clang-format-buffer)
                :map c++-mode-map
                ("C-c C-f" . clang-format-buffer)))
  
  ;; QML support
  (use-package qml-mode
    :ensure t
    :mode "\\.qml\\'"
    ;; Removed qml-mode hook - Qt5 has no language server
    ))

(defun dev-mode-modern-setup-editing-tools ()
  "Setup advanced editing tools for development."
  ;; These are now in init-qol.el, but we can add dev-specific configs
  (with-eval-after-load 'hl-todo
    (add-hook 'prog-mode-hook #'hl-todo-mode))
  
  (with-eval-after-load 'rainbow-delimiters
    (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))
  
  ;; Origami for code folding
  (use-package origami
    :ensure t
    :hook (prog-mode . origami-mode)
    :bind (:map origami-mode-map
                ("C-c o f" . origami-toggle-node)
                ("C-c o o" . origami-open-node)
                ("C-c o c" . origami-close-node)
                ("C-c o a" . origami-close-all-nodes)
                ("C-c o A" . origami-open-all-nodes))))

(defun dev-mode-modern-setup-keybindings ()
  "Setup development-specific keybindings."
  ;; Compile commands
  (global-set-key (kbd "<f5>") 'compile)
  (global-set-key (kbd "<f6>") 'recompile)
  
  ;; Testing - use C-c C-t prefix to avoid conflict with CUA copy
  (global-set-key (kbd "C-c C-t p") 'project-compile)
  (global-set-key (kbd "C-c C-t r") 'recompile)
  
  ;; Navigation
  (global-set-key (kbd "M-.") 'xref-find-definitions)
  (global-set-key (kbd "M-,") 'xref-pop-marker-stack)
  (global-set-key (kbd "M-?") 'xref-find-references))

;;;###autoload
(defun enable-dev-mode-modern ()
  "Enable modern development mode with Eglot and other tools."
  (interactive)
  (if dev-mode-modern-enabled
      (message "Modern development mode is already enabled")
    (message "Enabling modern development mode...")
    ;; Ensure packages are installed
    (dev-mode-modern-ensure-packages)
    ;; Set up all development features
    (dev-mode-modern-setup-eglot)
    (dev-mode-modern-setup-completion)
    (dev-mode-modern-setup-yasnippet)
    (dev-mode-modern-setup-flycheck)
    (dev-mode-modern-setup-project)
    (dev-mode-modern-setup-magit)
    (dev-mode-modern-setup-debugging)
    (dev-mode-modern-setup-languages)
    (dev-mode-modern-setup-editing-tools)
    (dev-mode-modern-setup-keybindings)
    ;; Load tree-sitter if available
    (when (file-exists-p (expand-file-name "lisp/init-treesitter.el" user-emacs-directory))
      (require 'init-treesitter))
    ;; Stop elfeed auto-updates to prevent UI lag
    (when (fboundp 'elfeed-stop-auto-updates)
      (elfeed-stop-auto-updates))
    (setq dev-mode-modern-enabled t)
    (message "Modern development mode enabled! Eglot will auto-start for supported files. Elfeed auto-updates disabled.")))

;;;###autoload
(defun disable-dev-mode-modern ()
  "Disable modern development mode."
  (interactive)
  (if (not dev-mode-modern-enabled)
      (message "Modern development mode is not enabled")
    (message "Disabling modern development mode...")
    ;; Shutdown all Eglot servers
    (when (fboundp 'eglot-shutdown-all)
      (eglot-shutdown-all))
    ;; Disable some modes
    (yas-global-mode -1)
    (global-flycheck-mode -1)
    ;; Re-enable elfeed auto-updates
    (when (fboundp 'elfeed-start-auto-updates)
      (elfeed-start-auto-updates))
    (setq dev-mode-modern-enabled nil)
    (message "Modern development mode disabled. Elfeed auto-updates re-enabled.")))

(provide 'emacs-dev-config-modern)
;;; emacs-dev-config-modern.el ends here