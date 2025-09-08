;;; init-eglot.el --- Eglot (built-in LSP) configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; Lightweight LSP client using Emacs built-in Eglot (Emacs 29+)

;;; Code:

;; Eglot is built-in for Emacs 29+, otherwise install it
(unless (fboundp 'eglot)
  (use-package eglot
    :ensure t))

;; Configure Eglot
(with-eval-after-load 'eglot
  ;; Performance tuning
  (setq eglot-autoshutdown t)  ; Shutdown language server after closing last file
  (setq eglot-sync-connect 0)  ; Don't block on language server startup
  (setq eglot-connect-timeout 10)  ; Connection timeout in seconds
  (setq eglot-events-buffer-size 0)  ; Disable events buffer for performance
  (setq eglot-ignored-server-capabilities '(:documentHighlightProvider))  ; Disable some features for speed
  
  ;; Don't let Eglot manage Flymake, we'll configure it separately
  (setq eglot-stay-out-of '(flymake))
  
  ;; Workspace configuration for different servers
  (setq-default eglot-workspace-configuration
                '(:pylsp (:plugins (:pycodestyle (:enabled t)
                                    :pyflakes (:enabled t)
                                    :pylint (:enabled nil)))
                  :gopls (:usePlaceholders t)
                  ;; Disable ESLint for TypeScript/JavaScript by default
                  :typescript (:validate (:enable :json-false))
                  :javascript (:validate (:enable :json-false))
                  :eslint (:enable :json-false)))
  
  ;; Configure language servers
  (add-to-list 'eglot-server-programs
               '((c-mode c++-mode c-ts-mode c++-ts-mode) . ("clangd"
                                                             "--background-index"
                                                             "--clang-tidy"
                                                             "--completion-style=detailed"
                                                             "--header-insertion=iwyu"
                                                             "--pch-storage=memory")))
  
  (add-to-list 'eglot-server-programs
               '((python-mode python-ts-mode) . ("pylsp")))
  
  (add-to-list 'eglot-server-programs
               '((rust-mode rust-ts-mode) . ("rust-analyzer")))
  
  (add-to-list 'eglot-server-programs
               '((go-mode go-ts-mode) . ("gopls")))
  
  (add-to-list 'eglot-server-programs
               '((typescript-mode typescript-ts-mode tsx-ts-mode) . ("typescript-language-server" "--stdio")))
  
  (add-to-list 'eglot-server-programs
               '((js-mode js-ts-mode) . ("typescript-language-server" "--stdio")))

  ;; QML support (if qmlls is available)
  (when (executable-find "qmlls")
    (add-to-list 'eglot-server-programs
                 '(qml-mode . ("qmlls"))))

  ;; Format on save
  (defun eglot-format-buffer-on-save ()
    "Format buffer with eglot before saving."
    (add-hook 'before-save-hook #'eglot-format-buffer -10 t))
  
  ;; Keybindings
  (define-key eglot-mode-map (kbd "C-c l r") 'eglot-rename)
  (define-key eglot-mode-map (kbd "C-c l a") 'eglot-code-actions)
  (define-key eglot-mode-map (kbd "C-c l f") 'eglot-format)
  (define-key eglot-mode-map (kbd "C-c l F") 'eglot-format-buffer)
  (define-key eglot-mode-map (kbd "C-c l h") 'eldoc)
  (define-key eglot-mode-map (kbd "C-c l d") 'xref-find-definitions)
  (define-key eglot-mode-map (kbd "C-c l R") 'xref-find-references)
  (define-key eglot-mode-map (kbd "C-c l i") 'eglot-find-implementation)
  (define-key eglot-mode-map (kbd "C-c l t") 'eglot-find-typeDefinition))

;; Hook eglot to programming modes
(defun maybe-enable-eglot ()
  "Enable eglot if a language server is available."
  (when (and (not (bound-and-true-p lsp-mode))  ; Don't conflict with lsp-mode
             (eglot-current-server))  ; Only if server is available
    (eglot-ensure)))

;; Enable eglot for specific modes
(dolist (mode '(c-mode-hook
                c++-mode-hook
                c-ts-mode-hook
                c++-ts-mode-hook
                python-mode-hook
                python-ts-mode-hook
                rust-mode-hook
                rust-ts-mode-hook
                go-mode-hook
                go-ts-mode-hook
                js-mode-hook
                js-ts-mode-hook
                typescript-mode-hook
                typescript-ts-mode-hook
                tsx-ts-mode-hook))
  (add-hook mode #'eglot-ensure))

;; Integration with Corfu for completion
(with-eval-after-load 'corfu
  (setq completion-category-overrides '((eglot (styles orderless))))
  (add-hook 'eglot-managed-mode-hook
            (lambda ()
              (setq-local completion-at-point-functions
                          (list (cape-super-capf
                                 #'eglot-completion-at-point
                                 #'cape-file))))))

;; Eldoc configuration for better documentation display
(with-eval-after-load 'eldoc
  (setq eldoc-echo-area-use-multiline-p nil)  ; Don't use multi-line eldoc
  (setq eldoc-idle-delay 0.5))

;; Diagnostic display with Flymake (built-in)
(with-eval-after-load 'flymake
  (define-key flymake-mode-map (kbd "C-c ! n") 'flymake-goto-next-error)
  (define-key flymake-mode-map (kbd "C-c ! p") 'flymake-goto-prev-error)
  (define-key flymake-mode-map (kbd "C-c ! l") 'flymake-show-buffer-diagnostics)
  (define-key flymake-mode-map (kbd "C-c ! L") 'flymake-show-project-diagnostics))

;; Helper functions
(defun eglot-organize-imports ()
  "Organize imports using Eglot."
  (interactive)
  (eglot-code-actions nil nil "source.organizeImports" t))

(defun eglot-restart-server ()
  "Restart the Eglot server."
  (interactive)
  (call-interactively #'eglot-shutdown)
  (call-interactively #'eglot))

(provide 'init-eglot)
;;; init-eglot.el ends here