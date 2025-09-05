;;; -*- lexical-binding: t -*-
;; QML Language Server Configuration Helper
;; This file provides enhanced QML auto-completion setup

;; Function to find QML language server executable
(defun find-qml-language-server ()
  "Find the QML language server executable in common locations."
  (or (executable-find "qml-lsp")
      (executable-find "qmlls")
      (executable-find "qml6-lsp")
      ;; Try common Qt installation paths
      "/usr/lib/qt6/bin/qmlls"
      "/usr/lib/x86_64-linux-gnu/qt6/bin/qmlls"
      "/opt/qt6/bin/qmlls"
      ;; Fallback - will show an error if not found
      "qmlls"))

;; Enhanced QML mode configuration
(use-package qml-mode
  :ensure t
  :mode ("\\.qml\\'" . qml-mode)
  :config
  ;; Set up proper indentation
  (setq qml-indent-offset 4)
  
  ;; Add QML-specific keywords for better syntax highlighting
  (font-lock-add-keywords 'qml-mode
                          '(("\\<\\(readonly\\|default\\|signal\\|alias\\)\\>" . font-lock-keyword-face)
                            ("\\<\\(Qt\\|QtQuick\\|QtQuick\\.Controls\\)\\>" . font-lock-constant-face))))

;; Enhanced LSP configuration for QML
(with-eval-after-load 'lsp-mode
  ;; Register QML language server
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection 
                                     (lambda () (list (find-qml-language-server))))
                    :activation-fn (lsp-activate-on "qml")
                    :server-id 'qmlls))
  
  ;; QML-specific LSP settings
  (setq lsp-qml-server-command (find-qml-language-server))
  
  ;; Enable LSP for QML files
  (add-to-list 'lsp-language-id-configuration '(qml-mode . "qml")))

;; Enhanced company configuration for QML with error handling
(with-eval-after-load 'company
  ;; Check if company-qml is working properly
  (defun company-qml-safe-p ()
    "Check if company-qml backend is available and working."
    (and (featurep 'company-qml)
         (condition-case nil
             (progn 
               (company-qml 'candidates "test")
               t)
           (error nil))))
  
  ;; QML-specific company backends with fallback
  (defun setup-qml-company-backends ()
    "Set up company backends for QML mode with error handling."
    (if (company-qml-safe-p)
        ;; Use company-qml if it works
        (setq-local company-backends
                    '((company-qml 
                       company-capf 
                       company-files 
                       company-yasnippet 
                       company-dabbrev-code)
                      company-dabbrev))
      ;; Fallback without company-qml
      (progn
        (message "company-qml not available or has errors, using LSP-based completion")
        (setq-local company-backends
                    '((company-capf       ; LSP completions (primary for QML)
                       company-files      ; File completions
                       company-yasnippet  ; Snippet completions
                       company-dabbrev-code ; Code word completions
                       company-keywords)  ; Language keywords
                      company-dabbrev))))) ; General word completions
  
  ;; Apply to QML mode
  (add-hook 'qml-mode-hook 'setup-qml-company-backends))

;; QML snippets configuration
(with-eval-after-load 'yasnippet
  ;; Create QML snippets directory if it doesn't exist
  (let ((qml-snippets-dir (expand-file-name "snippets/qml-mode" user-emacs-directory)))
    (unless (file-exists-p qml-snippets-dir)
      (make-directory qml-snippets-dir t))))

;; Enhanced QML development settings
(add-hook 'qml-mode-hook
          (lambda ()
            ;; Enable electric pair mode for automatic bracket/quote pairing
            (electric-pair-local-mode 1)
            ;; Enable automatic indentation
            (electric-indent-local-mode 1)
            ;; Set tab width
            (setq tab-width 4)
            ;; Use spaces instead of tabs
            (setq indent-tabs-mode nil)
            ;; Enable line numbers
            (display-line-numbers-mode 1)
            ;; Enable syntax checking
            (flycheck-mode 1)))

;; Key bindings for QML development
(with-eval-after-load 'qml-mode
  (define-key qml-mode-map (kbd "C-c C-r") 'lsp-rename)
  (define-key qml-mode-map (kbd "C-c C-d") 'lsp-find-definition)
  (define-key qml-mode-map (kbd "C-c C-f") 'lsp-find-references))

(provide 'qml-config)
