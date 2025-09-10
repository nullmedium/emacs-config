;;; init-eslint-fix.el --- Fix ESLint configuration issues -*- lexical-binding: t -*-
;;; Commentary:
;;; Handles ESLint configuration issues when no .eslintrc is present

;;; Code:

;; Function to check if ESLint is configured in the current project
(defun project-has-eslint-config-p ()
  "Check if the current project has ESLint configuration."
  (require 'project)
  (let ((project-root (or (when-let ((proj (project-current)))
                            (project-root proj))
                          (locate-dominating-file default-directory ".git")
                          default-directory)))
    (or (file-exists-p (expand-file-name ".eslintrc" project-root))
        (file-exists-p (expand-file-name ".eslintrc.js" project-root))
        (file-exists-p (expand-file-name ".eslintrc.json" project-root))
        (file-exists-p (expand-file-name ".eslintrc.yml" project-root))
        (file-exists-p (expand-file-name ".eslintrc.yaml" project-root))
        (file-exists-p (expand-file-name "eslint.config.js" project-root))
        (file-exists-p (expand-file-name "package.json" project-root)))))

;; Disable ESLint for TypeScript/JavaScript if no config exists
(defun maybe-disable-eslint ()
  "Disable ESLint if no configuration is found."
  (when (and (derived-mode-p 'js-mode 'js-ts-mode 'typescript-mode 'typescript-ts-mode 'tsx-ts-mode)
             (not (project-has-eslint-config-p)))
    ;; Tell Eglot to ignore ESLint diagnostics
    (setq-local eglot-ignored-server-capabilities 
                (append eglot-ignored-server-capabilities 
                        '(:documentFormattingProvider :documentRangeFormattingProvider)))
    (message "ESLint disabled - no configuration found in project")))

;; Hook to check ESLint configuration
(add-hook 'eglot-managed-mode-hook #'maybe-disable-eslint)

;; Alternative: Completely disable ESLint in typescript-language-server
(with-eval-after-load 'eglot
  (defun eglot-disable-eslint-for-typescript ()
    "Disable ESLint for TypeScript language server."
    (when (cl-find-if (lambda (server)
                        (and (eglot--server-capable server :textDocument/publishDiagnostics)
                             (string-match-p "typescript" (eglot--project-nickname server))))
                      (eglot-current-server))
      (setq-local eglot-workspace-configuration
                  '(:typescript (:validate (:enable :json-false))
                    :javascript (:validate (:enable :json-false))
                    :eslint (:enable :json-false))))))

;; Function to manually disable ESLint in current buffer
(defun disable-eslint-in-buffer ()
  "Disable ESLint validation in the current buffer."
  (interactive)
  (when (eglot-current-server)
    (eglot--server-capable-or-lose (eglot-current-server) :workspace/didChangeConfiguration)
    (jsonrpc-notify
     (eglot-current-server)
     :workspace/didChangeConfiguration
     '(:settings (:eslint (:enable :json-false)
                  :validate (:enable :json-false))))
    (message "ESLint disabled in current buffer")))

;; Function to create a basic .eslintrc if needed
(defun create-basic-eslintrc ()
  "Create a basic .eslintrc.json file in the project root."
  (interactive)
  (require 'project)
  (let* ((project-root (or (when-let ((proj (project-current)))
                             (project-root proj))
                           (locate-dominating-file default-directory ".git")
                           default-directory))
         (eslintrc-path (expand-file-name ".eslintrc.json" project-root)))
    (if (file-exists-p eslintrc-path)
        (message ".eslintrc.json already exists")
      (with-temp-file eslintrc-path
        (insert "{\n")
        (insert "  \"env\": {\n")
        (insert "    \"browser\": true,\n")
        (insert "    \"es2021\": true,\n")
        (insert "    \"node\": true\n")
        (insert "  },\n")
        (insert "  \"extends\": \"eslint:recommended\",\n")
        (insert "  \"parserOptions\": {\n")
        (insert "    \"ecmaVersion\": \"latest\",\n")
        (insert "    \"sourceType\": \"module\"\n")
        (insert "  },\n")
        (insert "  \"rules\": {}\n")
        (insert "}\n"))
      (message "Created basic .eslintrc.json in %s" project-root))))

;; Global keybinding to disable ESLint - using C-c L e for LSP ESLint commands
(global-set-key (kbd "C-c L e d") 'disable-eslint-in-buffer)
(global-set-key (kbd "C-c L e c") 'create-basic-eslintrc)

;; Advice to suppress ESLint errors in the echo area
(defun suppress-eslint-errors (orig-fun &rest args)
  "Suppress ESLint configuration errors."
  (let ((msg (apply orig-fun args)))
    (if (and msg (string-match-p "ESLint.*not configured" msg))
        nil  ; Don't show the message
      msg)))

(advice-add 'eglot--message :around #'suppress-eslint-errors)

(provide 'init-eslint-fix)
;;; init-eslint-fix.el ends here