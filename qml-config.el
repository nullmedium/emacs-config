;;; -*- lexical-binding: t -*-
;; QML Mode Configuration (Qt5 - No LSP)
;; This file provides QML mode setup without LSP (Qt5 has no language server)

;; Basic QML mode configuration
(use-package qml-mode
  :ensure t
  :mode ("\\.qml\\'" . qml-mode)
  :config
  ;; Set up proper indentation
  (setq qml-indent-offset 4)
  
  ;; Add QML-specific keywords for better syntax highlighting
  (font-lock-add-keywords 'qml-mode
                          '(("\\<\\(readonly\\|default\\|signal\\|alias\\|property\\|required\\)\\>" . font-lock-keyword-face)
                            ("\\<\\(Qt\\|QtQuick\\|QtQuick\\.Controls\\|QtQuick\\.Layouts\\)\\>" . font-lock-constant-face)
                            ("\\<\\(Item\\|Rectangle\\|Text\\|Image\\|MouseArea\\|Column\\|Row\\|Grid\\|ListView\\|GridView\\)\\>" . font-lock-type-face))))

;; Company backends for QML (without LSP)
(with-eval-after-load 'company
  (defun setup-qml-company-backends ()
    "Set up company backends for QML mode without LSP."
    (setq-local company-backends
                '((company-dabbrev-code   ; Code word completions
                   company-keywords        ; Language keywords
                   company-files           ; File completions
                   company-yasnippet)      ; Snippet completions
                  company-dabbrev)))       ; General word completions
  
  ;; Apply to QML mode
  (add-hook 'qml-mode-hook 'setup-qml-company-backends))

;; QML snippets configuration
(with-eval-after-load 'yasnippet
  ;; Create QML snippets directory if it doesn't exist
  (let ((qml-snippets-dir (expand-file-name "snippets/qml-mode" user-emacs-directory)))
    (unless (file-exists-p qml-snippets-dir)
      (make-directory qml-snippets-dir t))))

;; Prevent LSP from being registered for QML
(with-eval-after-load 'lsp-mode
  ;; Remove QML from LSP language configurations
  (setq lsp-language-id-configuration 
        (assq-delete-all 'qml-mode lsp-language-id-configuration))
  ;; Unregister any QML LSP clients
  (when (boundp 'lsp-clients)
    (setq lsp-clients (delq 'qmlls lsp-clients))))

;; Prevent Eglot from activating in QML mode
(with-eval-after-load 'eglot
  ;; Remove QML from eglot server programs if present
  (setq eglot-server-programs 
        (assq-delete-all 'qml-mode eglot-server-programs)))

;; QML development settings
(add-hook 'qml-mode-hook
          (lambda ()
            ;; Forcefully disable LSP for QML files (Qt5 has no language server)
            (when (bound-and-true-p lsp-mode)
              (lsp-disconnect)
              (lsp-mode -1))
            (when (bound-and-true-p lsp-managed-mode)
              (lsp-managed-mode -1))
            ;; Also disable Eglot if it tries to start
            (when (bound-and-true-p eglot--managed-mode)
              (eglot-shutdown)
              (eglot--managed-mode -1))
            ;; Electric pair mode for automatic bracket/quote pairing
            (electric-pair-local-mode 1)
            ;; Enable automatic indentation
            (electric-indent-local-mode 1)
            ;; Set tab width
            (setq tab-width 4)
            ;; Use spaces instead of tabs
            (setq indent-tabs-mode nil)
            ;; Enable line numbers
            (display-line-numbers-mode 1)
            ;; Disable flycheck (no QML checker for Qt5)
            (when (bound-and-true-p flycheck-mode)
              (flycheck-mode -1))))

;; Simple navigation functions for QML
(defun qml-find-definition ()
  "Simple definition finder using grep."
  (interactive)
  (let ((thing (thing-at-point 'symbol)))
    (when thing
      (grep (format "grep -n \"\\b%s\\b\" *.qml" thing)))))

(defun qml-find-references ()
  "Simple reference finder using grep."
  (interactive)
  (let ((thing (thing-at-point 'symbol)))
    (when thing
      (grep (format "grep -n \"\\b%s\\b\" *.qml" thing)))))

;; Key bindings for QML development (without LSP)
(with-eval-after-load 'qml-mode
  (define-key qml-mode-map (kbd "C-c C-d") 'qml-find-definition)
  (define-key qml-mode-map (kbd "C-c C-f") 'qml-find-references)
  (define-key qml-mode-map (kbd "C-c C-c") 'comment-region)
  (define-key qml-mode-map (kbd "C-c C-u") 'uncomment-region))

;; Helper function to insert common QML snippets
(defun qml-insert-property ()
  "Insert a QML property declaration."
  (interactive)
  (insert "property ")
  (save-excursion (insert ": ")))

(defun qml-insert-signal ()
  "Insert a QML signal declaration."
  (interactive)
  (insert "signal ")
  (save-excursion (insert "()")))

(defun qml-insert-function ()
  "Insert a QML function declaration."
  (interactive)
  (insert "function ")
  (save-excursion (insert "() {\n    \n}")))

;; Add snippet key bindings
(with-eval-after-load 'qml-mode
  (define-key qml-mode-map (kbd "C-c i p") 'qml-insert-property)
  (define-key qml-mode-map (kbd "C-c i s") 'qml-insert-signal)
  (define-key qml-mode-map (kbd "C-c i f") 'qml-insert-function))

(provide 'qml-config)