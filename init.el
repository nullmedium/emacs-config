;;; init.el --- Main Emacs configuration file -*- lexical-binding: t -*-
;;; Commentary:
;;; This is the main configuration file that loads all modular configuration files.
;;; The configuration has been split into logical modules for better organization
;;; and maintainability.

;;; Code:

;; Add lisp subdirectory to load path (avoids load-path warning)
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; EMERGENCY FIX - Load this first to ensure editing works
(require 'init-emergency-fix)
;; (require 'init-seq-fix)  ; Fix seq library issues

;; Load performance optimizations early
;; (require 'init-performance)

;;; Load core modules in order
(require 'init-core)      ; Core settings and package management
(require 'init-completion); Modern completion with Vertico, Consult, etc.
(require 'init-ui)        ; UI and theme configurations
(require 'init-editor)    ; Basic editor enhancements
(require 'init-project)   ; Project management with projectile
(require 'init-vcs)       ; Version control (diff-hl)
(require 'init-search)    ; Search tools
(require 'init-dired)     ; Dired configuration
(require 'init-treemacs)  ; Treemacs file browser
(require 'init-markdown)  ; Markdown support
(require 'init-utils)     ; Utility functions
(require 'init-keybindings); Global keybindings
(require 'init-compile)   ; Byte compilation utilities
(require 'init-qol)       ; Quality of life improvements
(require 'init-treesitter); Tree-sitter support for Emacs 29+
(require 'init-eglot)     ; Built-in LSP client
(require 'init-eslint-fix); Fix ESLint configuration issues
(require 'init-terminal)  ; Terminal emulator configuration

;;; Load optional configurations

;; Development configuration - Modern version with Eglot
(let ((dev-config-modern (expand-file-name "emacs-dev-config-modern.el" user-emacs-directory)))
  (when (file-exists-p dev-config-modern)
    (load-file dev-config-modern)
    (add-hook 'emacs-startup-hook
              (lambda ()
                (run-with-timer 1 nil
                                (lambda ()
                                  (message "Modern development mode available. Use M-x enable-dev-mode-modern to activate.")))))))

;; Legacy development configuration (lsp-mode) - kept for compatibility
(let ((dev-config (expand-file-name "emacs-dev-config.el" user-emacs-directory)))
  (when (file-exists-p dev-config)
    (load-file dev-config)))

;; SHR Configuration (for HTML rendering in mu4e, elfeed, eww)
(let ((shr-config (expand-file-name "shr-config.el" user-emacs-directory)))
  (when (file-exists-p shr-config)
    (load-file shr-config)
    (message "SHR configuration loaded.")))

;; RSS Reader Configuration (Elfeed)
(let ((elfeed-config (expand-file-name "elfeed-config.el" user-emacs-directory)))
  (when (file-exists-p elfeed-config)
    (load-file elfeed-config)
    (message "Elfeed RSS reader configuration loaded.")))

;; Email Configuration (mu4e)
(let ((mu4e-config (expand-file-name "mu4e-config.el" user-emacs-directory)))
  (when (file-exists-p mu4e-config)
    (condition-case err
        (progn
          (load-file mu4e-config)
          (message "mu4e email configuration loaded."))
      (error
       (message "mu4e configuration available but mu4e not installed. Install mu4e package to enable email.")))))

;; Beancount Configuration
(let ((beancount-config (expand-file-name "beancount-config.el" user-emacs-directory)))
  (when (file-exists-p beancount-config)
    (load-file beancount-config)
    (message "Beancount portfolio tracking configuration loaded.")))

;; Portfolio Tracker Configuration
(with-eval-after-load 'tabulated-list
  (let ((portfolio-tracker (expand-file-name "portfolio-tracker-v2.el" user-emacs-directory)))
    (when (file-exists-p portfolio-tracker)
      (load-file portfolio-tracker)
      (message "Portfolio tracker with live prices loaded."))))

;; Keybinding fixes for special modes
(let ((keybinding-fix (expand-file-name "keybinding-fix.el" user-emacs-directory)))
  (when (file-exists-p keybinding-fix)
    (load-file keybinding-fix)
    ;; Automatically apply fixes for special modes
    (fix-elfeed-keybindings)
    (fix-portfolio-tracker-keybindings)
    (disable-cua-in-special-modes)
    (message "Keybinding fixes loaded and applied.")))

;;; Custom Settings (preserved from original)
;;; These are managed by Emacs Custom system - do not edit manually
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

;; Create custom.el with the existing customizations if it doesn't exist
(unless (file-exists-p custom-file)
  (with-temp-file custom-file
    (insert ";;; custom.el --- Custom variables set by Emacs -*- lexical-binding: t -*-\n")
    (insert ";;; Commentary:\n")
    (insert ";;; This file contains customizations set through the Custom interface.\n")
    (insert ";;; It is automatically loaded by init.el.\n\n")
    (insert ";;; Code:\n\n")
    (insert "(custom-set-variables\n")
    (insert " ;; custom-set-variables was added by Custom.\n")
    (insert " ;; If you edit it by hand, you could mess it up, so be careful.\n")
    (insert " ;; Your init file should contain only one such instance.\n")
    (insert " ;; If there is more than one, they won't work right.\n")
    (insert " '(custom-enabled-themes '(developer-dark))\n")
    (insert " '(custom-safe-themes\n")
    (insert "   '(\"2974c2d5ffede4f111b02701ccdfadfbcde3d158ad9b09dade627b3ce3049ea1\"\n")
    (insert "     \"f8859f15bd0089a85d8d14e21dd774a9a8b391dac076e3ff17a13529b3d16576\"\n")
    (insert "     \"de2f009a49f8eaf2f323519d86016849cd1716d979bc3f7e4afb58899e52ddb7\"\n")
    (insert "     \"9fb69436c074b82a62b78b8d733e6274d0bd16d156f7b094e2afe4345c040c49\"\n")
    (insert "     \"004f174754c688f358fa2afc4f8699b5db647fbfaa0d6b55ff39f63e05bfbbf5\"\n")
    (insert "     \"ca1b398ceb1b61709197478dc7f705b8337a0a9631e399948e643520c5557382\"\n")
    (insert "     \"75eef60308d7328ed14fa27002e85de255c2342e73275173a14ed3aa1643d545\"\n")
    (insert "     \"77f281064ea1c8b14938866e21c4e51e4168e05db98863bd7430f1352cab294a\"\n")
    (insert "     \"242e6f00c98aa43539b41c505ef094d21cbc981203809a82949efaa2bc6cb194\"\n")
    (insert "     \"c9e63647d90e7dc59d52c176fbfd46fd2cfed275cd2bad9b29b6cf620d3389b6\"\n")
    (insert "     \"ad7d874d137291e09fe2963babc33d381d087fa14928cb9d34350b67b6556b6d\"\n")
    (insert "     default))\n")
    (insert " '(diff-hl-global-modes t)\n")
    (insert " '(neo-show-hidden-files t)\n")
    (insert " '(neo-window-width 40)\n")
    (insert " '(url-proxy-services\n")
    (insert "   '((\"https\" . \"eudewerepo001:3128\") (\"http\" . \"eudewerepo001:3128\")))\n")
    (insert " '(safe-local-variable-values\n")
    (insert "   '((company-backends\n")
    (insert "      (company-qml company-capf company-files company-yasnippet))\n")
    (insert "     (lsp-clients-qml-server-executable . \"/usr/lib/qt6/bin/qmlls\")\n")
    (insert "     (company-minimum-prefix-length . 1) (company-idle-delay . 0.2)\n")
    (insert "     (lsp-clangd-binary-path . \"clangd\")\n")
    (insert "     (lsp-clients-clangd-args\n")
    (insert "      \"--compile-commands-dir=/home/jens/sources/thulio\"\n")
    (insert "      \"--background-index\" \"--clang-tidy\"\n")
    (insert "      \"--completion-style=detailed\" \"--header-insertion=iwyu\"\n")
    (insert "      \"--pch-storage=memory\")\n")
    (insert "     (projectile-project-root . \"/home/jens/sources/thulio\"))))\n\n")
    (insert "(custom-set-faces\n")
    (insert " ;; custom-set-faces was added by Custom.\n")
    (insert " ;; If you edit it by hand, you could mess it up, so be careful.\n")
    (insert " ;; Your init file should contain only one such instance.\n")
    (insert " ;; If there is more than one, they won't work right.\n")
    (insert " )\n\n")
    (insert "(provide 'custom)\n")
    (insert ";;; custom.el ends here\n")))

;; Load custom file
(when (file-exists-p custom-file)
  (load custom-file))

;; Optional: Set default portfolio file to load on startup
(defvar portfolio-tracker-default-file
  (expand-file-name "sample-portfolio-v2.el" user-emacs-directory)
  "Default portfolio file to load.")

(provide 'init)
;;; init.el ends here
