;;; init-utils.el --- Utility functions and configurations -*- lexical-binding: t -*-
;;; Commentary:
;;; Custom utility functions and helper configurations

;;; Code:

;;; Custom Functions
(defun kill-current-buffer-no-confirm ()
  "Kill the current buffer without confirmation, unless it has unsaved changes."
  (interactive)
  (kill-buffer (current-buffer)))

(defun reload-emacs-config ()
  "Reload the Emacs configuration file and all dependent configs."
  (interactive)
  ;; First reload the main init.el
  (load-file (expand-file-name "init.el" user-emacs-directory))

  ;; Reload development config if it exists
  (let ((dev-config (expand-file-name "emacs-dev-config.el" user-emacs-directory)))
    (when (file-exists-p dev-config)
      (load-file dev-config)))

  ;; Reload SHR config if it exists
  (let ((shr-config (expand-file-name "shr-config.el" user-emacs-directory)))
    (when (file-exists-p shr-config)
      (load-file shr-config)))

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

;;; Package management helpers
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

;;; Proxy management
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

;;; Development mode helpers
(defun show-dev-mode-info ()
  "Show information about development mode."
  (interactive)
  (message "Development mode is available. Use M-x enable-dev-mode to activate LSP, company-mode, flycheck, and other development tools."))

;;; God-mode helpers
(defun enable-god-mode-config ()
  "Enable god-mode configuration."
  (interactive)
  (let ((god-config (expand-file-name "god-mode-config.el" user-emacs-directory)))
    (if (file-exists-p god-config)
        (condition-case err
            (progn
              (load-file god-config)
              (message "God-mode configuration loaded. Press ESC to toggle god-mode."))
          (error (message "Failed to load god-mode config: %s" err)))
      (message "God-mode config file not found at %s" god-config))))

(provide 'init-utils)
;;; init-utils.el ends here