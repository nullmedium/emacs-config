;;; init-utils.el --- Utility functions and configurations -*- lexical-binding: t -*-
;;; Commentary:
;;; Custom utility functions and helper configurations

;;; Code:

;;; Custom Functions
(defun kill-current-buffer-no-confirm ()
  "Kill the current buffer without confirmation, unless it has unsaved changes."
  (interactive)
  (kill-buffer (current-buffer)))

;; Keep original function but rename it
(defun reload-emacs-config-blocking ()
  "Reload the Emacs configuration file and all dependent configs (blocking version)."
  (interactive)
  ;; First reload the main init.el
  (load-file (expand-file-name "init.el" user-emacs-directory))

  ;; Reload development config if it exists
  (let ((dev-config (expand-file-name "lisp/emacs-dev-config.el" user-emacs-directory)))
    (when (file-exists-p dev-config)
      (load-file dev-config)))

  ;; Reload SHR config if it exists
  (let ((shr-config (expand-file-name "lisp/shr-config.el" user-emacs-directory)))
    (when (file-exists-p shr-config)
      (load-file shr-config)))

  ;; Reload elfeed config if it exists
  (let ((elfeed-config (expand-file-name "lisp/elfeed-config.el" user-emacs-directory)))
    (when (file-exists-p elfeed-config)
      (load-file elfeed-config)))

  ;; Reload mu4e config if it exists
  (let ((mu4e-config (expand-file-name "lisp/mu4e-config.el" user-emacs-directory)))
    (when (file-exists-p mu4e-config)
      (condition-case err
          (load-file mu4e-config)
        (error
         (message "mu4e config available but mu4e not installed")))))

  (message "Emacs configuration fully reloaded!"))

(defvar reload-emacs-config-timer nil
  "Timer for non-blocking configuration reload.")

(defvar reload-emacs-config-files nil
  "List of files to reload.")

(defvar reload-emacs-config-index 0
  "Current index in the reload process.")

(defun reload-emacs-config-process-next ()
  "Process the next file in the reload queue."
  (when (< reload-emacs-config-index (length reload-emacs-config-files))
    (let ((file (nth reload-emacs-config-index reload-emacs-config-files)))
      (condition-case err
          (progn
            (load-file file)
            (message "[%d/%d] Loaded %s"
                     (1+ reload-emacs-config-index)
                     (length reload-emacs-config-files)
                     (file-name-nondirectory file)))
        (error
         (message "[%d/%d] Error loading %s: %s"
                  (1+ reload-emacs-config-index)
                  (length reload-emacs-config-files)
                  (file-name-nondirectory file)
                  err)))
      (cl-incf reload-emacs-config-index)
      ;; Schedule next file
      (setq reload-emacs-config-timer
            (run-with-idle-timer 0.01 nil #'reload-emacs-config-process-next)))
    ;; All done
    (when (>= reload-emacs-config-index (length reload-emacs-config-files))
      (message "✓ Configuration reload complete!")
      (setq reload-emacs-config-timer nil
            reload-emacs-config-files nil
            reload-emacs-config-index 0))))

(defun reload-emacs-config (&optional arg)
  "Reload Emacs configuration non-blocking with incremental updates.
This version loads configuration files one by one during idle time
to prevent UI freezing. With prefix ARG, use blocking reload."
  (interactive "P")
  ;; If prefix arg given, use blocking reload
  (if arg
      (reload-emacs-config-blocking)
    ;; Otherwise, proceed with non-blocking reload
    ;; Cancel any ongoing reload
  (when reload-emacs-config-timer
    (cancel-timer reload-emacs-config-timer)
    (setq reload-emacs-config-timer nil))
  
  ;; Prepare list of files to reload
  (setq reload-emacs-config-files
        (cl-remove-if-not
         #'file-exists-p
         (list (expand-file-name "init.el" user-emacs-directory)
               (expand-file-name "lisp/emacs-dev-config.el" user-emacs-directory)
               (expand-file-name "lisp/shr-config.el" user-emacs-directory)
               (expand-file-name "lisp/elfeed-config.el" user-emacs-directory)
               (expand-file-name "lisp/mu4e-config.el" user-emacs-directory))))
  
  (setq reload-emacs-config-index 0)
  
  (message "Starting configuration reload (%d files)..."
           (length reload-emacs-config-files))
  
  ;; Start the reload process
  (reload-emacs-config-process-next)))

(defun reload-emacs-config-async ()
  "Reload Emacs configuration asynchronously with progress feedback."
  (interactive)
  (let* ((start-time (current-time))
         (config-files '())
         (total-files 0)
         (loaded-files 0)
         (progress-reporter nil))
    
    ;; Collect all config files to reload
    (setq config-files
          (append
           ;; Main init.el
           (list (expand-file-name "init.el" user-emacs-directory))
           ;; Optional configs
           (cl-remove-if-not
            #'file-exists-p
            (list
             (expand-file-name "lisp/emacs-dev-config.el" user-emacs-directory)
             (expand-file-name "lisp/shr-config.el" user-emacs-directory)
             (expand-file-name "lisp/elfeed-config.el" user-emacs-directory)
             (expand-file-name "lisp/mu4e-config.el" user-emacs-directory)))))
    
    (setq total-files (length config-files))
    (setq progress-reporter
          (make-progress-reporter "Reloading configuration..." 0 total-files))
    
    ;; Load files with progress updates
    (dolist (file config-files)
      (condition-case err
          (progn
            (load-file file)
            (cl-incf loaded-files)
            (progress-reporter-update progress-reporter loaded-files))
        (error
         (message "Error loading %s: %s" (file-name-nondirectory file) err))))
    
    (progress-reporter-done progress-reporter)
    (message "Configuration reloaded in %.2f seconds"
             (float-time (time-subtract (current-time) start-time)))))

(defun reload-emacs-config-smart ()
  "Smart reload that only reloads changed files since last load."
  (interactive)
  (let* ((lisp-dir (expand-file-name "lisp" user-emacs-directory))
         (init-file (expand-file-name "init.el" user-emacs-directory))
         (changed-files '())
         (reload-all nil))
    
    ;; Check if init.el has changed
    (when (file-newer-than-file-p init-file (current-time))
      (setq reload-all t))
    
    (if reload-all
        ;; If init.el changed, do full reload
        (reload-emacs-config-async)
      ;; Otherwise, reload only changed files
      (progn
        ;; Find changed files in lisp directory
        (dolist (file (directory-files lisp-dir t "\\.el$"))
          (when (and (not (string-match-p "\\.elc$" file))
                     (not (string-match-p "#" file))
                     (file-newer-than-file-p file (time-subtract (current-time) 60)))
            (push file changed-files)))
        
        (if changed-files
            (progn
              (message "Reloading %d changed file(s)..." (length changed-files))
              (dolist (file changed-files)
                (condition-case err
                    (progn
                      (load-file file)
                      (message "Reloaded %s" (file-name-nondirectory file)))
                  (error
                   (message "Error reloading %s: %s" 
                            (file-name-nondirectory file) err))))
              (message "Smart reload complete!"))
          (message "No files changed recently. Use C-u C-c C-r for full reload.")))))

(defun reload-current-file ()
  "Reload only the current file if it's an Emacs Lisp file."
  (interactive)
  (when (and (buffer-file-name)
             (string-match-p "\\.el$" (buffer-file-name)))
    (save-buffer)
    (load-file (buffer-file-name))
    (message "Reloaded %s" (file-name-nondirectory (buffer-file-name)))))

(defun reload-emacs-config-fast ()
  "Fast reload using byte-compiled files when available.
This is the fastest reload method but requires byte-compilation."
  (interactive)
  (let* ((init-file (expand-file-name "init" user-emacs-directory))
         (start-time (current-time)))
    ;; Try to load byte-compiled version first, fall back to source
    (if (file-exists-p (concat init-file ".elc"))
        (progn
          (load init-file t t)
          (message "Fast reload complete (byte-compiled) in %.2f seconds"
                   (float-time (time-subtract (current-time) start-time))))
      ;; Fall back to non-blocking reload
      (reload-emacs-config))))

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
  (let ((god-config (expand-file-name "lisp/god-mode-config.el" user-emacs-directory)))
    (if (file-exists-p god-config)
        (condition-case err
            (progn
              (load-file god-config)
              (message "God-mode configuration loaded. Press ESC to toggle god-mode."))
          (error (message "Failed to load god-mode config: %s" err)))
      (message "God-mode config file not found at %s" god-config)))))

(provide 'init-utils)
;;; init-utils.el ends here