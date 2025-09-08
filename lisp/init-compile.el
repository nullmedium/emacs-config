;;; init-compile.el --- Byte compilation utilities -*- lexical-binding: t -*-
;;; Commentary:
;;; Functions to byte-compile configuration files for faster loading

;;; Code:

(defun byte-compile-init-files ()
  "Byte compile all init files in the lisp directory."
  (interactive)
  (let ((lisp-dir (expand-file-name "lisp" user-emacs-directory)))
    (byte-recompile-directory lisp-dir 0 t)
    (message "All init files byte-compiled")))

(defun byte-compile-config ()
  "Byte compile the entire Emacs configuration."
  (interactive)
  (byte-compile-file (expand-file-name "init.el" user-emacs-directory))
  (byte-compile-file (expand-file-name "early-init.el" user-emacs-directory))
  (byte-compile-init-files)
  (message "Configuration byte-compiled successfully"))

(defun clean-byte-compiled-files ()
  "Remove all byte-compiled files from configuration."
  (interactive)
  (let ((lisp-dir (expand-file-name "lisp" user-emacs-directory)))
    ;; Remove .elc files from lisp directory
    (dolist (file (directory-files lisp-dir t "\\.elc$"))
      (delete-file file))
    ;; Remove main init.elc and early-init.elc
    (let ((init-elc (expand-file-name "init.elc" user-emacs-directory))
          (early-elc (expand-file-name "early-init.elc" user-emacs-directory)))
      (when (file-exists-p init-elc) (delete-file init-elc))
      (when (file-exists-p early-elc) (delete-file early-elc)))
    (message "All byte-compiled files removed")))

;; Auto-compile on save if enabled
(defvar auto-compile-on-save nil
  "If non-nil, automatically byte-compile init files when saved.")

(defun maybe-byte-compile-file ()
  "Byte compile the current file if it's an init file and auto-compile is enabled."
  (when (and auto-compile-on-save
             (string-match-p "\\(init\\|early-init\\).*\\.el$" buffer-file-name)
             (not (string-match-p "\\.elc$" buffer-file-name)))
    (byte-compile-file buffer-file-name)))

(add-hook 'after-save-hook #'maybe-byte-compile-file)

;; Byte compile configuration after packages are installed
(defun byte-compile-after-package-install ()
  "Byte compile configuration after new packages are installed."
  (when (and (boundp 'package-alist)
             (> (length package-alist) 0))
    (byte-compile-config)))

;; Add startup message about byte compilation
(add-hook 'emacs-startup-hook
          (lambda ()
            (let ((init-elc (expand-file-name "init.elc" user-emacs-directory)))
              (if (file-exists-p init-elc)
                  (message "Running byte-compiled configuration")
                (message "Running interpreted configuration (use M-x byte-compile-config for faster startup)"))))
          90)

(provide 'init-compile)
;;; init-compile.el ends here