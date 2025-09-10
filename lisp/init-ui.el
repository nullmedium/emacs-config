;;; init-ui.el --- UI and theme configurations -*- lexical-binding: t -*-
;;; Commentary:
;;; User interface settings, themes, and visual configurations

;;; Code:

;;; Display Settings
(column-number-mode t)
(global-display-line-numbers-mode t)
(setq display-line-numbers-type 'relative)
(tool-bar-mode -1)
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))
(global-hl-line-mode 1)
(show-paren-mode 1)
(setq show-paren-delay 0)

;; Enable syntax highlighting globally
(global-font-lock-mode t)
(setq font-lock-maximum-decoration t)
(setq font-lock-support-mode 'jit-lock-mode)
(setq jit-lock-contextually t)
(setq jit-lock-stealth-time 5)

;;; CUA Mode Configuration
;; Enable full CUA mode for standard copy/paste/cut
;; This provides C-c (copy), C-v (paste), C-x (cut), C-z (undo)
(setq cua-enable-cua-keys t)
(setq cua-auto-tabify-rectangles nil)
(setq cua-keep-region-after-copy t)

;; CRITICAL: Set a very short delay to allow C-c to work as prefix when needed
;; This allows C-c C-something to work while C-c alone copies
(setq cua-prefix-override-inhibit-delay 0.001)

;; Enable CUA mode
(cua-mode 1)

;; Function to disable CUA in modes where it conflicts
(defun disable-cua-in-conflicting-modes ()
  "Disable CUA mode in modes where it causes conflicts."
  (dolist (hook '(elfeed-search-mode-hook
                  elfeed-show-mode-hook
                  magit-mode-hook
                  dired-mode-hook
                  help-mode-hook
                  compilation-mode-hook
                  special-mode-hook))
    (add-hook hook
              (lambda ()
                (setq-local cua-mode nil)
                (setq-local cua-enable-cua-keys nil)))))

;; Apply mode-specific fixes
(disable-cua-in-conflicting-modes)

;; Ensure CUA works properly after init
(defun ensure-cua-after-init ()
  "Ensure CUA mode is properly configured after initialization."
  (when (not cua-mode)
    (cua-mode 1))
  ;; Ensure the keybindings are active
  (setq cua-enable-cua-keys t))

(add-hook 'after-init-hook 'ensure-cua-after-init)

;; Function to fix syntax highlighting in current buffer
(defun fix-syntax-highlighting ()
  "Fix syntax highlighting in the current buffer."
  (interactive)
  (font-lock-mode -1)
  (font-lock-mode 1)
  (font-lock-fontify-buffer)
  (message "Syntax highlighting refreshed"))

;; Function to switch between tree-sitter and regular modes
(defun toggle-tree-sitter-mode ()
  "Toggle between tree-sitter and regular mode for current buffer."
  (interactive)
  (cond
   ((eq major-mode 'c-ts-mode)
    (c-mode)
    (message "Switched to regular c-mode"))
   ((eq major-mode 'c++-ts-mode)
    (c++-mode)
    (message "Switched to regular c++-mode"))
   ((eq major-mode 'c-mode)
    (if (fboundp 'c-ts-mode)
        (progn (c-ts-mode)
               (message "Switched to c-ts-mode"))
      (message "Tree-sitter mode not available")))
   ((eq major-mode 'c++-mode)
    (if (fboundp 'c++-ts-mode)
        (progn (c++-ts-mode)
               (message "Switched to c++-ts-mode"))
      (message "Tree-sitter mode not available")))
   (t (message "Not in a C/C++ buffer"))))

;; Function to diagnose syntax highlighting issues
(defun diagnose-syntax-highlighting ()
  "Diagnose syntax highlighting issues in current buffer."
  (interactive)
  (with-output-to-temp-buffer "*Syntax Highlighting Diagnostics*"
    (princ "=== SYNTAX HIGHLIGHTING DIAGNOSTICS ===\n\n")
    (princ (format "Buffer: %s\n" (buffer-name)))
    (princ (format "Major mode: %s\n" major-mode))
    (princ (format "Font-lock mode: %s\n" (if font-lock-mode "ENABLED" "DISABLED")))
    (princ (format "Global font-lock mode: %s\n" (if global-font-lock-mode "ENABLED" "DISABLED")))
    (princ (format "Font-lock keywords defined: %s\n" 
                   (if font-lock-keywords "YES" "NO")))
    (princ (format "Buffer size: %d bytes\n" (buffer-size)))
    (princ (format "File size threshold check: %s\n"
                   (if (> (buffer-size) (* 1024 1024)) 
                       "LARGE FILE (>1MB) - highlighting may be disabled"
                       "Normal size")))
    (princ "\nTo fix issues, try:\n")
    (princ "  M-x fix-syntax-highlighting\n")
    (princ "  M-x font-lock-mode (toggle off and on)\n")
    (princ "  M-x font-lock-fontify-buffer\n")))

;; Ensure font-lock works in C/C++ modes (both regular and tree-sitter)
(defun ensure-c-syntax-highlighting ()
  "Ensure syntax highlighting works in C/C++ modes."
  (font-lock-mode 1)
  (setq font-lock-keywords-case-fold-search nil)
  ;; Force fontification if needed
  (when (and (boundp 'font-lock-mode) (not font-lock-mode))
    (font-lock-mode 1))
  ;; For tree-sitter modes, ensure proper setup
  (when (or (eq major-mode 'c-ts-mode) 
            (eq major-mode 'c++-ts-mode))
    (when (fboundp 'treesit-font-lock-recompute-features)
      (treesit-font-lock-recompute-features))))

;; Apply to all C/C++ mode variants
(add-hook 'c-mode-hook 'ensure-c-syntax-highlighting)
(add-hook 'c++-mode-hook 'ensure-c-syntax-highlighting)
(add-hook 'c-ts-mode-hook 'ensure-c-syntax-highlighting)
(add-hook 'c++-ts-mode-hook 'ensure-c-syntax-highlighting)

;; Diagnostic function for CUA mode
(defun diagnose-cua-mode ()
  "Diagnose CUA mode settings and keybindings."
  (interactive)
  (with-output-to-temp-buffer "*CUA Mode Diagnostics*"
    (princ "=== CUA MODE DIAGNOSTICS ===\n\n")
    (princ (format "CUA mode enabled: %s\n" (if cua-mode "YES" "NO")))
    (princ (format "CUA keys enabled: %s\n" (if cua-enable-cua-keys "YES" "NO")))
    (princ (format "CUA prefix override delay: %s\n" cua-prefix-override-inhibit-delay))
    (princ "\nKey bindings:\n")
    (princ (format "C-c binding: %s\n" (key-binding (kbd "C-c"))))
    (princ (format "C-v binding: %s\n" (key-binding (kbd "C-v"))))
    (princ (format "C-x binding: %s\n" (key-binding (kbd "C-x"))))
    (princ (format "C-z binding: %s\n" (key-binding (kbd "C-z"))))
    (princ "\nTo fix issues, try:\n")
    (princ "  M-x ensure-cua-bindings\n")
    (princ "  M-x cua-mode (toggle off and on)\n")))

;; Trailing whitespace
(setq show-trailing-whitespace t)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Fill column indicator
(setq-default display-fill-column-indicator-column 80)
(setq-default display-fill-column-indicator-character ?\u2502)
(global-display-fill-column-indicator-mode 1)
(set-face-attribute 'fill-column-indicator nil :foreground "red")

;; Window dividers for mouse resizing
(setq mouse-autoselect-window nil)
(setq window-divider-default-places t)
(setq window-divider-default-bottom-width 1)
(setq window-divider-default-right-width 1)
(window-divider-mode 1)

;;; X11 optimizations for no-toolkit builds
(when (and (display-graphic-p)
           (eq window-system 'x)
           ;; Check if using no-toolkit build (OLDXMENU present)
           (string-match-p "OLDXMENU" system-configuration-features))
  ;; Disable double buffering to fix redraw issues
  (add-to-list 'default-frame-alist '(inhibit-double-buffering . t))
  ;; Force synchronous X operations
  (setq x-gtk-use-system-tooltips nil)
  ;; More responsive display updates
  (setq redisplay-dont-pause t))

;;; Font Settings (preserved from custom-set-faces)
;; Apply font settings to all frames (current and future)
(set-face-attribute 'default nil
                    :family "0xProto Nerd Font Mono"
                    :foundry "nil"
                    :slant 'normal
                    :weight 'regular
                    :height 180
                    :width 'normal)

;; Ensure font settings apply to new frames
;; Use the proper font spec format
(add-to-list 'default-frame-alist 
             (cons 'font (font-spec :family "0xProto Nerd Font Mono" :size 18)))

;;; Diff-hl face customizations
(with-eval-after-load 'diff-hl
  (set-face-attribute 'diff-hl-change nil :background "blue3" :foreground "blue3")
  (set-face-attribute 'diff-hl-delete nil :background "red3" :foreground "red3")
  (set-face-attribute 'diff-hl-insert nil :background "green3" :foreground "green3"))

;;; Theme Management
;; Add lisp directory to theme load path
(add-to-list 'custom-theme-load-path
             (expand-file-name "lisp" user-emacs-directory))

(defvar jens-themes
  '(developer-dark
    modus-vivendi
    modus-operandi)
  "List of preferred themes.")

(defun load-jens-dark-theme ()
  "Load the custom jens-dark theme."
  (interactive)
  (load-theme 'jens-dark t)
  (message "Jens Dark theme loaded"))

(defun switch-theme (theme)
  "Switch to a different theme interactively."
  (interactive
   (list (intern (completing-read "Load theme: "
                                  (mapcar #'symbol-name (custom-available-themes))))))
  (mapc #'disable-theme custom-enabled-themes)
  (load-theme theme t)
  (message "Switched to %s theme" theme))

;; Load default theme
(when (member 'developer-dark (custom-available-themes))
  (load-theme 'developer-dark t))

;;; Icons
(use-package all-the-icons
  :ensure t
  :defer t)

(use-package all-the-icons-dired
  :ensure t
  :after all-the-icons
  :hook (dired-mode . all-the-icons-dired-mode))

;;; Which-key for discovering keybindings
(use-package which-key
  :ensure t
  :init (which-key-mode)
  :config
  (setq which-key-idle-delay 0.3)
  (setq which-key-popup-type 'side-window))

(provide 'init-ui)
;;; init-ui.el ends here
