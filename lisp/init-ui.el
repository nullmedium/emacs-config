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

;; CUA mode for rectangles - use selection mode only to avoid conflicts
(cua-selection-mode t)  ; Only rectangle selection, not full CUA bindings
(setq cua-auto-tabify-rectangles nil)
(setq cua-keep-region-after-copy t)

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
                    :height 140
                    :width 'normal)

;; Ensure font settings apply to new frames
(add-to-list 'default-frame-alist '(font . "0xProto Nerd Font Mono-14"))

;;; Diff-hl face customizations
(with-eval-after-load 'diff-hl
  (set-face-attribute 'diff-hl-change nil :background "blue3" :foreground "blue3")
  (set-face-attribute 'diff-hl-delete nil :background "red3" :foreground "red3")
  (set-face-attribute 'diff-hl-insert nil :background "green3" :foreground "green3"))

;;; Theme Management
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