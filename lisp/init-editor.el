;;; init-editor.el --- Editor enhancements and editing settings -*- lexical-binding: t -*-
;;; Commentary:
;;; Basic editor settings and enhancements for a better editing experience

;;; Code:

;;; Basic editing settings
(global-auto-revert-mode t)
;; Electric-pair-mode is replaced by smartparens in init-qol.el

;; Enable shift-select mode for selecting text with Shift+Arrow keys
(setq shift-select-mode t)
(transient-mark-mode t)

;; Standard word navigation with C-left/right
(global-set-key (kbd "C-<left>") 'left-word)
(global-set-key (kbd "C-<right>") 'right-word)

;; Custom functions for shift-selection with word movement
(defun left-word-select ()
  "Move left by words, extending selection."
  (interactive "^")
  (left-word))

(defun right-word-select ()
  "Move right by words, extending selection."
  (interactive "^")
  (right-word))

;; Word selection with C-Shift-left/right
(global-set-key (kbd "C-S-<left>") 'left-word-select)
(global-set-key (kbd "C-S-<right>") 'right-word-select)

;; Mark these functions as shift-selectable
(put 'left-word 'shift-selection-mode t)
(put 'right-word 'shift-selection-mode t)
(put 'left-word-select 'shift-selection-mode t)
(put 'right-word-select 'shift-selection-mode t)

;; Additional selection keybindings for consistency
;; Shift+Home/End to select to beginning/end of line
(global-set-key (kbd "S-<home>") 'beginning-of-line-select)
(global-set-key (kbd "S-<end>") 'end-of-line-select)

(defun beginning-of-line-select ()
  "Move to beginning of line, extending selection."
  (interactive "^")
  (beginning-of-line))

(defun end-of-line-select ()
  "Move to end of line, extending selection."
  (interactive "^")
  (end-of-line))

;; Ctrl+Shift+Home/End to select to beginning/end of buffer
(global-set-key (kbd "C-S-<home>") 'beginning-of-buffer-select)
(global-set-key (kbd "C-S-<end>") 'end-of-buffer-select)

(defun beginning-of-buffer-select ()
  "Move to beginning of buffer, extending selection."
  (interactive "^")
  (beginning-of-buffer))

(defun end-of-buffer-select ()
  "Move to end of buffer, extending selection."
  (interactive "^")
  (end-of-buffer))

;; Ensure shift-arrow keys work for character selection
(global-set-key (kbd "S-<left>") 'left-char-select)
(global-set-key (kbd "S-<right>") 'right-char-select)
(global-set-key (kbd "S-<up>") 'previous-line-select)
(global-set-key (kbd "S-<down>") 'next-line-select)

(defun left-char-select ()
  "Move left by character, extending selection."
  (interactive "^")
  (left-char))

(defun right-char-select ()
  "Move right by character, extending selection."
  (interactive "^")
  (right-char))

(defun previous-line-select ()
  "Move up by line, extending selection."
  (interactive "^")
  (previous-line))

(defun next-line-select ()
  "Move down by line, extending selection."
  (interactive "^")
  (next-line))

;;; Text manipulation
(global-set-key (kbd "C-<return>") 'cua-set-rectangle-mark)

;; Diagnostic function for selection keybindings
(defun diagnose-selection-keys ()
  "Check if selection keybindings are properly configured."
  (interactive)
  (with-output-to-temp-buffer "*Selection Keys Diagnostics*"
    (princ "=== SELECTION KEYBINDINGS DIAGNOSTICS ===\n\n")
    (princ (format "Shift-select mode: %s\n" (if shift-select-mode "ENABLED" "DISABLED")))
    (princ (format "Transient mark mode: %s\n\n" (if transient-mark-mode "ENABLED" "DISABLED")))
    (princ "Word selection keys:\n")
    (princ (format "  C-S-<left>:  %s\n" (key-binding (kbd "C-S-<left>"))))
    (princ (format "  C-S-<right>: %s\n\n" (key-binding (kbd "C-S-<right>"))))
    (princ "Character selection keys:\n")
    (princ (format "  S-<left>:  %s\n" (key-binding (kbd "S-<left>"))))
    (princ (format "  S-<right>: %s\n" (key-binding (kbd "S-<right>"))))
    (princ (format "  S-<up>:    %s\n" (key-binding (kbd "S-<up>"))))
    (princ (format "  S-<down>:  %s\n\n" (key-binding (kbd "S-<down>"))))
    (princ "Line selection keys:\n")
    (princ (format "  S-<home>: %s\n" (key-binding (kbd "S-<home>"))))
    (princ (format "  S-<end>:  %s\n\n" (key-binding (kbd "S-<end>"))))
    (princ "If keys are not bound correctly, reload with:\n")
    (princ "  M-x reload-emacs-config\n")))

;;; Anzu - show match information in mode line
(use-package anzu
  :ensure t
  :defer t
  :config
  (global-anzu-mode 1)
  :bind (([remap query-replace] . anzu-query-replace)
         ([remap query-replace-regexp] . anzu-query-replace-regexp)))

;;; Avy - jump to visible text
(use-package avy
  :ensure t
  :defer t
  :bind (("C-:" . avy-goto-char)
         ("C-'" . avy-goto-char-2)
         ("M-g f" . avy-goto-line)
         ("M-g w" . avy-goto-word-1)))

;;; Olivetti - distraction-free writing
(use-package olivetti
  :ensure t
  :defer t
  :commands olivetti-mode
  :config
  (setq olivetti-body-width 100))

;;; Rainbow delimiters - colorize matching brackets
(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

;;; God-mode configuration (disabled by default)
;; Uncomment the following lines to enable god-mode
;; (let ((god-config (expand-file-name "god-mode-config.el" user-emacs-directory)))
;;   (when (file-exists-p god-config)
;;     (condition-case err
;;         (load-file god-config)
;;       (error (message "Failed to load god-mode config: %s" err)))))

(provide 'init-editor)
;;; init-editor.el ends here