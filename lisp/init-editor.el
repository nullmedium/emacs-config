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

;; Word selection with C-Shift-left/right
(global-set-key (kbd "C-S-<left>") 'left-word)
(global-set-key (kbd "C-S-<right>") 'right-word)

;; Make sure shift-selection works with these commands
(put 'left-word 'CUA 'move)
(put 'right-word 'CUA 'move)

;;; Text manipulation
(global-set-key (kbd "C-<return>") 'cua-set-rectangle-mark)

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