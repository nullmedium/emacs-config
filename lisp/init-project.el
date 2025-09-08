;;; init-project.el --- Project management configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; Projectile and project management settings

;;; Code:

;;; Projectile - Project Management
(use-package projectile
  :ensure t
  :init
  (projectile-mode +1)
  :bind-keymap ("C-c p" . projectile-command-map)
  :bind (("C-c d" . dired-jump)
         ("C-c D" . projectile-dired))
  :config
  (setq projectile-completion-system 'default)  ; Use default completion (works with Vertico)
  (setq projectile-switch-project-action #'projectile-dired)
  (setq projectile-enable-caching t))

(provide 'init-project)
;;; init-project.el ends here