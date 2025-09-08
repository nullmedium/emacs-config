;;; init-keybindings.el --- Global keybindings -*- lexical-binding: t -*-
;;; Commentary:
;;; Global keybinding configurations

;;; Code:

;;; Buffer management
(global-set-key (kbd "C-x k") 'kill-current-buffer-no-confirm)

;;; Configuration reload
(global-set-key (kbd "C-c C-r") 'reload-emacs-config)

;;; Portfolio tracker
(global-set-key (kbd "C-c $") 'portfolio-tracker)

;;; Window management
;; Consider adding ace-window or windmove keybindings here
;; (global-set-key (kbd "M-o") 'ace-window)
;; (windmove-default-keybindings)

;;; Better defaults
(global-set-key (kbd "C-x C-r") 'recentf-open-files)
(global-set-key (kbd "M-/") 'hippie-expand)

;;; Text manipulation helpers
(defun duplicate-current-line-or-region (arg)
  "Duplicate current line, or region if active.
With argument ARG, make ARG copies."
  (interactive "p")
  (let (beg end (origin (point)))
    (if (and mark-active (> (point) (mark)))
        (exchange-point-and-mark))
    (setq beg (line-beginning-position))
    (if mark-active
        (exchange-point-and-mark))
    (setq end (line-end-position))
    (let ((region (buffer-substring-no-properties beg end)))
      (dotimes (i arg)
        (goto-char end)
        (newline)
        (insert region)
        (setq end (point)))
      (goto-char (+ origin (* (length region) arg) arg)))))

(global-set-key (kbd "C-c C-d") 'duplicate-current-line-or-region)

;;; Quick buffer switching
(defun switch-to-previous-buffer ()
  "Switch to the most recently used buffer."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))

(global-set-key (kbd "C-c b") 'switch-to-previous-buffer)

(provide 'init-keybindings)
;;; init-keybindings.el ends here