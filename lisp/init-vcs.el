;;; init-vcs.el --- Version control configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; Configuration for version control, primarily diff-hl and Git support

;;; Code:

;;; Diff-hl Configuration
(use-package diff-hl
  :ensure t
  :hook ((prog-mode . diff-hl-mode)
         (dired-mode . diff-hl-dired-mode)
         (text-mode . diff-hl-mode)
         (conf-mode . diff-hl-mode))
  :bind (("M-n" . diff-hl-next-hunk)
         ("M-p" . diff-hl-previous-hunk)
         ("C-c v r" . diff-hl-revert-hunk)
         ("C-c v s" . diff-hl-diff-goto-hunk)
         ("C-c v u" . diff-hl-update))
  :init
  ;; Set fringe width before diff-hl loads
  (setq-default left-fringe-width 8)
  (setq-default right-fringe-width 8)
  ;; Ensure fringes are visible
  (fringe-mode 8)
  :config
  ;; Configure VC backend for Git
  (setq vc-handled-backends '(Git))
  (setq vc-git-diff-switches '("--histogram"))

  ;; Tell diff-hl to use VC backend
  (setq diff-hl-reference-revision nil)
  (setq diff-hl-disable-on-remote nil)

  ;; Make diff-hl use the left fringe
  (setq diff-hl-side 'left)

  ;; Ensure diff-hl draws in fringe, not margin
  (setq diff-hl-draw-borders nil)
  (setq diff-hl-margin-mode nil)

  ;; Set diff-hl fringe bitmaps
  (setq diff-hl-fringe-bmp-function 'diff-hl-fringe-bmp-from-type)

  ;; Enable flydiff for real-time updates
  (diff-hl-flydiff-mode 1)

  ;; Update immediately when visiting a file
  (setq diff-hl-flydiff-delay 0.3)

  ;; Make sure diff-hl updates on various events
  (add-hook 'after-save-hook 'diff-hl-update)
  (add-hook 'after-revert-hook 'diff-hl-update)
  (add-hook 'find-file-hook 'diff-hl-update)
  (add-hook 'vc-checkin-hook 'diff-hl-update)

  ;; Enable globally with a slight delay to speed up initial startup
  (run-with-idle-timer 2 nil #'global-diff-hl-mode))

;; Manual refresh command
(defun diff-hl-refresh ()
  "Manually refresh diff-hl indicators in all buffers."
  (interactive)
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when diff-hl-mode
        (diff-hl-update)))))

;; Force VC to refresh its state
(defun diff-hl-force-vc-refresh ()
  "Force VC to refresh state and then update diff-hl."
  (interactive)
  (when buffer-file-name
    (vc-refresh-state)
    (diff-hl-update)
    (message "VC state refreshed and diff-hl updated")))

(provide 'init-vcs)
;;; init-vcs.el ends here