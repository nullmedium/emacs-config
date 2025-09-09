;;; init-magit.el --- Magit configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; Git interface configuration with Magit

;;; Code:

(use-package magit
  :ensure t
  :defer t
  :bind (("C-x g" . magit-status)
         ("C-x M-g" . magit-dispatch)
         ("C-c g" . magit-file-dispatch))
  :config
  (setq magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)
  
  ;; Performance improvements
  (setq magit-refresh-status-buffer nil)
  (setq magit-diff-highlight-indentation nil)
  (setq magit-diff-highlight-trailing nil)
  
  ;; Custom function to save commits as patches
  (defun magit-save-commit-as-patch ()
    "Save the commit at point as a patch file."
    (interactive)
    (let* ((commit (or (magit-commit-at-point)
                       (error "No commit at point")))
           (default-name (format "%s.patch"
                                (substring commit 0 (min 8 (length commit)))))
           (file (read-file-name "Save patch to file: "
                                default-directory
                                default-name
                                nil
                                default-name))
           (default-directory (magit-toplevel)))
      (if (zerop (shell-command
                  (format "git format-patch -1 %s --stdout > %s"
                          (shell-quote-argument commit)
                          (shell-quote-argument (expand-file-name file)))))
          (message "Patch saved to %s" file)
        (error "Failed to save patch"))))
  
  ;; Bind C-c C-p in all relevant Magit modes
  (define-key magit-revision-mode-map (kbd "C-c C-p") 'magit-save-commit-as-patch)
  (define-key magit-log-mode-map (kbd "C-c C-p") 'magit-save-commit-as-patch)
  (define-key magit-log-select-mode-map (kbd "C-c C-p") 'magit-save-commit-as-patch)
  
  ;; Also add to cherry mode and refs mode
  (with-eval-after-load 'magit-refs
    (define-key magit-refs-mode-map (kbd "C-c C-p") 'magit-save-commit-as-patch))
  (with-eval-after-load 'magit-cherry
    (define-key magit-cherry-mode-map (kbd "C-c C-p") 'magit-save-commit-as-patch)))

;; Ensure keybindings persist through hooks
(add-hook 'magit-revision-mode-hook
          (lambda () (local-set-key (kbd "C-c C-p") 'magit-save-commit-as-patch)))
(add-hook 'magit-log-mode-hook
          (lambda () (local-set-key (kbd "C-c C-p") 'magit-save-commit-as-patch)))
(add-hook 'magit-log-select-mode-hook
          (lambda () (local-set-key (kbd "C-c C-p") 'magit-save-commit-as-patch)))
(add-hook 'magit-refs-mode-hook
          (lambda () (local-set-key (kbd "C-c C-p") 'magit-save-commit-as-patch)))
(add-hook 'magit-cherry-mode-hook
          (lambda () (local-set-key (kbd "C-c C-p") 'magit-save-commit-as-patch)))

;; Optional: Magit-delta for better diffs (if delta is installed)
(when (executable-find "delta")
  (use-package magit-delta
    :ensure t
    :defer t
    :hook (magit-mode . magit-delta-mode)))

;; Integration with diff-hl if available
(with-eval-after-load 'diff-hl
  (add-hook 'magit-pre-refresh-hook 'diff-hl-magit-pre-refresh)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh))

(provide 'init-magit)
;;; init-magit.el ends here