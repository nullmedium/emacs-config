;;; init-emergency-fix.el --- Emergency fixes for editing issues -*- lexical-binding: t -*-
;;; Commentary:
;;; Quick fixes for when editing is broken

;;; Code:

;; Emergency function to restore editing capability
(defun fix-editing-now ()
  "Emergency fix to restore editing capability."
  (interactive)
  ;; Disable read-only mode
  (read-only-mode -1)
  (setq buffer-read-only nil)
  
  ;; Disable god-mode if it's enabled
  (when (boundp 'god-local-mode)
    (god-local-mode -1))
  (when (boundp 'god-global-mode)
    (god-global-mode -1))
  
  ;; Disable view-mode if active
  (when (bound-and-true-p view-mode)
    (view-mode -1))
  
  ;; Ensure we're not in special modes
  (when (bound-and-true-p special-mode)
    (fundamental-mode))
  
  ;; Disable any potential read-only causing modes
  (when (bound-and-true-p buffer-read-only)
    (toggle-read-only -1))
  
  ;; Reset cursor type
  (setq cursor-type 'bar)
  
  ;; Ensure CUA mode isn't causing issues
  (when cua-mode
    (cua-mode -1)
    (cua-mode 1))
  
  (message "Editing should be restored. If not, try M-x fundamental-mode"))

;; Diagnostic function
(defun diagnose-editing-issue ()
  "Diagnose why editing might be disabled."
  (interactive)
  (let ((issues '()))
    (when buffer-read-only
      (push "Buffer is read-only" issues))
    (when (bound-and-true-p god-local-mode)
      (push "God-mode is enabled" issues))
    (when (bound-and-true-p view-mode)
      (push "View-mode is enabled" issues))
    (when (bound-and-true-p special-mode)
      (push "Special-mode is active" issues))
    (when (eq major-mode 'fundamental-mode)
      (push "In fundamental-mode" issues))
    (if issues
        (message "Issues found: %s" (string-join issues ", "))
      (message "No obvious issues found. Mode: %s, Read-only: %s" 
               major-mode buffer-read-only))))

;; Global keybinding for emergency fix
(global-set-key (kbd "C-c C-!") 'fix-editing-now)
(global-set-key (kbd "C-c C-?") 'diagnose-editing-issue)

;; Ensure fundamental settings are correct
(setq-default buffer-read-only nil)
(setq inhibit-read-only nil)

(provide 'init-emergency-fix)
;;; init-emergency-fix.el ends here