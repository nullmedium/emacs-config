;;; init-org.el --- Org mode configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; This file configures Org mode with custom TODO keywords and highlighting.

;;; Code:

(require 'org)

;; Configure TODO keywords with proper format
(setq org-todo-keywords
      '((sequence "OPEN" "TODO" "INPROGRESS" "POSTPONED" "FEEDBACK" "|" "DONE" "CANCELLED")))

;; Configure TODO keyword faces for highlighting
(setq org-todo-keyword-faces
      '(("OPEN" . (:foreground "cyan" :weight bold))
        ("TODO" . (:foreground "red" :weight bold))
        ("INPROGRESS" . (:foreground "yellow" :weight bold))
        ("POSTPONED" . (:foreground "orange" :weight bold))
        ("FEEDBACK" . (:foreground "magenta" :weight bold))
        ("DONE" . (:foreground "green" :weight bold))
        ("CANCELLED" . (:foreground "gray" :weight bold :strike-through t))))

;; Enable org-kanban if installed
(when (require 'org-kanban nil t)
  ;; Configure org-kanban to use our custom TODO keywords
  ;; The order here determines the column order in the kanban board
  (setq org-kanban-todo-keywords '("OPEN" "TODO"))
  (setq org-kanban-doing-keywords '("INPROGRESS"))
  (setq org-kanban-blocked-keywords '("POSTPONED" "FEEDBACK"))
  (setq org-kanban-done-keywords '("DONE" "CANCELLED"))
  
  ;; Set the column order explicitly
  (setq org-kanban-keyword-order '("OPEN" "TODO" "INPROGRESS" "POSTPONED" "FEEDBACK" "DONE" "CANCELLED"))
  
  ;; Configure the kanban board layout
  (setq org-kanban-abbreviation t)  ; Use abbreviated names in headers
  (setq org-kanban-column-padding 2)
  
  ;; Include subtasks in kanban board
  (setq org-kanban-subtree-toggle t))

;; Set up fast TODO selection
(setq org-use-fast-todo-selection t)

;; Configure TODO dependencies
(setq org-enforce-todo-dependencies t)

;; Log when TODO items are completed
(setq org-log-done 'time)

;; Refresh org-mode files to apply new settings
(defun refresh-org-buffers ()
  "Refresh all org-mode buffers to apply new TODO settings."
  (interactive)
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (eq major-mode 'org-mode)
        (org-mode)
        (message "Refreshed %s" (buffer-name))))))

;; Hook to ensure settings are applied
(add-hook 'org-mode-hook
          (lambda ()
            ;; Force refresh of TODO keywords
            (setq-local org-todo-keywords
                        '((sequence "OPEN" "TODO" "INPROGRESS" "POSTPONED" "FEEDBACK" "|" "DONE" "CANCELLED")))
            ;; Ensure font-lock is refreshed
            (font-lock-flush)
            (font-lock-ensure)))

;; Auto-update kanban boards on save
(add-hook 'before-save-hook
          (lambda ()
            (when (and (eq major-mode 'org-mode)
                       (save-excursion
                         (goto-char (point-min))
                         (re-search-forward "^#\\+BEGIN: kanban" nil t)))
              (org-update-all-dblocks))))

(provide 'init-org)
;;; init-org.el ends here