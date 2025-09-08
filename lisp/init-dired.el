;;; init-dired.el --- Dired configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; Enhanced Dired configuration for file management

;;; Code:

(use-package diredfl
  :ensure t
  :defer t
  :hook (dired-mode . diredfl-mode))

;; Enhanced Dired configuration for multi-file operations
(use-package dired
  :ensure nil
  :config
  (setq dired-dwim-target t)  ; Guess target directory
  (setq dired-recursive-copies 'always)
  (setq dired-recursive-deletes 'always)
  ;; Use macOS-compatible ls options (no --group-directories-first)
  (setq dired-listing-switches "-alh")

  ;; Enable multiple file marking with mouse
  (define-key dired-mode-map (kbd "<mouse-2>") 'dired-find-file)
  (define-key dired-mode-map (kbd "C-<mouse-1>") 'dired-mouse-mark)

  ;; Quick marking shortcuts
  (define-key dired-mode-map (kbd "* .") 'dired-mark-extension)
  (define-key dired-mode-map (kbd "* /") 'dired-mark-directories))

;; Custom sorting: directories first (dotted first), then files (dotted first)
(defun dired-sort-dotfiles-first ()
  "Sort dired: dirs first (dots first within), then files (dots first within)."
  (save-excursion
    (let (buffer-read-only)
      (goto-char (point-min))
      ;; Skip past the directory header
      (while (and (not (eobp))
                  (not (looking-at "^  \\|^\\s-*$")))
        (forward-line 1))
      (let ((start (point))
            dirs dotdirs files dotfiles special-entries)
        ;; Collect all entries
        (while (not (eobp))
          (when (looking-at "^  \\(.*\\)$")
            (let* ((line (match-string 0))
                   (filename (ignore-errors (dired-get-filename 'no-dir t))))
              (cond
               ;; Keep . and .. entries separate to put at top
               ((member filename '("." ".."))
                (push line special-entries))
               ;; Process other entries
               (filename
                (let ((fullpath (ignore-errors (dired-get-filename t))))
                  (when fullpath
                    (cond
                     ;; Dot directory
                     ((and (file-directory-p fullpath)
                           (string-prefix-p "." filename))
                      (push line dotdirs))
                     ;; Regular directory
                     ((file-directory-p fullpath)
                      (push line dirs))
                     ;; Dotfile
                     ((string-prefix-p "." filename)
                      (push line dotfiles))
                     ;; Regular file
                     (t
                      (push line files)))))))))
          (forward-line 1))
        ;; Delete old content and insert sorted
        (when (or special-entries dirs dotdirs files dotfiles)
          (delete-region start (point-max))
          (goto-char start)
          ;; Insert in order: . and .., dot dirs, regular dirs, dotfiles, regular files
          (dolist (line (nreverse special-entries))
            (insert line "\n"))
          (dolist (line (sort (nreverse dotdirs) 'string<))
            (insert line "\n"))
          (dolist (line (sort (nreverse dirs) 'string<))
            (insert line "\n"))
          (dolist (line (sort (nreverse dotfiles) 'string<))
            (insert line "\n"))
          (dolist (line (sort (nreverse files) 'string<))
            (insert line "\n")))))
    (set-buffer-modified-p nil)))

;; Apply custom sorting after dired reads directory
(add-hook 'dired-after-readin-hook 'dired-sort-dotfiles-first)

(provide 'init-dired)
;;; init-dired.el ends here