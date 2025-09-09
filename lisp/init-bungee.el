;;; init-bungee.el --- Initialize Bungee symbol finder -*- lexical-binding: t; -*-

;; Load and configure the Bungee package
(load-file "~/.emacs.d/bungee.el")
(require 'bungee)

;; Configure cache directory
(setq bungee-cache-directory ".symbol_cache")  ; Use standard cache dir

;; Optional: Use Python indexer for better parsing
;; Only set if the file exists
(let ((python-indexer-path "~/sources/bungee/symbol_finder.py"))
  (when (file-exists-p (expand-file-name python-indexer-path))
    (setq bungee-python-indexer python-indexer-path)))

;; Optional: Save JSON cache for Python tool compatibility
(setq bungee-save-json-cache nil)  ; Set to t if you need Python compatibility

;; Enable auto-update when saving files
(setq bungee-auto-update t)

;; Enable Bungee mode globally for supported files
(global-bungee-mode 1)

;; Override M-. in QML and C++ files to use Bungee
(add-hook 'qml-mode-hook
          (lambda ()
            (local-set-key (kbd "M-.") 'bungee-jump-to-definition)
            (local-set-key (kbd "M-?") 'bungee-find-references)
            (local-set-key (kbd "M-,") 'pop-tag-mark)))

(add-hook 'c++-mode-hook
          (lambda ()
            (local-set-key (kbd "M-.") 'bungee-jump-to-definition)
            (local-set-key (kbd "M-?") 'bungee-find-references)
            (local-set-key (kbd "M-,") 'pop-tag-mark)))

;; For .qml files if qml-mode is not available
(add-to-list 'auto-mode-alist '("\\.qml\\'" . js-mode))
(add-hook 'js-mode-hook
          (lambda ()
            (when (and buffer-file-name
                       (string-match-p "\\.qml\\'" buffer-file-name))
              (bungee-mode 1)
              (local-set-key (kbd "M-.") 'bungee-jump-to-definition)
              (local-set-key (kbd "M-?") 'bungee-find-references)
              (local-set-key (kbd "M-,") 'pop-tag-mark))))

;; Convenient commands
(defun bungee-reindex-project ()
  "Reindex the entire project using Python indexer."
  (interactive)
  (if bungee-python-indexer
      (bungee-index-with-python t)
    (bungee-index-directory nil t)))

(defun bungee-reindex-current-project ()
  "Force reindex current project."
  (interactive)
  (bungee-index-directory nil t))

(provide 'init-bungee)
;;; init-bungee.el ends here
