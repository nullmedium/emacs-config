;;; init-search.el --- Search and navigation tools -*- lexical-binding: t -*-
;;; Commentary:
;;; Configuration for search tools like deadgrep, ripgrep, and wgrep

;;; Code:

;;; Deadgrep - fast search with ripgrep
(use-package deadgrep
  :ensure t
  :defer t
  :commands deadgrep
  :bind (("C-c r" . deadgrep)))

;;; Wgrep - editable grep buffers
(use-package wgrep
  :ensure t
  :defer t
  :after grep
  :config
  (setq wgrep-auto-save-buffer t)
  (setq wgrep-enable-key "r"))

;;; Ripgrep
(use-package ripgrep
  :ensure t
  :defer t
  :commands (ripgrep-regexp))

(provide 'init-search)
;;; init-search.el ends here