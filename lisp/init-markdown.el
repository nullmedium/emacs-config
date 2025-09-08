;;; init-markdown.el --- Markdown support configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; Markdown editing and preview configuration

;;; Code:

(use-package markdown-mode
  :ensure t
  :defer t
  :commands (markdown-mode gfm-mode)
  :mode (("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode)
         ("README\\.md\\'" . gfm-mode))
  :hook (markdown-mode . (lambda ()
                          (visual-line-mode 1)
                          (flyspell-mode 1)
                          (auto-fill-mode -1)))
  :bind (:map markdown-mode-map
              ("C-c C-l" . markdown-insert-link)
              ("C-c C-i" . markdown-insert-image)
              ("C-c C-c p" . markdown-preview)
              ("C-c C-c l" . markdown-live-preview-mode))
  :config
  (setq markdown-command "markdown")
  (setq markdown-enable-wiki-links t)
  (setq markdown-italic-underscore t)
  (setq markdown-asymmetric-header t)
  (setq markdown-make-gfm-checkboxes-buttons t)
  (setq markdown-gfm-uppercase-checkbox t)
  (setq markdown-fontify-code-blocks-natively t))

(use-package markdown-toc
  :ensure t
  :after markdown-mode
  :defer t
  :bind (:map markdown-mode-map
              ("C-c C-t" . markdown-toc-generate-or-refresh-toc)))

(use-package grip-mode
  :ensure t
  :after markdown-mode
  :defer t
  :bind (:map markdown-mode-map
              ("C-c C-c g" . grip-mode)))

(use-package obsidian
  :ensure t
  :defer t)

(provide 'init-markdown)
;;; init-markdown.el ends here