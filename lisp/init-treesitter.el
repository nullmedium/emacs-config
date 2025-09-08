;;; init-treesitter.el --- Tree-sitter configuration for Emacs 29+ -*- lexical-binding: t -*-
;;; Commentary:
;;; Modern syntax highlighting and code analysis with tree-sitter

;;; Code:

;; Only load tree-sitter configuration if Emacs 29+ with tree-sitter support
(when (and (fboundp 'treesit-available-p)
           (treesit-available-p))
  
  ;; Configure tree-sitter languages
  (setq treesit-language-source-alist
        '((bash "https://github.com/tree-sitter/tree-sitter-bash")
          (c "https://github.com/tree-sitter/tree-sitter-c")
          (cmake "https://github.com/uyha/tree-sitter-cmake")
          (cpp "https://github.com/tree-sitter/tree-sitter-cpp")
          (css "https://github.com/tree-sitter/tree-sitter-css")
          (elisp "https://github.com/Wilfred/tree-sitter-elisp")
          (go "https://github.com/tree-sitter/tree-sitter-go")
          (html "https://github.com/tree-sitter/tree-sitter-html")
          (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
          (json "https://github.com/tree-sitter/tree-sitter-json")
          (make "https://github.com/alemuller/tree-sitter-make")
          (markdown "https://github.com/ikatyang/tree-sitter-markdown")
          (python "https://github.com/tree-sitter/tree-sitter-python")
          (rust "https://github.com/tree-sitter/tree-sitter-rust")
          (toml "https://github.com/tree-sitter/tree-sitter-toml")
          (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
          (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
          (yaml "https://github.com/ikatyang/tree-sitter-yaml")))

  ;; Function to install a tree-sitter grammar
  (defun treesit-install-language-grammar (lang)
    "Install tree-sitter grammar for LANG."
    (interactive (list (intern (completing-read "Language: "
                                                (mapcar #'car treesit-language-source-alist)))))
    (unless (treesit-language-available-p lang)
      (let ((lang-source (alist-get lang treesit-language-source-alist)))
        (if lang-source
            (treesit-install-language-grammar lang)
          (message "Language source not configured for %s" lang)))))

  ;; Install all configured grammars
  (defun treesit-install-all-grammars ()
    "Install all configured tree-sitter grammars."
    (interactive)
    (dolist (lang-source treesit-language-source-alist)
      (let ((lang (car lang-source)))
        (unless (treesit-language-available-p lang)
          (message "Installing tree-sitter grammar for %s..." lang)
          (condition-case err
              (treesit-install-language-grammar lang)
            (error (message "Failed to install %s: %s" lang err)))))))

  ;; Remap major modes to use tree-sitter variants
  (setq major-mode-remap-alist
        '((yaml-mode . yaml-ts-mode)
          (bash-mode . bash-ts-mode)
          (js2-mode . js-ts-mode)
          (javascript-mode . js-ts-mode)
          (js-mode . js-ts-mode)
          (typescript-mode . typescript-ts-mode)
          (json-mode . json-ts-mode)
          (css-mode . css-ts-mode)
          (python-mode . python-ts-mode)
          (c-mode . c-ts-mode)
          (c++-mode . c++-ts-mode)
          (cmake-mode . cmake-ts-mode)
          (toml-mode . toml-ts-mode)
          (rust-mode . rust-ts-mode)
          (go-mode . go-ts-mode)))

  ;; Auto-mode-alist for tree-sitter modes
  (add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.tsx\\'" . tsx-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.js\\'" . js-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.mjs\\'" . js-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.json\\'" . json-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.yaml\\'" . yaml-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.toml\\'" . toml-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.go\\'" . go-ts-mode))

  ;; Enhanced font-lock for tree-sitter modes
  (setq treesit-font-lock-level 3)  ; Balanced highlighting (was 4, reduced for performance)

  ;; Tree-sitter debugging helpers
  (defun treesit-inspect-node-at-point ()
    "Show tree-sitter node information at point."
    (interactive)
    (when (treesit-parser-list)
      (let ((node (treesit-node-at (point))))
        (message "Node: %s, Type: %s, Text: %s"
                 node
                 (treesit-node-type node)
                 (treesit-node-text node)))))

  ;; Add to startup hook to check for tree-sitter grammars
  (add-hook 'emacs-startup-hook
            (lambda ()
              (when (and (treesit-available-p)
                         (not (treesit-language-available-p 'python)))
                (message "Tree-sitter grammars not installed. Run M-x treesit-install-all-grammars"))))

  (message "Tree-sitter support enabled"))

(provide 'init-treesitter)
;;; init-treesitter.el ends here