;;; init-search.el --- Search and navigation tools -*- lexical-binding: t -*-
;;; Commentary:
;;; Configuration for search tools like deadgrep, ripgrep, and wgrep

;;; Code:

;;; Deadgrep - fast search with ripgrep
(use-package deadgrep
  :ensure t
  :defer t
  :commands deadgrep
  :bind (("C-c d g" . deadgrep)))

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

;;; Live search functions using consult-ripgrep
(with-eval-after-load 'consult
  (defun search-project-for-symbol-at-point ()
    "Search project for symbol at point using consult-ripgrep."
    (interactive)
    (require 'projectile)
    (if (use-region-p)
        (consult-ripgrep (projectile-project-root) 
                         (buffer-substring-no-properties (region-beginning) (region-end)))
      (consult-ripgrep (projectile-project-root) (thing-at-point 'symbol))))

  (defun search-project ()
    "Live search in project files using consult-ripgrep."
    (interactive)
    (require 'projectile)
    (consult-ripgrep (projectile-project-root)))

  (defun search-current-directory ()
    "Live search in current directory using consult-ripgrep."
    (interactive)
    (consult-ripgrep default-directory)))

;;; Enhanced grep with live preview
(use-package consult
  :defer t
  :config
  ;; Configure ripgrep arguments for better results
  (setq consult-ripgrep-args
        "rg --null --line-buffered --color=never --max-columns=1000 --path-separator / --smart-case --no-heading --with-filename --line-number --search-zip")
  
  ;; Preview at point for grep commands
  (consult-customize
   consult-ripgrep consult-git-grep consult-grep
   :preview-key '(:debounce 0.4 any)))

;;; Set up search keymap
(define-prefix-command 'search-map)
(global-set-key (kbd "C-c s") 'search-map)

;;; Convenient keybindings for search - using deferred loading
(define-key search-map (kbd "p") 
  (lambda () (interactive) 
    (require 'consult) 
    (require 'projectile)
    (if (fboundp 'search-project)
        (call-interactively 'search-project)
      (consult-ripgrep (projectile-project-root)))))

(define-key search-map (kbd "s")
  (lambda () (interactive)
    (require 'consult)
    (require 'projectile) 
    (if (fboundp 'search-project-for-symbol-at-point)
        (call-interactively 'search-project-for-symbol-at-point)
      (consult-ripgrep (projectile-project-root) (thing-at-point 'symbol)))))

(define-key search-map (kbd "d")
  (lambda () (interactive)
    (require 'consult)
    (consult-ripgrep default-directory)))

(define-key search-map (kbd "r")
  (lambda () (interactive)
    (require 'consult)
    (call-interactively 'consult-ripgrep)))

(define-key search-map (kbd "g")
  (lambda () (interactive)
    (require 'consult)
    (call-interactively 'consult-grep)))

(define-key search-map (kbd "G")
  (lambda () (interactive)
    (require 'consult)
    (call-interactively 'consult-git-grep)))

(define-key search-map (kbd "l")
  (lambda () (interactive)
    (require 'consult)
    (call-interactively 'consult-line)))

(define-key search-map (kbd "L")
  (lambda () (interactive)
    (require 'consult)
    (call-interactively 'consult-line-multi)))

(define-key search-map (kbd "o")
  (lambda () (interactive)
    (require 'consult)
    (call-interactively 'consult-outline)))

;;; Alternative: Helm-ag for live search (if you prefer helm)
;; Uncomment if you want to use helm-ag instead
;; (use-package helm-ag
;;   :ensure t
;;   :defer t
;;   :config
;;   (setq helm-ag-base-command "rg --vimgrep --no-heading --smart-case"))

;;; Ag (The Silver Searcher) - alternative to ripgrep
(use-package ag
  :ensure t
  :defer t
  :commands (ag ag-project ag-regexp)
  :config
  (setq ag-highlight-search t)
  (setq ag-reuse-buffers t))

;;; Rg.el - Another ripgrep interface
(use-package rg
  :ensure t
  :defer t
  :commands (rg rg-project rg-dwim)
  :config
  (rg-enable-default-bindings))

;;; Search and replace across project
(defun project-search-and-replace (search-string replace-string)
  "Search for SEARCH-STRING and replace with REPLACE-STRING in project."
  (interactive 
   (list (read-string "Search: " (thing-at-point 'symbol))
         (read-string "Replace: ")))
  (let ((project-root (projectile-project-root)))
    (rg search-string "*" project-root)
    (with-current-buffer "*rg*"
      (wgrep-change-to-wgrep-mode)
      (goto-char (point-min))
      (while (re-search-forward search-string nil t)
        (replace-match replace-string))
      (wgrep-finish-edit))))

(define-key search-map (kbd "R") 'project-search-and-replace)

;;; Help function to show all search keybindings
(defun show-search-help ()
  "Show available search commands and keybindings."
  (interactive)
  (with-output-to-temp-buffer "*Search Commands Help*"
    (princ "=== SEARCH COMMANDS ===\n\n")
    (princ "LIVE SEARCH (with preview):\n")
    (princ "  C-c s p    : Search in project files (live)\n")
    (princ "  C-c s s    : Search project for symbol at point\n")
    (princ "  C-c s d    : Search in current directory\n")
    (princ "  C-c s r    : Consult ripgrep (specify directory)\n")
    (princ "  C-c s l    : Search lines in current buffer\n")
    (princ "  C-c s L    : Search lines in multiple buffers\n")
    (princ "  C-c s o    : Search outline/headings in buffer\n\n")
    (princ "OTHER SEARCH TOOLS:\n")
    (princ "  C-c s g    : Consult grep\n")
    (princ "  C-c s G    : Consult git-grep\n")
    (princ "  C-c d g    : Deadgrep (ripgrep with nice UI)\n")
    (princ "  C-c s R    : Project search and replace\n")
    (princ "  M-s .      : isearch symbol at point\n\n")
    (princ "TIPS:\n")
    (princ "  - In consult-ripgrep, use '#pattern' to filter results\n")
    (princ "  - Use C-SPC to preview different results\n")
    (princ "  - In deadgrep results, press 'r' to enable wgrep mode for editing\n")))

(define-key search-map (kbd "h") 'show-search-help)
(define-key search-map (kbd "?") 'show-search-help)

(provide 'init-search)
;;; init-search.el ends here