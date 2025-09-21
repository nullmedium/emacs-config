;;; init-qol.el --- Quality of Life improvements -*- lexical-binding: t -*-
;;; Commentary:
;;; Better help, undo, parentheses, and useful functions

;;; Code:

;;; Helpful - Better help buffers
(use-package helpful
  :ensure t
  :defer t
  :bind
  (("C-h f" . helpful-callable)
   ("C-h v" . helpful-variable)
   ("C-h k" . helpful-key)
   ("C-h F" . helpful-function)
   ("C-h C" . helpful-command)
   ("C-c C-d" . helpful-at-point))
  :config
  ;; Make helpful buffers more readable
  (setq helpful-max-buffers 3))

;;; Undo-tree - Visual undo history
(use-package undo-tree
  :ensure t
  :diminish undo-tree-mode
  :init
  (global-undo-tree-mode)
  :config
  (setq undo-tree-visualizer-timestamps t)
  (setq undo-tree-visualizer-diff t)
  (setq undo-tree-history-directory-alist `(("." . ,(expand-file-name "undo-tree-history" user-emacs-directory))))
  (setq undo-tree-auto-save-history t)
  :bind (("C-x u" . undo-tree-visualize)
         ("C-/" . undo-tree-undo)
         ("C-?" . undo-tree-redo)))

;;; Smartparens - Better parentheses handling
(use-package smartparens
  :ensure t
  :diminish smartparens-mode
  :hook (prog-mode . smartparens-mode)
  :init
  ;; Start disabled to avoid conflicts
  (setq smartparens-global-mode nil)
  :config
  ;; Load default config
  (require 'smartparens-config)
  
  ;; Keybindings
  (define-key smartparens-mode-map (kbd "C-M-f") 'sp-forward-sexp)
  (define-key smartparens-mode-map (kbd "C-M-b") 'sp-backward-sexp)
  (define-key smartparens-mode-map (kbd "C-M-n") 'sp-next-sexp)
  (define-key smartparens-mode-map (kbd "C-M-p") 'sp-previous-sexp)
  (define-key smartparens-mode-map (kbd "C-M-k") 'sp-kill-sexp)
  (define-key smartparens-mode-map (kbd "C-M-w") 'sp-copy-sexp)
  (define-key smartparens-mode-map (kbd "M-<delete>") 'sp-unwrap-sexp)
  (define-key smartparens-mode-map (kbd "M-<backspace>") 'sp-backward-unwrap-sexp)
  ;; Changed from C-<left/right> to avoid conflict with word navigation
  (define-key smartparens-mode-map (kbd "C-c <right>") 'sp-forward-slurp-sexp)
  (define-key smartparens-mode-map (kbd "C-c <left>") 'sp-forward-barf-sexp)
  (define-key smartparens-mode-map (kbd "C-c M-<right>") 'sp-backward-barf-sexp)
  (define-key smartparens-mode-map (kbd "C-c M-<left>") 'sp-backward-slurp-sexp)
  (define-key smartparens-mode-map (kbd "M-D") 'sp-splice-sexp)
  (define-key smartparens-mode-map (kbd "C-]") 'sp-select-next-thing-exchange)
  (define-key smartparens-mode-map (kbd "C-<") 'sp-select-previous-thing)
  (define-key smartparens-mode-map (kbd "C->") 'sp-select-next-thing)
  
  ;; Disable smartparens in minibuffer
  (setq sp-ignore-modes-list '(minibuffer-mode minibuffer-inactive-mode))
  
  ;; Don't use strict mode by default as it can prevent editing
  ;; (dolist (mode '(emacs-lisp-mode-hook
  ;;                 lisp-mode-hook
  ;;                 lisp-interaction-mode-hook
  ;;                 scheme-mode-hook
  ;;                 clojure-mode-hook))
  ;;   (add-hook mode #'smartparens-strict-mode))
  )

;;; Crux - Collection of useful functions
(use-package crux
  :ensure t
  :bind (("C-c o" . crux-open-with)
         ("C-c n" . crux-cleanup-buffer-or-region)
         ("C-c f" . crux-recentf-find-file)
         ("C-M-z" . crux-indent-defun)
         ("C-c u" . crux-view-url)
         ("C-c e" . crux-eval-and-replace)
         ("C-c w" . crux-swap-windows)
         ("C-c D" . crux-delete-file-and-buffer)
         ("C-c r" . crux-rename-buffer-and-file)
         ("C-c t" . crux-visit-term-buffer)
         ("C-c k" . crux-kill-other-buffers)
         ("C-c TAB" . crux-indent-rigidly-and-copy-to-clipboard)
         ("C-c I" . crux-find-user-init-file)
         ("C-c S" . crux-find-shell-init-file)
         ("s-r" . crux-recentf-find-file)
         ("s-j" . crux-top-join-line)
         ("C-^" . crux-top-join-line)
         ("s-k" . crux-kill-whole-line)
         ("C-<backspace>" . crux-kill-line-backwards)
         ("s-o" . crux-smart-open-line-above)
         ("C-S-RET" . crux-smart-open-line-above)
         ("C-RET" . crux-smart-open-line)
         ("C-c c" . crux-create-scratch-buffer)
         ("C-c d" . crux-duplicate-current-line-or-region)
         ("C-c M-d" . crux-duplicate-and-comment-current-line-or-region)
         ("C-c F" . crux-recentf-find-directory)
         ([remap move-beginning-of-line] . crux-move-beginning-of-line)
         ([remap kill-whole-line] . crux-kill-whole-line)
         ([remap kill-line] . crux-smart-kill-line))
  :config
  ;; Use crux cleanup on save for programming modes
  (add-hook 'before-save-hook 'crux-cleanup-buffer-or-region))

;;; Ace-window - Quick window switching
(use-package ace-window
  :ensure t
  :bind (("M-o" . ace-window)
         ("C-x o" . ace-window))
  :config
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  (setq aw-scope 'frame)
  (setq aw-background t)
  ;; Make ace-window numbers larger and more visible
  (custom-set-faces
   '(aw-leading-char-face
     ((t (:inherit ace-jump-face-foreground :height 3.0))))))

;;; Additional useful functions
(defun qol-toggle-window-split ()
  "Toggle between horizontal and vertical split."
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
             (next-win-buffer (window-buffer (next-window)))
             (this-win-edges (window-edges (selected-window)))
             (next-win-edges (window-edges (next-window)))
             (this-win-2nd (not (and (<= (car this-win-edges)
                                         (car next-win-edges))
                                     (<= (cadr this-win-edges)
                                         (cadr next-win-edges)))))
             (splitter
              (if (= (car this-win-edges)
                     (car (window-edges (next-window))))
                  'split-window-horizontally
                'split-window-vertically)))
        (delete-other-windows)
        (let ((first-win (selected-window)))
          (funcall splitter)
          (if this-win-2nd (other-window 1))
          (set-window-buffer (selected-window) this-win-buffer)
          (set-window-buffer (next-window) next-win-buffer)
          (select-window first-win)
          (if this-win-2nd (other-window 1))))))

(global-set-key (kbd "C-x |") 'qol-toggle-window-split)

;;; Window movement with windmove
;; Use Meta (Alt) + arrows instead of Shift+arrows to avoid conflicts with shift-selection
(windmove-default-keybindings 'meta)  ; Use Meta+arrows to move between windows
(setq windmove-wrap-around t)

;;; Better buffer names for duplicates
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)
(setq uniquify-separator "/")
(setq uniquify-after-kill-buffer-p t)
(setq uniquify-ignore-buffers-re "^\\*")

;;; Remember recently opened files
(setq recentf-max-saved-items 500)
(setq recentf-max-menu-items 15)
(setq recentf-auto-cleanup 'never)
(setq recentf-exclude '("^/var/folders\\.*"
                        "COMMIT_EDITMSG\\'"
                        ".*-autoloads\\.el\\'"
                        "[/\\]\\.elpa/"))

;;; Better scrolling
(setq scroll-conservatively 101)
(setq scroll-margin 3)
(setq scroll-preserve-screen-position t)

;;; Enable some disabled commands
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

(provide 'init-qol)
;;; init-qol.el ends here