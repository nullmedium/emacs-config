;;; god-mode-config.el --- God mode configuration for modal editing -*- lexical-binding: t -*-

;;; Commentary:
;; God mode provides modal editing without leaving Emacs paradigm
;; Press ESC to toggle god-mode, where you can use Emacs commands without modifier keys
;; For example: In god-mode, 'xf' = C-x C-f, 'xs' = C-x C-s

;;; Code:

(use-package god-mode
  :ensure t
  :bind (("<escape>" . god-mode-all)
         ("C-x C-1" . delete-other-windows)
         ("C-x C-2" . split-window-below)
         ("C-x C-3" . split-window-right)
         ("C-x C-0" . delete-window))
  :config
  ;; Define keybindings after god-mode is loaded and map exists
  (with-eval-after-load 'god-mode
    (when (boundp 'god-local-mode-map)
      (define-key god-local-mode-map (kbd ".") 'repeat)
      (define-key god-local-mode-map (kbd "[") 'backward-paragraph)
      (define-key god-local-mode-map (kbd "]") 'forward-paragraph)
      (define-key god-local-mode-map (kbd "i") 'god-mode-all)))
  ;; Update cursor to indicate god-mode state
  (defun god-mode-update-cursor ()
    "Update cursor style based on god-mode state."
    (setq cursor-type (if (or god-local-mode buffer-read-only)
                          'box
                        'bar)))
  
  ;; Change cursor color based on state
  (defun god-mode-update-cursor-color ()
    "Change cursor color to indicate god-mode state."
    (set-cursor-color (if (or god-local-mode buffer-read-only)
                          "#ff7a85"  ; Red cursor in god-mode
                        "#61afef")))  ; Blue cursor in insert mode
  
  (add-hook 'god-mode-enabled-hook 'god-mode-update-cursor)
  (add-hook 'god-mode-disabled-hook 'god-mode-update-cursor)
  (add-hook 'god-mode-enabled-hook 'god-mode-update-cursor-color)
  (add-hook 'god-mode-disabled-hook 'god-mode-update-cursor-color)
  
  ;; Update modeline to show god-mode state
  (defun god-mode-update-modeline ()
    "Update modeline to indicate god-mode state."
    (let ((limited-colors-p (> 257 (length (defined-colors)))))
      (cond (god-local-mode (progn
                              (set-face-attribute 'mode-line nil
                                                  :foreground "#604000"
                                                  :background "#fff29a")
                              (set-face-attribute 'mode-line-inactive nil
                                                  :foreground "#3f3000"
                                                  :background "#fff3da")))
            (t (progn
                 (set-face-attribute 'mode-line nil
                                     :foreground (face-attribute 'mode-line :foreground)
                                     :background (face-attribute 'mode-line :background))
                 (set-face-attribute 'mode-line-inactive nil
                                     :foreground (face-attribute 'mode-line-inactive :foreground)
                                     :background (face-attribute 'mode-line-inactive :background)))))))
  
  ;; Lighter modeline indicator
  (defun my-god-mode-update-modeline ()
    "Minimal modeline indicator for god-mode."
    (cond
     ((bound-and-true-p god-local-mode)
      (set-face-attribute 'mode-line nil
                          :background "#2d3640"  ; Slightly different background
                          :box '(:line-width 2 :color "#ff7a85")))  ; Red border
     (t
      (set-face-attribute 'mode-line nil
                          :background "#232830"  ; Normal background
                          :box '(:line-width 1 :color "#3a4049")))))  ; Normal border
  
  ;; Use the lighter modeline update
  (add-hook 'god-mode-enabled-hook 'my-god-mode-update-modeline)
  (add-hook 'god-mode-disabled-hook 'my-god-mode-update-modeline)
  
  ;; Make certain commands exit god-mode automatically
  (defun god-mode-self-insert-exit ()
    "Exit god-mode when self-inserting."
    (when god-local-mode
      (god-mode-all)))
  
  ;; Optional: Exit god-mode on certain commands
  ;; (add-to-list 'god-exempt-major-modes 'dired-mode)
  ;; (add-to-list 'god-exempt-major-modes 'magit-mode)
  
  ;; Better integration with isearch
  (define-key isearch-mode-map (kbd "<escape>") 'god-mode-isearch-activate)
  (define-key god-local-mode-map (kbd "/") 'isearch-forward)
  (define-key god-local-mode-map (kbd "?") 'isearch-backward)
  
  (defun god-mode-isearch-activate ()
    "Exit isearch and activate god-mode."
    (interactive)
    (isearch-exit)
    (god-mode-all))
  
  ;; Quick navigation bindings in god-mode
  (define-key god-local-mode-map (kbd "j") 'next-line)
  (define-key god-local-mode-map (kbd "k") 'previous-line)
  (define-key god-local-mode-map (kbd "h") 'backward-char)
  (define-key god-local-mode-map (kbd "l") 'forward-char)
  (define-key god-local-mode-map (kbd "w") 'forward-word)
  (define-key god-local-mode-map (kbd "b") 'backward-word)
  (define-key god-local-mode-map (kbd "e") 'move-end-of-line)
  (define-key god-local-mode-map (kbd "a") 'move-beginning-of-line)
  (define-key god-local-mode-map (kbd "v") 'scroll-up-command)
  (define-key god-local-mode-map (kbd "V") 'scroll-down-command)
  (define-key god-local-mode-map (kbd "g") 'keyboard-quit)  ; Like C-g
  (define-key god-local-mode-map (kbd "u") 'undo)
  (define-key god-local-mode-map (kbd "/") 'isearch-forward)
  (define-key god-local-mode-map (kbd "?") 'isearch-backward)
  (define-key god-local-mode-map (kbd ">") 'end-of-buffer)
  (define-key god-local-mode-map (kbd "<") 'beginning-of-buffer)
  (define-key god-local-mode-map (kbd "SPC") 'set-mark-command)
  
  ;; Special god-mode specific commands
  (define-key god-local-mode-map (kbd "z") 'god-mode-all)  ; Quick toggle
  
  ;; Support for literal key insertion
  (define-key god-local-mode-map (kbd "q") 'quoted-insert)  ; Like C-q
  
  ;; Window management in god-mode (no C-x needed)
  (define-key god-local-mode-map (kbd "1") 'delete-other-windows)
  (define-key god-local-mode-map (kbd "2") 'split-window-below)
  (define-key god-local-mode-map (kbd "3") 'split-window-right)
  (define-key god-local-mode-map (kbd "0") 'delete-window)
  (define-key god-local-mode-map (kbd "o") 'other-window))

;; God-mode indicator in modeline
(defun god-mode-modeline-indicator ()
  "Return a string indicating god-mode state."
  (cond
   ((bound-and-true-p god-local-mode)
    (propertize " GOD " 'face '(:background "#ff7a85" :foreground "#1a1d23" :weight bold)))
   (t "")))

;; Add to mode-line
(setq-default mode-line-format
              (cons '(:eval (god-mode-modeline-indicator))
                    (default-value 'mode-line-format)))

;; Integration with other modes
(defun god-mode-helm-integration ()
  "Better integration with Helm."
  (require 'helm nil t)
  (when (featurep 'helm)
    (define-key god-local-mode-map (kbd "xx") 'helm-M-x)
    (define-key god-local-mode-map (kbd "xf") 'helm-find-files)
    (define-key god-local-mode-map (kbd "xb") 'helm-buffers-list)
    (define-key god-local-mode-map (kbd "xa") 'helm-apropos)
    (define-key god-local-mode-map (kbd "xr") 'helm-recentf)))

(with-eval-after-load 'helm
  (god-mode-helm-integration))

;; Quick toggle function
(defun god-mode-toggle ()
  "Toggle god-mode."
  (interactive)
  (god-mode-all))

;; Helper functions for common operations
(defun god-mode-kill-line ()
  "Kill line in god-mode style."
  (interactive)
  (if god-local-mode
      (kill-line)
    (god-mode-all)
    (kill-line)))

(defun god-mode-save-buffer ()
  "Save buffer, works in both modes."
  (interactive)
  (save-buffer)
  (when god-local-mode
    (message "Buffer saved (god-mode active)")))

;; Bind common operations
(global-set-key (kbd "C-c g") 'god-mode-all)  ; Alternative toggle

;; Visual feedback for god-mode state changes
(defun god-mode-bell ()
  "Visual bell for god-mode state changes."
  (let ((buf (current-buffer)))
    (with-current-buffer buf
      (inverse-video-mode)
      (run-with-timer 0.1 nil (lambda ()
                                (with-current-buffer buf
                                  (inverse-video-mode)))))))

;; Optional: Add bell on state change
;; (add-hook 'god-mode-enabled-hook 'god-mode-bell)
;; (add-hook 'god-mode-disabled-hook 'god-mode-bell)

;; Cheat sheet function
(defun god-mode-cheat-sheet ()
  "Display god-mode cheat sheet."
  (interactive)
  (with-output-to-temp-buffer "*God Mode Cheat Sheet*"
    (princ "GOD MODE CHEAT SHEET\n")
    (princ "====================\n\n")
    (princ "ACTIVATION:\n")
    (princ "  ESC or C-c g  : Toggle god-mode\n")
    (princ "  i or z        : Exit god-mode (return to insert)\n\n")
    (princ "MOVEMENT (in god-mode):\n")
    (princ "  h/j/k/l       : Left/Down/Up/Right (Vim-style)\n")
    (princ "  w/b           : Forward/Backward word\n")
    (princ "  a/e           : Beginning/End of line\n")
    (princ "  </>/          : Beginning/End of buffer\n")
    (princ "  [/]           : Previous/Next paragraph\n\n")
    (princ "EDITING:\n")
    (princ "  d             : C-d (delete char)\n")
    (princ "  k             : C-k (kill line)\n")
    (princ "  u             : Undo\n")
    (princ "  SPC           : Set mark\n")
    (princ "  y             : C-y (yank/paste)\n")
    (princ "  w             : C-w (kill region)\n\n")
    (princ "COMMANDS (no C- needed):\n")
    (princ "  xs            : Save file (C-x C-s)\n")
    (princ "  xf            : Find file (C-x C-f)\n")
    (princ "  xb            : Switch buffer (C-x b)\n")
    (princ "  xk            : Kill buffer (C-x k)\n")
    (princ "  xx            : M-x\n\n")
    (princ "WINDOWS:\n")
    (princ "  1/2/3/0       : Delete other/Split below/Split right/Delete window\n")
    (princ "  o             : Other window\n\n")
    (princ "SEARCH:\n")
    (princ "  //            : Search forward\n")
    (princ "  ?             : Search backward\n")
    (princ "  n             : C-n (next line or search result)\n\n")
    (princ "SPECIAL:\n")
    (princ "  .             : Repeat last command\n")
    (princ "  g             : Keyboard quit (C-g)\n")
    (princ "  q             : Quoted insert (C-q)\n\n")
    (princ "TIP: Most C- commands work by just dropping the C- in god-mode!\n")
    (princ "     For C-x sequences, just type x then the letter.\n")
    (princ "     For M-x, type xx\n")))

(global-set-key (kbd "C-c G") 'god-mode-cheat-sheet)

;; Make god-mode play nice with company
(with-eval-after-load 'company
  (define-key god-local-mode-map (kbd "TAB") 'company-indent-or-complete-common)
  (add-hook 'company-mode-hook
            (lambda ()
              (when (bound-and-true-p god-local-mode)
                (god-mode-all)))))

(provide 'god-mode-config)
;;; god-mode-config.el ends here