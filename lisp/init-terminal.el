;;; init-terminal.el --- Terminal emulator configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; Configuration for eat (Emacs Terminal Emulator) and other terminal settings

;;; Code:

;;; Eat - Emacs Terminal Emulator
(use-package eat
  :ensure t
  :defer t
  :hook ((eshell-mode . eat-eshell-mode)
         (eshell-mode . eat-eshell-visual-command-mode))
  :config
  ;; Close eat buffer when process exits
  (setq eat-kill-buffer-on-exit t)
  
  ;; Enable eat for eshell
  (eat-eshell-mode 1)
  
  ;; Better color support
  (setq eat-term-name "xterm-256color")
  
  ;; Set default shell
  (setq eat-shell (getenv "SHELL"))
  
  ;; Increase scrollback
  (setq eat-term-scrollback-size 10000)
  
  ;; Enable mouse support
  (setq eat-enable-mouse t)
  
  ;; Custom keybindings for eat
  (add-hook 'eat-mode-hook
            (lambda ()
              (local-set-key (kbd "C-c C-d") 'eat-self-input)  ; Send C-d to terminal
              (local-set-key (kbd "C-c C-c") 'eat-self-input)  ; Send C-c to terminal
              (local-set-key (kbd "C-c C-z") 'eat-self-input)  ; Send C-z to terminal
              (local-set-key (kbd "C-c C-k") 'eat-kill-process) ; Kill terminal process
              (local-set-key (kbd "C-c C-p") 'eat-previous-shell-prompt)
              (local-set-key (kbd "C-c C-n") 'eat-next-shell-prompt)))
  
  ;; Auto-close on exit codes
  (defun eat-close-on-exit ()
    "Close eat buffer automatically when shell exits."
    (when (and (eq major-mode 'eat-mode)
               (not (process-live-p (get-buffer-process (current-buffer)))))
      (kill-buffer)))
  
  ;; Add hook to close buffer when process exits
  (add-hook 'eat-exit-hook 'eat-close-on-exit)
  
  ;; Alternative: Close with a delay to see exit message
  (defun eat-close-on-exit-with-delay ()
    "Close eat buffer with a small delay after exit."
    (run-with-timer 0.5 nil
                    (lambda (buf)
                      (when (buffer-live-p buf)
                        (kill-buffer buf)))
                    (current-buffer)))
  
  ;; Uncomment this and comment the previous hook if you want a delay
  ;; (add-hook 'eat-exit-hook 'eat-close-on-exit-with-delay)
  )

;;; Quick terminal access
(defun open-eat-terminal ()
  "Open a new eat terminal in the current window."
  (interactive)
  (eat))

(defun open-eat-terminal-here ()
  "Open a new eat terminal in the current directory."
  (interactive)
  (let ((default-directory (if (buffer-file-name)
                               (file-name-directory (buffer-file-name))
                             default-directory)))
    (eat)))

(defun open-eat-terminal-split ()
  "Open a new eat terminal in a horizontal split."
  (interactive)
  (split-window-below)
  (other-window 1)
  (eat))

(defun open-eat-terminal-vsplit ()
  "Open a new eat terminal in a vertical split."
  (interactive)
  (split-window-right)
  (other-window 1)
  (eat))

;;; Global keybindings for terminal access - using C-c E for Eat terminal
(global-set-key (kbd "C-c E t") 'open-eat-terminal)
(global-set-key (kbd "C-c E h") 'open-eat-terminal-here)
(global-set-key (kbd "C-c E s") 'open-eat-terminal-split)
(global-set-key (kbd "C-c E v") 'open-eat-terminal-vsplit)

;;; Eshell configuration (as alternative to eat)
(use-package eshell
  :ensure nil
  :config
  ;; Close eshell on exit
  (defun eshell-close-on-exit ()
    "Close eshell window on exit."
    (when (not (one-window-p))
      (delete-window)))
  
  (add-hook 'eshell-exit-hook 'eshell-close-on-exit)
  
  ;; Better prompt
  (setq eshell-prompt-function
        (lambda ()
          (concat
           (propertize (abbreviate-file-name (eshell/pwd)) 'face '(:foreground "blue"))
           (if (= (user-uid) 0) " # " " $ "))))
  
  (setq eshell-highlight-prompt t)
  (setq eshell-prompt-regexp "^[^#$\n]* [#$] "))

(provide 'init-terminal)
;;; init-terminal.el ends here