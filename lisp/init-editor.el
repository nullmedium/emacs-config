;;; init-editor.el --- Editor enhancements and editing settings -*- lexical-binding: t -*-
;;; Commentary:
;;; Basic editor settings and enhancements for a better editing experience

;;; Code:

;;; Basic editing settings
(global-auto-revert-mode t)
;; Electric-pair-mode is replaced by smartparens in init-qol.el

;;; Whitespace handling
;; Show trailing whitespace in programming modes
(add-hook 'prog-mode-hook
          (lambda ()
            (setq show-trailing-whitespace t)))

;; Also show trailing whitespace in text modes
(add-hook 'text-mode-hook
          (lambda ()
            (setq show-trailing-whitespace t)))

;; Delete trailing whitespace on save
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Optionally, you can make trailing whitespace more visible
(setq-default show-trailing-whitespace t)
(custom-set-faces
 '(trailing-whitespace ((t (:background "red1")))))

;;; Shift-Selection Configuration
;; Enable shift-select mode for selecting text with Shift+Arrow keys
(setq shift-select-mode t)
(transient-mark-mode t)

;; Standard word navigation with C-left/right
(global-set-key (kbd "C-<left>") 'left-word)
(global-set-key (kbd "C-<right>") 'right-word)

;; Fix for C-Shift-Arrow word selection with CUA mode
(defun backward-word-select (&optional arg)
  "Move backward by words, extending selection with shift."
  (interactive "^p")
  (unless (region-active-p)
    (push-mark (point) nil t))
  (backward-word (or arg 1)))

(defun forward-word-select (&optional arg)
  "Move forward by words, extending selection with shift."
  (interactive "^p")
  (unless (region-active-p)
    (push-mark (point) nil t))
  (forward-word (or arg 1)))

;; Alternative implementation using left-word/right-word directly
(defun left-word-select (&optional arg)
  "Move left by words, extending selection with shift."
  (interactive "^p")
  (unless (region-active-p)
    (push-mark (point) nil t))
  (left-word (or arg 1)))

(defun right-word-select (&optional arg)
  "Move right by words, extending selection with shift."
  (interactive "^p")
  (unless (region-active-p)
    (push-mark (point) nil t))
  (right-word (or arg 1)))

;; Bind C-Shift-Arrow keys for word selection - multiple approaches
;; Use the built-in left-word/right-word with shift handling
(global-set-key (kbd "C-S-<left>") 'left-word-select)
(global-set-key (kbd "C-S-<right>") 'right-word-select)

;; Alternative bindings for compatibility
(global-set-key [(control shift left)] 'left-word-select)
(global-set-key [(control shift right)] 'right-word-select)

;; Add C-Shift-Up/Down for paragraph selection
(defun backward-paragraph-select (&optional arg)
  "Move backward by paragraphs, extending selection with shift."
  (interactive "^p")
  (unless (region-active-p)
    (push-mark (point) nil t))
  (backward-paragraph (or arg 1)))

(defun forward-paragraph-select (&optional arg)
  "Move forward by paragraphs, extending selection with shift."
  (interactive "^p")
  (unless (region-active-p)
    (push-mark (point) nil t))
  (forward-paragraph (or arg 1)))

(global-set-key (kbd "C-S-<up>") 'backward-paragraph-select)
(global-set-key (kbd "C-S-<down>") 'forward-paragraph-select)
(global-set-key [(control shift up)] 'backward-paragraph-select)
(global-set-key [(control shift down)] 'forward-paragraph-select)

;; Mark shift-selection functions properly for CUA compatibility
(put 'backward-word-select 'CUA 'move)
(put 'forward-word-select 'CUA 'move)
(put 'left-word-select 'CUA 'move)
(put 'right-word-select 'CUA 'move)
(put 'backward-paragraph-select 'CUA 'move)
(put 'forward-paragraph-select 'CUA 'move)
(put 'backward-word-select 'shift-selection-mode t)
(put 'forward-word-select 'shift-selection-mode t)
(put 'left-word-select 'shift-selection-mode t)
(put 'right-word-select 'shift-selection-mode t)
(put 'backward-paragraph-select 'shift-selection-mode t)
(put 'forward-paragraph-select 'shift-selection-mode t)

;; Ensure bindings work after CUA mode initialization
(defun ensure-word-selection-bindings ()
  "Ensure C-Shift-Arrow word selection bindings are active."
  (global-set-key (kbd "C-S-<left>") 'left-word-select)
  (global-set-key (kbd "C-S-<right>") 'right-word-select)
  (global-set-key (kbd "C-S-<up>") 'backward-paragraph-select)
  (global-set-key (kbd "C-S-<down>") 'forward-paragraph-select)
  (global-set-key [(control shift left)] 'left-word-select)
  (global-set-key [(control shift right)] 'right-word-select)
  (global-set-key [(control shift up)] 'backward-paragraph-select)
  (global-set-key [(control shift down)] 'forward-paragraph-select))

;; Apply bindings after CUA mode loads
(with-eval-after-load 'cua-base
  (ensure-word-selection-bindings))

;; Also apply after init in case CUA mode is loaded later
(add-hook 'after-init-hook 'ensure-word-selection-bindings)

;; Additional selection keybindings for consistency
;; Ensure regular shift-arrow selection works
(global-set-key (kbd "S-<left>") 'backward-char)
(global-set-key (kbd "S-<right>") 'forward-char)
(global-set-key (kbd "S-<up>") 'previous-line)
(global-set-key (kbd "S-<down>") 'next-line)

;; Line selection with Shift-Home/End
(global-set-key (kbd "S-<home>") 'beginning-of-line)
(global-set-key (kbd "S-<end>") 'end-of-line)

;; Buffer selection with C-Shift-Home/End
(global-set-key (kbd "C-S-<home>") 'beginning-of-buffer)
(global-set-key (kbd "C-S-<end>") 'end-of-buffer)

;; Mark all movement functions as CUA-compatible
(put 'backward-char 'CUA 'move)
(put 'forward-char 'CUA 'move)
(put 'previous-line 'CUA 'move)
(put 'next-line 'CUA 'move)
(put 'beginning-of-line 'CUA 'move)
(put 'end-of-line 'CUA 'move)
(put 'beginning-of-buffer 'CUA 'move)
(put 'end-of-buffer 'CUA 'move)

;;; Text manipulation
(global-set-key (kbd "C-<return>") 'cua-set-rectangle-mark)

;; Diagnostic function for CUA and selection keybindings
(defun diagnose-cua-selection ()
  "Diagnose CUA and selection keybinding issues."
  (interactive)
  (with-output-to-temp-buffer "*CUA Selection Diagnostics*"
    (princ "=== CUA AND SELECTION DIAGNOSTICS ===\n\n")
    (princ "CUA Mode Settings:\n")
    (princ (format "  CUA mode: %s\n" (if cua-mode "ENABLED" "DISABLED")))
    (princ (format "  CUA keys: %s\n" (if cua-enable-cua-keys "ENABLED" "DISABLED")))
    (princ (format "  Prefix delay: %s\n" cua-prefix-override-inhibit-delay))
    (princ (format "  Keep region after copy: %s\n" cua-keep-region-after-copy))
    (princ "\nShift Selection:\n")
    (princ (format "  Shift-select mode: %s\n" (if shift-select-mode "ENABLED" "DISABLED")))
    (princ (format "  Transient mark mode: %s\n" (if transient-mark-mode "ENABLED" "DISABLED")))
    (princ "\nKey Bindings:\n")
    (princ "  Copy/Paste:\n")
    (princ (format "    C-c: %s\n" (key-binding (kbd "C-c"))))
    (princ (format "    C-v: %s\n" (key-binding (kbd "C-v"))))
    (princ (format "    C-x: %s\n" (key-binding (kbd "C-x"))))
    (princ "  Word Selection:\n")
    (princ (format "    C-S-<left>: %s\n" (key-binding (kbd "C-S-<left>"))))
    (princ (format "    C-S-<right>: %s\n" (key-binding (kbd "C-S-<right>"))))
    (princ (format "    [(control shift left)]: %s\n" (key-binding [(control shift left)])))
    (princ (format "    [(control shift right)]: %s\n" (key-binding [(control shift right)])))
    (princ "  Character Selection:\n")
    (princ (format "    S-<left>: %s\n" (key-binding (kbd "S-<left>"))))
    (princ (format "    S-<right>: %s\n" (key-binding (kbd "S-<right>"))))
    (princ "\nTo fix issues:\n")
    (princ "  M-x ensure-cua-after-init\n")
    (princ "  M-x reload-emacs-config\n")))

;; Global keybinding for diagnostics
(global-set-key (kbd "C-c C-d c") 'diagnose-cua-selection)

;; Additional diagnostic function for key conflicts
(defun diagnose-key-conflicts ()
  "Diagnose what's intercepting single-key bindings in special modes."
  (interactive)
  (with-current-buffer (get-buffer-create "*Key Binding Diagnosis*")
    (erase-buffer)
    (insert "=== Key Binding Conflict Diagnosis ===\n\n")
    
    ;; Check various mode states
    (insert (format "1. Current major mode: %s\n" major-mode))
    (insert (format "2. CUA mode: %s\n" (if cua-mode "ENABLED" "disabled")))
    (insert (format "3. God mode loaded: %s\n" (if (featurep 'god-mode) "yes" "no")))
    (when (featurep 'god-mode)
      (insert (format "   God mode active: %s\n" (if (bound-and-true-p god-local-mode) "YES" "no"))))
    (insert (format "4. Buffer read-only: %s\n" buffer-read-only))
    (insert (format "5. Overriding keymaps active: %s\n" 
                   (or overriding-terminal-local-map
                       overriding-local-map
                       "none")))
    
    ;; Check specific problematic keys
    (insert "\n6. Key binding lookups for common single keys:\n")
    (dolist (key '("j" "k" "r" "u" "g" "m" "s" "a" "t"))
      (let* ((key-seq (kbd key))
             (global-binding (lookup-key global-map key-seq))
             (local-binding (lookup-key (current-local-map) key-seq))
             (minor-binding (minor-mode-key-binding key-seq)))
        (insert (format "   %s: " key))
        (when local-binding
          (insert (format "local=%s " local-binding)))
        (when minor-binding
          (insert (format "minor=%s " minor-binding)))
        (when (and global-binding (not (eq global-binding 'self-insert-command)))
          (insert (format "global=%s" global-binding)))
        (insert "\n")))
    
    ;; Check active minor modes
    (insert "\n7. Active minor modes:\n")
    (dolist (mode minor-mode-list)
      (when (and (boundp mode) (symbol-value mode))
        (insert (format "   - %s\n" mode))))
    
    ;; Check CUA key behavior
    (insert "\n8. CUA mode special behavior:\n")
    (insert (format "   CUA keys active: %s\n" 
                   (if (and cua-mode mark-active) "YES (selection active)" "no")))
    (insert (format "   CUA rectangle mode: %s\n" 
                   (if (bound-and-true-p cua--rectangle) "YES" "no")))
    
    (display-buffer (current-buffer))
    (switch-to-buffer (current-buffer))))

(global-set-key (kbd "C-c C-d k") 'diagnose-key-conflicts)

;;; Anzu - show match information in mode line
(use-package anzu
  :ensure t
  :defer t
  :config
  (global-anzu-mode 1)
  :bind (([remap query-replace] . anzu-query-replace)
         ([remap query-replace-regexp] . anzu-query-replace-regexp)))

;;; Avy - jump to visible text
(use-package avy
  :ensure t
  :defer t
  :bind (("C-:" . avy-goto-char)
         ("C-'" . avy-goto-char-2)
         ("M-g f" . avy-goto-line)
         ("M-g w" . avy-goto-word-1)))

;;; Olivetti - distraction-free writing
(use-package olivetti
  :ensure t
  :defer t
  :commands olivetti-mode
  :config
  (setq olivetti-body-width 100))

;;; Rainbow delimiters - colorize matching brackets
(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

;;; God-mode configuration (disabled by default)
;; Uncomment the following lines to enable god-mode
;; (let ((god-config (expand-file-name "god-mode-config.el" user-emacs-directory)))
;;   (when (file-exists-p god-config)
;;     (condition-case err
;;         (load-file god-config)
;;       (error (message "Failed to load god-mode config: %s" err)))))

(provide 'init-editor)
;;; init-editor.el ends here