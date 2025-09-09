;;; shr-config.el --- SHR (Simple HTML Renderer) configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Global configuration for SHR which is used by mu4e, elfeed, eww, etc.

;;; Code:

;; Configure SHR for better readability
(with-eval-after-load 'shr
  ;; Basic SHR settings
  (setq shr-use-fonts t)              ; Use variable fonts
  (setq shr-use-colors t)             ; Use colors from HTML
  (setq shr-max-image-proportion 0.7) ; Limit image size
  (setq shr-width nil)                ; Use full window width
  (setq shr-bullet "• ")              ; Nice bullet character
  (setq shr-inhibit-images nil)       ; Enable image display
  (setq shr-blocked-images nil)       ; Don't block any images
  
  ;; Increase indentation for better structure
  (setq shr-indentation 2)
  
  ;; Cookie policy
  (setq shr-cookie-policy 'same-origin))

;; Define a serif face for SHR content
(defface shr-text
  '((t :family "Georgia" :height 1.1))
  "Face for SHR body text with serif font."
  :group 'shr)

;; Override SHR faces to use serif fonts
(with-eval-after-load 'shr
  ;; Set default SHR text to use serif font
  (set-face-attribute 'shr-text nil 
                      :family "Georgia"  ; You can change to "Times New Roman", "Palatino", "Baskerville", etc.
                      :height 1.1)
  
  ;; Apply serif font to the main text
  (defun my-shr-tag-p (dom)
    "Custom paragraph handler to apply serif font."
    (shr-ensure-paragraph)
    (let ((shr-current-font 'shr-text))
      (shr-generic dom))
    (shr-ensure-paragraph))
  
  ;; Hook to apply serif font after rendering
  (defun my-shr-apply-serif-font ()
    "Apply serif font to SHR rendered content."
    (when (derived-mode-p 'eww-mode 'mu4e-view-mode 'elfeed-show-mode)
      (buffer-face-set 'shr-text)))
  
  (add-hook 'shr-after-render-hook 'my-shr-apply-serif-font))

;; Disable the fill-column indicator (red margin line) in SHR-related modes
(defun disable-fill-column-indicator ()
  "Disable the fill column indicator in the current buffer."
  (display-fill-column-indicator-mode -1))

;; Add hooks to disable fill-column indicator in SHR-using modes
(add-hook 'eww-mode-hook 'disable-fill-column-indicator)
(add-hook 'elfeed-show-mode-hook 'disable-fill-column-indicator)
(add-hook 'mu4e-view-mode-hook 'disable-fill-column-indicator)

;; Also disable it after SHR renders content
(add-hook 'shr-after-render-hook 'disable-fill-column-indicator)

;; Disable line numbers in all modes that use SHR
(defun my-shr-disable-line-numbers ()
  "Disable line numbers in SHR-rendered buffers."
  (display-line-numbers-mode -1)
  (setq-local display-line-numbers nil))

;; Apply to mu4e view mode
(with-eval-after-load 'mu4e
  (add-hook 'mu4e-view-mode-hook 'my-shr-disable-line-numbers))

;; Apply to elfeed show mode (already done in elfeed-config, but adding here for completeness)
(with-eval-after-load 'elfeed
  (add-hook 'elfeed-show-mode-hook 'my-shr-disable-line-numbers))

;; Apply to EWW mode
(with-eval-after-load 'eww
  (add-hook 'eww-mode-hook 'my-shr-disable-line-numbers))

;; Apply to any buffer after SHR renders content
(with-eval-after-load 'shr
  (add-hook 'shr-after-render-hook 'my-shr-disable-line-numbers))

;; Function to increase font size in any SHR-rendered buffer
(defun shr-increase-font-size ()
  "Increase font size in SHR-rendered content."
  (interactive)
  (text-scale-increase 1))

;; Function to decrease font size in any SHR-rendered buffer
(defun shr-decrease-font-size ()
  "Decrease font size in SHR-rendered content."
  (interactive)
  (text-scale-decrease 1))

;; Function to reset font size
(defun shr-reset-font-size ()
  "Reset font size to default."
  (interactive)
  (text-scale-set 0))

;; Auto-increase font size in specific modes that use SHR
(defun auto-increase-shr-font-size ()
  "Automatically increase font size in SHR content."
  (text-scale-set 1))  ; Increase by 1 step. Change to 2 or 3 for larger increase

;; Apply to mu4e
(with-eval-after-load 'mu4e
  (add-hook 'mu4e-view-mode-hook 'auto-increase-shr-font-size))

;; Apply to elfeed
(with-eval-after-load 'elfeed
  (add-hook 'elfeed-show-mode-hook 'auto-increase-shr-font-size))

;; Apply to EWW
(with-eval-after-load 'eww
  (add-hook 'eww-mode-hook 'auto-increase-shr-font-size))

;; Keybindings for manual font size adjustment
(with-eval-after-load 'shr
  ;; These will work in any buffer with SHR content
  (define-key shr-map (kbd "+") 'shr-increase-font-size)
  (define-key shr-map (kbd "=") 'shr-increase-font-size)
  (define-key shr-map (kbd "-") 'shr-decrease-font-size)
  (define-key shr-map (kbd "0") 'shr-reset-font-size))

(provide 'shr-config)
;;; shr-config.el ends here