;;; init-performance.el --- Performance optimizations and fixes -*- lexical-binding: t -*-
;;; Commentary:
;;; Comprehensive performance optimizations to prevent UI freezing, lag, and other issues

;;; Code:

;;;; Garbage Collection Optimizations
;; Increase garbage collection threshold during startup
(setq gc-cons-threshold (* 100 1024 1024))  ; 100MB
(setq gc-cons-percentage 0.6)

;; Reset after startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 16 1024 1024))  ; 16MB
            (setq gc-cons-percentage 0.1)))

;;;; File Watching and Auto-revert Optimizations
(setq auto-revert-interval 5)  ; Check every 5 seconds instead of 1
(setq auto-revert-use-notify t)  ; Use file system notifications
(setq auto-revert-avoid-polling t)  ; Don't poll, use notifications
(setq global-auto-revert-non-file-buffers nil)  ; Don't auto-revert non-file buffers

;;;; Display and Rendering Optimizations
(setq idle-update-delay 2.0)  ; Default is 0.5
(setq redisplay-dont-pause t)  ; Never pause redisplay
(setq fast-but-imprecise-scrolling t)  ; Faster scrolling
(setq inhibit-compacting-font-caches t)  ; Don't compact font caches during GC

;; Font-lock optimizations
(setq jit-lock-defer-time 0.05)  ; Slightly defer font-locking
(setq jit-lock-stealth-time 5)  ; Wait 5 seconds before stealth fontification
(setq jit-lock-chunk-size 1000)  ; Smaller chunks
(setq jit-lock-stealth-load 20)  ; Don't fontify when load is high

;; Disable bidirectional text rendering for performance
(setq-default bidi-display-reordering nil)
(setq-default bidi-paragraph-direction 'left-to-right)

;; Optimize long line handling
(setq-default truncate-lines t)
(setq line-move-visual nil)

;;;; Memory and Process Optimizations
(setq undo-limit 80000)  ; Default is 160000
(setq undo-strong-limit 120000)  ; Default is 240000
(setq undo-outer-limit 12000000)  ; Default is 24000000
(setq read-process-output-max (* 1024 1024))  ; 1MB, default is 4096

;;;; X11 Specific Optimizations (for no-toolkit builds)
(when (and (display-graphic-p)
           (not (memq window-system '(ns mac w32))))
  ;; More aggressive redrawing for X11
  (setq idle-update-delay 0.1)  ; Faster idle updates
  (setq redisplay-skip-fontification-on-input nil))  ; Don't skip font-lock

;;;; Timer Management Functions
(defun clear-duplicate-idle-timers ()
  "Remove duplicate idle timers that may be causing performance issues."
  (let ((seen-timers '()))
    (dolist (timer timer-idle-list)
      (let ((timer-func (timer--function timer)))
        (if (member timer-func seen-timers)
            (cancel-timer timer)
          (push timer-func seen-timers))))))

(defun disable-qml-timers ()
  "Disable QML idle timers."
  (dolist (timer timer-idle-list)
    (when (and (timer--function timer)
               (eq (timer--function timer) 'qml-timer-handler))
      (cancel-timer timer))))

;; Run timer cleanup on load
(clear-duplicate-idle-timers)
(disable-qml-timers)

;;;; Mode-specific Performance Fixes

;; Disable DAP mode if it's somehow enabled (causes major lag)
(when (boundp 'dap-mode)
  (dap-mode -1))
(when (boundp 'dap-ui-mode)
  (dap-ui-mode -1))
(when (boundp 'dap-auto-configure-mode)
  (dap-auto-configure-mode -1))

;; Disable LSP-UI doc if causing issues
(when (boundp 'lsp-ui-doc-mode)
  (setq lsp-ui-doc-enable nil))

;; Prevent QML timers from being created
(with-eval-after-load 'qml-mode
  (remove-hook 'qml-mode-hook 'qml-start-timer-handler)
  (fset 'qml-timer-handler 'ignore)
  (fset 'qml-start-timer-handler 'ignore))

;; Prevent LSP from activating in QML files
(with-eval-after-load 'lsp-mode
  ;; Remove QML mode from LSP's activation list
  (setq lsp--major-modes-for-activate 
        (delete 'qml-mode lsp--major-modes-for-activate))
  ;; Advise lsp-deferred to skip QML files
  (defadvice lsp-deferred (around no-lsp-for-qml activate)
    "Prevent LSP from starting in QML mode."
    (unless (eq major-mode 'qml-mode)
      ad-do-it)))

;;;; Large File Handling
(defun my-large-file-hook ()
  "Disable expensive features in large files."
  (when (> (buffer-size) (* 1024 1024))  ; Files larger than 1MB
    (setq-local line-number-mode nil)
    (setq-local column-number-mode nil)
    (setq-local show-paren-mode nil)
    (setq-local font-lock-mode nil)
    (setq-local bidi-display-reordering nil)))

(defun check-large-file-performance ()
  "Disable expensive features in large files (512KB threshold)."
  (when (> (buffer-size) (* 512 1024))  ; Files larger than 512KB
    (when (bound-and-true-p rainbow-delimiters-mode)
      (rainbow-delimiters-mode -1))
    (when (bound-and-true-p diff-hl-mode)
      (diff-hl-mode -1))
    (when (bound-and-true-p undo-tree-mode)
      (undo-tree-mode -1))))

(add-hook 'find-file-hook 'my-large-file-hook)
(add-hook 'find-file-hook 'check-large-file-performance)

;; Prevent timer accumulation when killing buffers
(add-hook 'kill-buffer-hook
          (lambda ()
            (when (derived-mode-p 'qml-mode)
              (disable-qml-timers))))

;;;; Manual Performance Control Functions

(defun force-redraw ()
  "Force a complete redraw of the frame."
  (interactive)
  (redraw-frame))

(defun force-redraw-all ()
  "Force redraw of all frames and windows."
  (interactive)
  (dolist (frame (frame-list))
    (redraw-frame frame))
  (redisplay t))

(defun fix-performance-now ()
  "Fix all known performance issues immediately."
  (interactive)
  ;; Disable DAP
  (when (boundp 'dap-mode) (dap-mode -1))
  (when (boundp 'dap-ui-mode) (dap-ui-mode -1))
  (when (boundp 'dap-auto-configure-mode) (dap-auto-configure-mode -1))
  
  ;; Clear timers
  (clear-duplicate-idle-timers)
  (disable-qml-timers)
  
  ;; Force garbage collection
  (garbage-collect)
  
  (message "Performance fixes applied!"))

(defun diagnose-performance ()
  "Show information about potential performance issues."
  (interactive)
  (with-current-buffer (get-buffer-create "*Performance Diagnostic*")
    (erase-buffer)
    (insert "=== Emacs Performance Diagnostic ===\n\n")
    (insert (format "Garbage Collection Threshold: %s\n" gc-cons-threshold))
    (insert (format "Garbage Collection Percentage: %s\n" gc-cons-percentage))
    (insert (format "Auto-revert interval: %s\n" auto-revert-interval))
    (insert (format "Number of buffers: %s\n" (length (buffer-list))))
    (insert (format "Active minor modes: %s\n" 
                    (mapconcat 'symbol-name 
                               (delq nil (mapcar (lambda (m) (and (boundp m) (symbol-value m) m))
                                                 minor-mode-list))
                               ", ")))
    (insert "\n=== Active Timers ===\n")
    (dolist (timer timer-list)
      (insert (format "%s\n" timer)))
    (insert "\n=== Idle Timers ===\n")
    (dolist (timer timer-idle-list)
      (insert (format "%s\n" timer)))
    (switch-to-buffer (current-buffer))))

;;;; Keybindings for Performance Control
(global-set-key (kbd "C-c r r") 'force-redraw)
(global-set-key (kbd "C-c r a") 'force-redraw-all)
(global-set-key (kbd "C-c p f") 'fix-performance-now)
(global-set-key (kbd "C-c p d") 'diagnose-performance)

;;;; Window Configuration Hook
(add-hook 'window-configuration-change-hook
          (lambda ()
            (when (and (display-graphic-p)
                       (not (memq window-system '(ns mac w32))))
              (redisplay))))

(provide 'init-performance)
;;; init-performance.el ends here