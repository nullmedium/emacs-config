;;; init-core.el --- Core settings and package management -*- lexical-binding: t -*-
;;; Commentary:
;;; Core Emacs settings, package management, and fundamental configurations

;;; Code:

;;; Package Management
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; Core package list - consolidated and cleaned
(setq package-selected-packages
      '(;; Core utilities
        use-package which-key

        ;; Themes
        modus-themes

        ;; Version control
        diff-hl

        ;; File management
        treemacs treemacs-all-the-icons
        neotree all-the-icons all-the-icons-dired diredfl

        ;; Modern completion ecosystem (replaces Helm)
        vertico         ; Vertical completion UI
        consult         ; Practical commands based on completing-read
        orderless       ; Powerful completion style
        marginalia      ; Rich annotations in minibuffer
        embark          ; Contextual actions
        embark-consult  ; Embark integration with Consult
        corfu           ; In-buffer completion popup
        cape            ; Completion extensions for Corfu

        ;; Markdown & Notes
        markdown-mode markdown-toc grip-mode
        obsidian olivetti

        ;; Search and navigation
        deadgrep ripgrep wgrep anzu
        ibuffer-sidebar

        ;; Required for some functionality
        org dash s f ht spinner lv hydra avy

        ;; RSS/News reader
        elfeed elfeed-org

        ;; Email (mu4e installed separately)

        ;; Finance tracking
        beancount

        ;; God mode for modal editing
        god-mode
        
        ;; Quality of life improvements
        helpful          ; Better help buffers
        undo-tree        ; Visual undo history
        smartparens      ; Better than electric-pair-mode
        crux             ; Collection of useful functions
        ace-window       ; Quick window switching
        
        ;; Terminal emulator
        eat              ; Emacs terminal emulator
        ))

;; Auto-install missing packages
(when (cl-find-if-not #'package-installed-p package-selected-packages)
  (package-refresh-contents)
  (mapc #'package-install package-selected-packages))

;; Ensure use-package is loaded
(eval-when-compile
  (require 'use-package))
(setq use-package-always-ensure t)

;;; Performance Optimizations
;; Reset GC threshold after init for better runtime performance
(add-hook 'emacs-startup-hook
          (lambda ()
            ;; Use the defaults we stored in early-init.el
            (setq gc-cons-threshold (if (boundp 'default-gc-cons-threshold)
                                        default-gc-cons-threshold
                                        16777216)  ; 16MB
                  gc-cons-percentage (if (boundp 'default-gc-cons-percentage)
                                         default-gc-cons-percentage
                                         0.1))
            (message "GC threshold reset to %s" gc-cons-threshold))
          50)  ; Run after most other startup hooks

(setq read-process-output-max (* 1024 1024))

;;; General Settings
(setq-default indent-tabs-mode nil)
(setq tab-width 4)
(setq inhibit-startup-screen t)
(setq ring-bell-function 'ignore)

;; Auto-save and backup settings
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))
(setq delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)
(setq auto-save-file-name-transforms
      '((".*" "~/.emacs.d/auto-save-list/" t)))

;; Use ls-lisp (Emacs's built-in ls emulation) for better cross-platform compatibility
(setq ls-lisp-use-insert-directory-program nil)
(setq ls-lisp-dirs-first t)

;;; Session Management
(save-place-mode 1)
(setq save-place-file "~/.emacs.d/saveplace")

(savehist-mode 1)
(setq savehist-file "~/.emacs.d/savehist")
(setq history-length 1000)

(recentf-mode 1)
(setq recentf-max-menu-items 50)
(setq recentf-max-saved-items 200)

;; Desktop save mode - with lazy loading for better performance
;; Defer desktop restoration to speed up initial startup
(setq desktop-restore-eager 5)  ; Restore only 5 buffers initially
(setq desktop-lazy-verbose nil)
(setq desktop-lazy-idle-delay 1)  ; Restore rest after 1 second idle

;; Enable desktop-save-mode after startup (unless disabled via environment)
(add-hook 'after-init-hook
          (lambda ()
            ;; Check if desktop mode should be disabled (for helper scripts)
            (unless (getenv "EMACS_NO_DESKTOP")
              (desktop-save-mode 1)
              (setq desktop-save t)
              (setq desktop-auto-save-timeout 300)
              (setq desktop-path '("~/.emacs.d/"))
              (setq desktop-dirname "~/.emacs.d/")
              (setq desktop-base-file-name "emacs-desktop")
              (setq desktop-restore-frames t)
              ;; Load desktop after a delay
              (run-with-idle-timer 2 nil
                                   (lambda ()
                                     (desktop-read)
                                     (message "Desktop restored"))))))

(provide 'init-core)
;;; init-core.el ends here