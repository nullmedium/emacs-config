;;; init-completion.el --- Modern completion configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; Configuration for Vertico, Consult, Orderless, Marginalia, and Embark
;;; This replaces Helm with a lighter, faster, more native completion system

;;; Code:

;;; Vertico - Vertical completion UI
(use-package vertico
  :ensure t
  :init
  (vertico-mode)
  :config
  ;; Different scroll margin
  (setq vertico-scroll-margin 0)
  ;; Show more candidates
  (setq vertico-count 15)
  ;; Grow and shrink the Vertico minibuffer
  (setq vertico-resize t)
  ;; Enable cycling
  (setq vertico-cycle t))

;;; Savehist - Persist history over Emacs restarts
(use-package savehist
  :ensure nil
  :init
  (savehist-mode))

;;; Orderless - Powerful completion style
(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

;;; Marginalia - Rich annotations in minibuffer
(use-package marginalia
  :ensure t
  :init
  (marginalia-mode)
  :bind (:map minibuffer-local-map
         ("M-A" . marginalia-cycle)))

;;; Consult - Practical commands based on completing-read
(use-package consult
  :ensure t
  :bind (;; C-c bindings (mode-specific-map)
         ("C-c h" . consult-history)
         ("C-c m" . consult-mode-command)
         ("C-c k" . consult-kmacro)
         ;; C-x bindings (ctl-x-map)
         ("C-x M-:" . consult-complex-command)
         ("C-x b" . consult-buffer)
         ("C-x 4 b" . consult-buffer-other-window)
         ("C-x 5 b" . consult-buffer-other-frame)
         ("C-x r b" . consult-bookmark)
         ("C-x p b" . consult-project-buffer)
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)
         ;; M-g bindings (goto-map)
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)
         ("M-g g" . consult-goto-line)
         ("M-g M-g" . consult-goto-line)
         ("M-g o" . consult-outline)
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings (search-map)
         ("M-s d" . consult-find)
         ("M-s D" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)
         ("M-s e" . consult-isearch-history)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)
         ("M-r" . consult-history))

  ;; Enable automatic preview at point in the *Completions* buffer.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  :init
  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  :config
  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  (setq consult-preview-key 'any)
  
  ;; Configure other variables and modes
  (setq consult-narrow-key "<") ;; "C-+"

  ;; Make narrowing help available in the minibuffer.
  (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)

  ;; By default `consult-project-function' uses `project-root' from project.el.
  ;; Configure a different project root function.
  (autoload 'projectile-project-root "projectile")
  (setq consult-project-function (lambda (_) (projectile-project-root))))

;;; Embark - Contextual actions
(use-package embark
  :ensure t
  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("C-;" . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'

  :init
  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)

  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

;;; Embark-Consult - Integration between Embark and Consult
(use-package embark-consult
  :ensure t
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

;;; Company - In-buffer completion framework
(use-package company
  :ensure t
  :diminish company-mode
  :hook (after-init . global-company-mode)
  :custom
  (company-idle-delay 0.2)
  (company-minimum-prefix-length 2)
  (company-show-numbers t)
  (company-tooltip-align-annotations t)
  (company-tooltip-flip-when-above t)
  (company-require-match nil)
  (company-dabbrev-downcase nil)
  (company-dabbrev-ignore-case nil)
  (company-selection-wrap-around t)
  (company-transformers '(company-sort-by-occurrence))
  
  :config
  ;; Use Tab and Shift-Tab to navigate completions
  (define-key company-active-map (kbd "TAB") 'company-complete-selection)
  (define-key company-active-map (kbd "<tab>") 'company-complete-selection)
  (define-key company-active-map (kbd "S-TAB") 'company-select-previous)
  (define-key company-active-map (kbd "<backtab>") 'company-select-previous)
  
  ;; Use C-n and C-p for navigation as well
  (define-key company-active-map (kbd "C-n") 'company-select-next)
  (define-key company-active-map (kbd "C-p") 'company-select-previous)
  
  ;; Disable conflicting bindings
  (define-key company-active-map (kbd "RET") nil)
  (define-key company-active-map (kbd "<return>") nil)
  
  ;; Complete with Enter
  (define-key company-active-map (kbd "RET") 'company-complete-selection)
  (define-key company-active-map (kbd "<return>") 'company-complete-selection)
  
  ;; Configure backends
  (setq company-backends
        '((company-capf company-files)
          (company-dabbrev-code company-keywords)
          company-dabbrev)))

;;; Company-box - Better UI for Company (optional, may not be available)
;; Commented out as company-box is not always available in package repos
;; Uncomment if you want to try installing it manually
;; (use-package company-box
;;   :ensure t
;;   :hook (company-mode . company-box-mode)
;;   :custom
;;   (company-box-show-single-candidate t)
;;   (company-box-backends-colors nil)
;;   (company-box-max-candidates 50)
;;   (company-box-icons-alist 'company-box-icons-all-the-icons)
;;   :config
;;   ;; Workaround for font/display issues
;;   (setq company-box-doc-enable nil)) ;; Disable doc popup to avoid display issues

;;; Additional Consult commands for enhanced functionality
(defun consult-ripgrep-project-root ()
  "Search project root with ripgrep."
  (interactive)
  (let ((root (or (projectile-project-root) default-directory)))
    (consult-ripgrep root)))

;; Quick access to ripgrep - C-c r for backward compatibility
(global-set-key (kbd "C-c r") 'consult-ripgrep-project-root)
;; Additional quick binding for project search
(global-set-key (kbd "C-c /") 'consult-ripgrep-project-root)

;;; Make completion work nicely with Projectile
(with-eval-after-load 'projectile
  (define-key projectile-command-map (kbd "b") #'consult-project-buffer)
  (define-key projectile-command-map (kbd "r") #'consult-ripgrep))

(provide 'init-completion)
;;; init-completion.el ends here