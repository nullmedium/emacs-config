;;; init-treemacs.el --- Treemacs configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; File tree configuration with Treemacs

;;; Code:

(use-package treemacs
  :ensure t
  :defer t
  :commands (treemacs treemacs-select-window)
  :bind (("M-0"       . treemacs-select-window)
         ("C-c T t"   . treemacs)
         ("C-c T 1"   . treemacs-delete-other-windows)
         ("C-c T d"   . treemacs-select-directory)
         ("C-c T B"   . treemacs-bookmark)
         ("<f8>"      . treemacs)
         ("C-c T f"   . treemacs-toggle-and-focus)
         ("C-c T s"   . treemacs-search-file)
         :map treemacs-mode-map
         ("/" . treemacs-search-file)
         ("C-s" . projectile-find-file)
         ("s" . consult-ripgrep))
  :config
  (setq treemacs-collapse-dirs                   (if treemacs-python-executable 3 0)
        treemacs-deferred-git-apply-delay        0.5
        treemacs-directory-name-transformer      #'identity
        treemacs-display-in-side-window          t
        treemacs-eldoc-display                   'simple
        treemacs-file-event-delay                2000
        treemacs-file-follow-delay                0.2
        treemacs-follow-after-init                t
        treemacs-expand-after-init               t
        treemacs-find-workspace-method           'find-for-file-or-pick-first
        treemacs-hide-dot-git-directory          t
        treemacs-indentation                     2
        treemacs-indentation-string              " "
        treemacs-is-never-other-window           nil
        treemacs-max-git-entries                  5000
        treemacs-missing-project-action          'ask
        treemacs-move-forward-on-expand          nil
        treemacs-no-delete-other-windows         t
        treemacs-persist-file                    (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
        treemacs-position                        'left
        treemacs-recenter-distance               0.1
        treemacs-recenter-after-file-follow      nil
        treemacs-recenter-after-tag-follow       nil
        treemacs-recenter-after-project-jump     'always
        treemacs-recenter-after-project-expand   'on-distance
        treemacs-show-cursor                     nil
        treemacs-show-hidden-files               t
        treemacs-silent-filewatch                nil
        treemacs-silent-refresh                  nil
        treemacs-sorting                         'alphabetic-asc
        treemacs-space-between-root-nodes        t
        treemacs-tag-follow-cleanup              t
        treemacs-tag-follow-delay                1.5
        treemacs-width                           35
        treemacs-width-is-initially-locked       t
        treemacs-workspace-switch-cleanup        nil)

  (treemacs-follow-mode t)
  (treemacs-filewatch-mode t)
  (treemacs-fringe-indicator-mode 'always)
  (when treemacs-python-executable
    (treemacs-git-commit-diff-mode t))

  (pcase (cons (not (null (executable-find "git")))
               (not (null treemacs-python-executable)))
    (`(t . t) (treemacs-git-mode 'deferred))
    (`(t . _) (treemacs-git-mode 'simple)))

  (treemacs-hide-gitignored-files-mode nil))

(use-package treemacs-projectile
  :ensure t
  :after (treemacs projectile)
  :defer t)

(use-package treemacs-all-the-icons
  :ensure t
  :after (treemacs all-the-icons)
  :defer t
  :config
  (treemacs-load-theme "all-the-icons"))

;; Treemacs helper functions
(defun treemacs-toggle-and-focus ()
  "Toggle treemacs and focus on it if it's visible."
  (interactive)
  (if (treemacs-get-local-window)
      (treemacs-toggle)
    (progn
      (treemacs)
      (treemacs-select-window))))

(defun treemacs-search-file ()
  "Search for a file in the current project using consult."
  (interactive)
  (if (fboundp 'projectile-find-file)
      (projectile-find-file)
    (consult-find)))

(defun treemacs-open-marked-files ()
  "Open all marked files in treemacs."
  (interactive)
  (when (eq major-mode 'treemacs-mode)
    (treemacs-bulk-file-actions
     :actions '(treemacs-visit-node-no-split))))

(defun treemacs-mark-visible-files ()
  "Mark all visible files in the current directory."
  (interactive)
  (when (eq major-mode 'treemacs-mode)
    (save-excursion
      (treemacs-goto-parent-node)
      (treemacs-TAB-action)
      (forward-line 1)
      (while (and (not (eobp))
                  (> (treemacs--get-depth-of-item) 0))
        (when (treemacs-is-node-file?)
          (treemacs-do-mark))
        (forward-line 1)))))

(provide 'init-treemacs)
;;; init-treemacs.el ends here