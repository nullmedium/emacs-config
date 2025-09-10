;;; init-project.el --- Project management configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; Built-in project.el configuration (replaces Projectile)

;;; Code:

(require 'project)

;; Add additional project root markers
(setq project-vc-extra-root-markers 
      '(".projectile"           ; Projectile marker
        ".project"              ; Generic project marker
        "Makefile"              ; Make projects
        "package.json"          ; Node.js projects
        "Cargo.toml"            ; Rust projects
        "go.mod"                ; Go modules
        "pom.xml"               ; Maven projects
        "build.gradle"          ; Gradle projects
        "requirements.txt"      ; Python projects
        "setup.py"              ; Python packages
        "pyproject.toml"        ; Modern Python projects
        "Gemfile"               ; Ruby projects
        "composer.json"         ; PHP projects
        ".git"                  ; Git repositories
        ".hg"                   ; Mercurial
        ".svn"))                ; SVN

;; Configure project.el behavior
(setq project-switch-commands
      '((project-find-file "Find file" ?f)
        (project-find-regexp "Grep" ?g)
        (project-dired "Dired" ?d)
        (project-vc-dir "VC Dir" ?v)
        (project-eshell "Eshell" ?e)
        (project-shell "Shell" ?s)
        (project-compile "Compile" ?c)
        (magit-project-status "Magit" ?m)))

;; Better project switching
(setq project-switch-use-entire-map t)

;; Cache project list
(setq project-list-file (expand-file-name "projects" user-emacs-directory))

;; Custom functions for compatibility with old Projectile workflow
(defun my/project-find-file ()
  "Find file in current project."
  (interactive)
  (project-find-file))

(defun my/project-switch-project ()
  "Switch to another project."
  (interactive)
  (project-switch-project))

(defun my/project-grep ()
  "Grep in current project."
  (interactive)
  (project-find-regexp))

(defun my/project-dired ()
  "Open project root in dired."
  (interactive)
  (let ((project (project-current)))
    (if project
        (dired (project-root project))
      (error "No project found"))))

(defun my/project-compile ()
  "Compile project."
  (interactive)
  (project-compile))

(defun my/project-run-shell ()
  "Start shell in project root."
  (interactive)
  (project-shell))

(defun my/project-kill-buffers ()
  "Kill all project buffers."
  (interactive)
  (project-kill-buffers))

;; Add project discovery for non-VC directories
(defun my/project-try-local (dir)
  "Try to find project root markers in DIR."
  (let ((root (locate-dominating-file
               dir
               (lambda (d)
                 (seq-some
                  (lambda (marker)
                    (file-exists-p (expand-file-name marker d)))
                  project-vc-extra-root-markers)))))
    (when root
      (cons 'transient root))))

(add-to-list 'project-find-functions #'my/project-try-local)

;; Integration with consult if available
(with-eval-after-load 'consult
  (setq consult-project-function #'project-root))

;; Keybindings - Main project map on C-x p (built-in)
;; Additional compatibility bindings for muscle memory
(global-set-key (kbd "C-c p f") #'project-find-file)
(global-set-key (kbd "C-c p p") #'project-switch-project)
(global-set-key (kbd "C-c p g") #'project-find-regexp)
(global-set-key (kbd "C-c p d") #'my/project-dired)
(global-set-key (kbd "C-c p c") #'project-compile)
(global-set-key (kbd "C-c p s") #'project-shell)
(global-set-key (kbd "C-c p k") #'project-kill-buffers)
(global-set-key (kbd "C-c p b") #'project-switch-to-buffer)
(global-set-key (kbd "C-c p r") #'project-query-replace-regexp)

;; Keep existing dired bindings
(global-set-key (kbd "C-c d") #'dired-jump)
(global-set-key (kbd "C-c D") #'my/project-dired)

(provide 'init-project)
;;; init-project.el ends here