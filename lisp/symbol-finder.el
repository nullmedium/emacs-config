;;; symbol-finder.el --- Jump to symbol definitions using Python symbol finder  -*- lexical-binding: t; -*-

;;; Commentary:
;; This package provides integration with the Python symbol_finder.py tool
;; for quickly jumping to symbol definitions in C++ and QML files.
;; 
;; Usage:
;; 1. Add this file to your Emacs load path
;; 2. Add (require 'symbol-finder) to your init.el
;; 3. Optionally customize symbol-finder-python-script path
;; 4. Use M-. to jump to definition (or customize keybinding)

;;; Code:

(defgroup symbol-finder nil
  "Jump to symbol definitions using Python symbol finder."
  :group 'tools)

(defcustom symbol-finder-python-script
  (expand-file-name "symbol_finder.py"
                    (file-name-directory (or load-file-name buffer-file-name)))
  "Path to the symbol_finder.py script."
  :type 'file
  :group 'symbol-finder)

(defcustom symbol-finder-root-directory nil
  "Root directory for symbol indexing. If nil, use project root or default-directory."
  :type '(choice (const :tag "Auto-detect" nil)
                 (directory :tag "Directory"))
  :group 'symbol-finder)

(defcustom symbol-finder-cache-directory ".symbol_cache"
  "Directory name for symbol cache (relative to root or absolute path)."
  :type 'string
  :group 'symbol-finder)

(defcustom symbol-finder-use-absolute-cache nil
  "If non-nil, treat cache-directory as an absolute path."
  :type 'boolean
  :group 'symbol-finder)

(defcustom symbol-finder-auto-index t
  "Whether to automatically index files when cache is missing."
  :type 'boolean
  :group 'symbol-finder)

(defvar symbol-finder--history nil
  "History of searched symbols.")

(defvar symbol-finder--marker-ring (make-ring 20)
  "Ring of markers for jumping back.")

(defun symbol-finder--get-root ()
  "Get the root directory for symbol operations."
  (or symbol-finder-root-directory
      (when (fboundp 'project-root)
        (and (project-current)
             (project-root (project-current))))
      default-directory))

(defun symbol-finder--get-cache-dir ()
  "Get the cache directory path."
  (if (or symbol-finder-use-absolute-cache
          (file-name-absolute-p symbol-finder-cache-directory))
      symbol-finder-cache-directory
    (expand-file-name symbol-finder-cache-directory (symbol-finder--get-root))))

(defun symbol-finder--run-command (args)
  "Run symbol_finder.py with ARGS and return output."
  (let* ((root-dir (symbol-finder--get-root))
         (cache-dir (symbol-finder--get-cache-dir))
         (default-directory root-dir)
         (cmd (format "python3 %s --root %s --cache-dir %s %s"
                      (shell-quote-argument symbol-finder-python-script)
                      (shell-quote-argument root-dir)
                      (shell-quote-argument cache-dir)
                      args)))
    (shell-command-to-string cmd)))

(defun symbol-finder-index (&optional force)
  "Index all source files. With prefix arg FORCE, force reindex."
  (interactive "P")
  (message "Indexing files...")
  (let* ((args (if force "--index --force" "--index"))
         (output (symbol-finder--run-command args)))
    (message "%s" (string-trim output))))

(defun symbol-finder--parse-emacs-output (output)
  "Parse Emacs-formatted output from symbol finder."
  (let ((lines (split-string output "\n" t))
        results)
    (dolist (line lines)
      (when (string-match "\\(.+?\\):\\([0-9]+\\):\\([0-9]+\\):\\(.*\\)" line)
        (push `(:file ,(match-string 1 line)
                :line ,(string-to-number (match-string 2 line))
                :column ,(string-to-number (match-string 3 line))
                :context ,(match-string 4 line))
              results)))
    (nreverse results)))

(defun symbol-finder--push-mark ()
  "Push current position to marker ring."
  (ring-insert symbol-finder--marker-ring (point-marker)))

(defun symbol-finder-jump-to-definition (&optional symbol)
  "Jump to definition of SYMBOL at point or prompt for symbol."
  (interactive)
  (message "symbol-finder-jump-to-definition called") ;; Debug
  (let* ((symbol (or symbol
                     (thing-at-point 'symbol t)
                     (read-string "Symbol: " nil 'symbol-finder--history)))
         (cmd (format "--definition %s --emacs" (shell-quote-argument symbol)))
         (output (symbol-finder--run-command cmd)))
    (message "Command: python3 %s %s" symbol-finder-python-script cmd) ;; Debug
    (message "Output: %s" output) ;; Debug
    (let ((results (symbol-finder--parse-emacs-output output)))
      (cond
       ((null results)
        (message "No definition found for '%s'" symbol))
       ((= 1 (length results))
        (let ((result (car results)))
          (symbol-finder--push-mark)
          (find-file (plist-get result :file))
          (goto-char (point-min))
          (forward-line (1- (plist-get result :line)))
          (move-to-column (1- (plist-get result :column)))
          (pulse-momentary-highlight-one-line (point))))
       (t
        (symbol-finder--select-and-jump results symbol))))))

(defun symbol-finder-find-references (&optional symbol)
  "Find all references to SYMBOL at point or prompt for symbol."
  (interactive)
  (let* ((symbol (or symbol
                     (thing-at-point 'symbol t)
                     (read-string "Find references to: " nil 'symbol-finder--history)))
         (output (symbol-finder--run-command
                  (format "--references %s --emacs" (shell-quote-argument symbol)))))
    (with-current-buffer (get-buffer-create "*Symbol References*")
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert output)
        (grep-mode)
        (goto-char (point-min)))
      (display-buffer (current-buffer)))))

(defun symbol-finder-find-symbol (&optional exact)
  "Find symbol by name. With prefix arg EXACT, use exact match."
  (interactive "P")
  (let* ((initial (thing-at-point 'symbol t))
         (symbol (read-string (format "Find symbol%s: " (if exact " (exact)" ""))
                              initial 'symbol-finder--history))
         (args (format "--find %s --emacs %s"
                       (shell-quote-argument symbol)
                       (if exact "--exact" "")))
         (output (symbol-finder--run-command args))
         (results (symbol-finder--parse-emacs-output output)))
    (cond
     ((null results)
      (message "No symbols found matching '%s'" symbol))
     ((= 1 (length results))
      (let ((result (car results)))
        (symbol-finder--push-mark)
        (find-file (plist-get result :file))
        (goto-char (point-min))
        (forward-line (1- (plist-get result :line)))
        (pulse-momentary-highlight-one-line (point))))
     (t
      (symbol-finder--select-and-jump results symbol)))))

(defun symbol-finder--select-and-jump (results symbol)
  "Let user select from RESULTS and jump to selected SYMBOL."
  (let* ((choices (mapcar (lambda (r)
                            (format "%s:%d: %s"
                                    (file-name-nondirectory (plist-get r :file))
                                    (plist-get r :line)
                                    (plist-get r :context)))
                          results))
         (choice (completing-read (format "Select %s: " symbol) choices nil t))
         (index (cl-position choice choices :test 'equal)))
    (when index
      (let ((result (nth index results)))
        (symbol-finder--push-mark)
        (find-file (plist-get result :file))
        (goto-char (point-min))
        (forward-line (1- (plist-get result :line)))
        (pulse-momentary-highlight-one-line (point))))))

(defun symbol-finder-cache-status ()
  "Show cache status and statistics."
  (interactive)
  (let* ((root-dir (symbol-finder--get-root))
         (output (symbol-finder--run-command "--stats")))
    (with-current-buffer (get-buffer-create "*Symbol Cache Status*")
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert "Symbol Finder Cache Status\n")
        (insert "==========================\n\n")
        (insert (format "Root directory: %s\n" root-dir))
        (insert (format "Python script: %s\n\n" symbol-finder-python-script))
        (insert output)
        (goto-char (point-min)))
      (display-buffer (current-buffer)))))

(defun symbol-finder-diagnose ()
  "Diagnose symbol-finder setup and keybindings."
  (interactive)
  (let ((global-binding (global-key-binding (kbd "M-.")))
        (local-binding (local-key-binding (kbd "M-.")))
        (mode-binding (and (boundp 'symbol-finder-mode-map)
                           (lookup-key symbol-finder-mode-map (kbd "M-.")))))
    (message "=== Symbol-Finder Diagnostic ===")
    (message "Symbol-finder-mode: %s" (if symbol-finder-mode "ON" "OFF"))
    (message "Python script: %s" (if (file-exists-p symbol-finder-python-script) 
                                      "FOUND" "NOT FOUND"))
    (message "M-. global binding: %s" global-binding)
    (message "M-. local binding: %s" local-binding)
    (message "M-. mode binding: %s" mode-binding)
    (message "Active minor modes: %s" (mapcar 'car minor-mode-alist))
    (when (eq global-binding 'xref-find-definitions)
      (message "NOTE: M-. is bound to xref. You may want to use symbol-finder-setup-override-xref"))))

(defun symbol-finder-setup-override-xref ()
  "Override xref M-. binding with symbol-finder in current buffer."
  (interactive)
  (local-set-key (kbd "M-.") 'symbol-finder-jump-to-definition)
  (local-set-key (kbd "M-,") 'symbol-finder-pop-mark)
  (local-set-key (kbd "M-?") 'symbol-finder-find-references)
  (message "M-. now bound to symbol-finder-jump-to-definition in this buffer"))

(defun symbol-finder-pop-mark ()
  "Pop back to previous position in marker ring."
  (interactive)
  (if (ring-empty-p symbol-finder--marker-ring)
      (message "No previous position")
    (let ((marker (ring-remove symbol-finder--marker-ring 0)))
      (switch-to-buffer (marker-buffer marker))
      (goto-char marker))))

(defun symbol-finder-update-file ()
  "Update index for current file."
  (interactive)
  (when buffer-file-name
    (message "Updating index for %s..." buffer-file-name)
    (let* ((args (format "--index --force --root %s"
                         (shell-quote-argument (file-name-directory buffer-file-name))))
           (output (symbol-finder--run-command args)))
      (message "Index updated"))))

;; Auto-update on save
(defun symbol-finder--after-save-hook ()
  "Hook to update index after saving."
  (when (and buffer-file-name
             (string-match-p "\\.\\(cpp\\|cc\\|cxx\\|c\\+\\+\\|hpp\\|h\\|hh\\|hxx\\|h\\+\\+\\|qml\\|js\\)$"
                             buffer-file-name))
    (symbol-finder-update-file)))

(defcustom symbol-finder-auto-update-on-save nil
  "Whether to automatically update index on file save."
  :type 'boolean
  :group 'symbol-finder
  :set (lambda (symbol value)
         (set-default symbol value)
         (if value
             (add-hook 'after-save-hook 'symbol-finder--after-save-hook)
           (remove-hook 'after-save-hook 'symbol-finder--after-save-hook))))

;; Minor mode for keybindings
(defvar symbol-finder-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "M-.") 'symbol-finder-jump-to-definition)
    (define-key map (kbd "M-?") 'symbol-finder-find-references)
    (define-key map (kbd "M-,") 'symbol-finder-pop-mark)
    (define-key map (kbd "C-c s f") 'symbol-finder-find-symbol)
    (define-key map (kbd "C-c s i") 'symbol-finder-index)
    (define-key map (kbd "C-c s u") 'symbol-finder-update-file)
    map)
  "Keymap for symbol-finder-mode.")

;;;###autoload
(define-minor-mode symbol-finder-mode
  "Minor mode for symbol navigation using Python symbol finder."
  :lighter " SymF"
  :keymap symbol-finder-mode-map
  :group 'symbol-finder
  (when symbol-finder-mode
    ;; Debug message
    (message "Symbol-finder-mode activated. M-. bound to: %s" 
             (lookup-key symbol-finder-mode-map (kbd "M-.")))
    (when (and symbol-finder-auto-index
               (not (file-exists-p
                     (expand-file-name symbol-finder-cache-directory
                                       (symbol-finder--get-root)))))
      (when (y-or-n-p "No symbol cache found. Index files now?")
        (symbol-finder-index)))))

;;;###autoload
(define-globalized-minor-mode global-symbol-finder-mode
  symbol-finder-mode
  (lambda ()
    (when (and (not (minibufferp))
               (string-match-p "\\.\\(cpp\\|cc\\|cxx\\|c\\+\\+\\|hpp\\|h\\|hh\\|hxx\\|h\\+\\+\\|qml\\|js\\)$"
                               (or buffer-file-name "")))
      (symbol-finder-mode 1))))

;; Compatibility with xref (optional)
(when (fboundp 'xref-make)
  (defun symbol-finder-xref-backend ()
    "Symbol finder backend for xref."
    'symbol-finder)
  
  (cl-defmethod xref-backend-identifier-at-point ((_backend (eql symbol-finder)))
    (thing-at-point 'symbol t))
  
  (cl-defmethod xref-backend-definitions ((_backend (eql symbol-finder)) identifier)
    (let* ((output (symbol-finder--run-command
                    (format "--definition %s --emacs" (shell-quote-argument identifier))))
           (results (symbol-finder--parse-emacs-output output)))
      (mapcar (lambda (r)
                (xref-make (plist-get r :context)
                           (xref-make-file-location (plist-get r :file)
                                                     (plist-get r :line)
                                                     (1- (plist-get r :column)))))
              results)))
  
  (cl-defmethod xref-backend-references ((_backend (eql symbol-finder)) identifier)
    (let* ((output (symbol-finder--run-command
                    (format "--references %s --emacs" (shell-quote-argument identifier))))
           (results (symbol-finder--parse-emacs-output output)))
      (mapcar (lambda (r)
                (xref-make (plist-get r :context)
                           (xref-make-file-location (plist-get r :file)
                                                     (plist-get r :line)
                                                     (1- (plist-get r :column)))))
              results)))
  
  (add-hook 'symbol-finder-mode-hook
            (lambda ()
              (add-hook 'xref-backend-functions 'symbol-finder-xref-backend nil t))))

(provide 'symbol-finder)
;;; symbol-finder.el ends here