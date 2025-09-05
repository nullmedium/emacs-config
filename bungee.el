;;; bungee.el --- Fast symbol navigation with Elisp-based caching  -*- lexical-binding: t; -*-

;;; Commentary:
;; Bungee provides fast symbol navigation for C++ and QML files using
;; an Elisp-based cache system. It can work standalone or with the
;; Python symbol_finder.py for indexing.

;;; Code:

(require 'json)
(require 'cl-lib)
(require 'xref nil t)
(require 'pulse nil t)
(require 'grep nil t)

(defgroup bungee nil
  "Fast symbol navigation with caching."
  :group 'tools)

(defcustom bungee-cache-directory ".symbol_cache"
  "Directory for symbol cache files."
  :type 'string
  :group 'bungee)

(defcustom bungee-python-indexer nil
  "Path to Python indexer script (optional). If nil, use Elisp indexer."
  :type '(choice (const :tag "Use Elisp indexer" nil)
                 (file :tag "Python script path"))
  :group 'bungee)

(defcustom bungee-auto-update t
  "Automatically update cache when files change."
  :type 'boolean
  :group 'bungee)

(defcustom bungee-save-json-cache nil
  "Also save cache in JSON format for Python compatibility."
  :type 'boolean
  :group 'bungee)

;; Cache data structures
(defvar bungee--symbol-cache nil
  "In-memory symbol cache. Hash table mapping file paths to symbol lists.")

(defvar bungee--index-cache nil
  "In-memory index cache. Hash table mapping file paths to modification times.")

(defvar bungee--cache-loaded nil
  "Whether the cache has been loaded from disk.")

(cl-defstruct bungee-symbol
  "A symbol in the codebase."
  name           ; Symbol name
  file-path      ; Absolute file path
  line-number    ; Line number (1-based)
  symbol-type    ; 'class, 'function, 'property, etc.
  context)       ; Line content for context

;; Cache management functions
(defun bungee--cache-dir ()
  "Get the cache directory path."
  (let ((root (or (and (fboundp 'project-current)
                        (project-current)
                        (fboundp 'project-root)
                        (ignore-errors
                          (car (project-roots (project-current)))))
                  default-directory)))
    (expand-file-name bungee-cache-directory root)))

(defun bungee--cache-file-path (filename)
  "Get path for cache FILENAME."
  (expand-file-name filename (bungee--cache-dir)))

(defun bungee--load-cache ()
  "Load cache from disk into memory."
  (let ((cache-file (bungee--cache-file-path "bungee-cache.el"))
        (json-symbols-file (bungee--cache-file-path "symbols.json"))
        (json-index-file (bungee--cache-file-path "index.json")))
    
    ;; Initialize hash tables
    (setq bungee--symbol-cache (make-hash-table :test 'equal))
    (setq bungee--index-cache (make-hash-table :test 'equal))
    
    (cond
     ;; Prefer Elisp cache if it exists
     ((file-exists-p cache-file)
      (load cache-file nil t)
      ;; Convert loaded lists back to symbol structs
      (let ((new-cache (make-hash-table :test 'equal)))
        (maphash (lambda (file-path symbol-lists)
                   (puthash file-path
                            (mapcar (lambda (s)
                                      (make-bungee-symbol
                                       :name (nth 0 s)
                                       :file-path (nth 1 s)
                                       :line-number (nth 2 s)
                                       :symbol-type (nth 3 s)
                                       :context (nth 4 s)))
                                    symbol-lists)
                            new-cache))
                 bungee--symbol-cache)
        (setq bungee--symbol-cache new-cache))
      (message "Loaded Elisp cache: %d files" (hash-table-count bungee--index-cache)))
     
     ;; Fall back to JSON if available
     ((and (file-exists-p json-symbols-file) (file-exists-p json-index-file))
      (bungee--load-json-cache json-symbols-file json-index-file)
      (message "Loaded JSON cache: %d files" (hash-table-count bungee--index-cache)))
     
     (t
      (message "No cache found. Run `bungee-index-directory' to create one.")))
    
    (setq bungee--cache-loaded t)))

(defun bungee--load-json-cache (symbols-file index-file)
  "Load JSON cache from SYMBOLS-FILE and INDEX-FILE."
  ;; Load symbols
  (when (file-exists-p symbols-file)
    (let* ((json-object-type 'hash-table)
           (json-array-type 'list)
           (json-key-type 'string)
           (symbols-data (json-read-file symbols-file)))
      (maphash (lambda (file-path symbols-list)
                 (let ((symbols (mapcar (lambda (s)
                                          (make-bungee-symbol
                                           :name (gethash "name" s)
                                           :file-path (gethash "file_path" s)
                                           :line-number (gethash "line_number" s)
                                           :symbol-type (intern (gethash "symbol_type" s))
                                           :context (gethash "context" s)))
                                        symbols-list)))
                   (puthash file-path symbols bungee--symbol-cache)))
               symbols-data)))
  
  ;; Load index
  (when (file-exists-p index-file)
    (let* ((json-object-type 'hash-table)
           (json-key-type 'string)
           (index-data (json-read-file index-file)))
      (maphash (lambda (file-path info)
                 (puthash file-path (gethash "mtime" info) bungee--index-cache))
               index-data))))

(defun bungee--ensure-cache ()
  "Ensure cache is loaded."
  (unless bungee--cache-loaded
    (bungee--load-cache)))

(defun bungee--save-cache ()
  "Save in-memory cache to disk as Elisp code."
  (let ((cache-dir (bungee--cache-dir)))
    (unless (file-exists-p cache-dir)
      (make-directory cache-dir t))
    
    ;; Save as Elisp file for fast loading
    (with-temp-file (bungee--cache-file-path "bungee-cache.el")
      (insert ";;; bungee-cache.el --- Bungee symbol cache -*- lexical-binding: t; -*-\n")
      (insert ";;; This file is auto-generated. Do not edit.\n\n")
      
      ;; Save symbol cache
      (insert "(setq bungee--symbol-cache (make-hash-table :test 'equal))\n\n")
      (maphash (lambda (file-path symbols)
                 (insert (format "(puthash %S\n  '(" (abbreviate-file-name file-path)))
                 (dolist (symbol symbols)
                   (insert (format "\n    %S" 
                                   (list (bungee-symbol-name symbol)
                                         (bungee-symbol-file-path symbol)
                                         (bungee-symbol-line-number symbol)
                                         (bungee-symbol-symbol-type symbol)
                                         (bungee-symbol-context symbol)))))
                 (insert ")\n  bungee--symbol-cache)\n\n"))
               bungee--symbol-cache)
      
      ;; Save index cache
      (insert "(setq bungee--index-cache (make-hash-table :test 'equal))\n\n")
      (maphash (lambda (file-path mtime)
                 (insert (format "(puthash %S %S bungee--index-cache)\n"
                                 (abbreviate-file-name file-path) mtime)))
               bungee--index-cache)
      
      (insert "\n;;; bungee-cache.el ends here\n"))
    
    ;; Optionally save as JSON for compatibility with Python tool
    (when bungee-save-json-cache
      (bungee--save-json-cache))))

(defun bungee--save-json-cache ()
  "Save cache in JSON format for Python compatibility."
  (let ((cache-dir (bungee--cache-dir)))
    ;; Save symbols
    (let ((symbols-file (expand-file-name "symbols.json" cache-dir))
          (symbols-data (make-hash-table :test 'equal)))
      (maphash (lambda (file-path symbols)
                 (puthash file-path
                          (mapcar (lambda (s)
                                    `((name . ,(bungee-symbol-name s))
                                      (file_path . ,(bungee-symbol-file-path s))
                                      (line_number . ,(bungee-symbol-line-number s))
                                      (symbol_type . ,(symbol-name (bungee-symbol-symbol-type s)))
                                      (context . ,(bungee-symbol-context s))))
                                  symbols)
                          symbols-data))
               bungee--symbol-cache)
      (with-temp-file symbols-file
        (insert (json-encode symbols-data))))
    
    ;; Save index
    (let ((index-file (expand-file-name "index.json" cache-dir))
          (index-data (make-hash-table :test 'equal)))
      (maphash (lambda (file-path mtime)
                 (puthash file-path
                          `((mtime . ,mtime)
                            (symbol_count . ,(length (gethash file-path bungee--symbol-cache))))
                          index-data))
               bungee--index-cache)
      (with-temp-file index-file
        (insert (json-encode index-data))))))

(defun bungee--file-cached-p (file-path)
  "Check if FILE-PATH is cached and up to date."
  (bungee--ensure-cache)
  (let ((cached-mtime (gethash file-path bungee--index-cache))
        (current-mtime (and (file-exists-p file-path)
                            (float-time (nth 5 (file-attributes file-path))))))
    (and cached-mtime current-mtime (>= cached-mtime current-mtime))))

;; Elisp-based parsing
(defun bungee--parse-qml-file (file-path)
  "Parse a QML file and extract symbols."
  (let ((symbols '())
        (case-fold-search nil))
    (with-temp-buffer
      (insert-file-contents file-path)
      (goto-char (point-min))
      
      ;; Find QML types
      (save-excursion
        (while (re-search-forward "^\\s-*\\([A-Z]\\w*\\)\\s-*{" nil t)
          (push (make-bungee-symbol
                 :name (match-string 1)
                 :file-path file-path
                 :line-number (line-number-at-pos)
                 :symbol-type 'class
                 :context (buffer-substring-no-properties
                           (line-beginning-position)
                           (line-end-position)))
                symbols)))
      
      ;; Find properties (including readonly)
      ;; Format: [readonly] property <type> <name>
      (goto-char (point-min))
      (while (re-search-forward "^\\s-*\\(?:readonly\\s-+\\)?property\\s-+[\\w.<>]+\\s-+\\([a-zA-Z_]\\w*\\)" nil t)
        (push (make-bungee-symbol
               :name (match-string 1)
               :file-path file-path
               :line-number (line-number-at-pos)
               :symbol-type 'property
               :context (buffer-substring-no-properties
                         (line-beginning-position)
                         (line-end-position)))
              symbols))
      
      ;; Find signals
      (goto-char (point-min))
      (while (re-search-forward "^\\s-*signal\\s-+\\([a-zA-Z_]\\w*\\)" nil t)
        (push (make-bungee-symbol
               :name (match-string 1)
               :file-path file-path
               :line-number (line-number-at-pos)
               :symbol-type 'signal
               :context (buffer-substring-no-properties
                         (line-beginning-position)
                         (line-end-position)))
              symbols))
      
      ;; Find functions
      (goto-char (point-min))
      (while (re-search-forward "^\\s-*function\\s-+\\([a-zA-Z_]\\w*\\)\\s-*(" nil t)
        (push (make-bungee-symbol
               :name (match-string 1)
               :file-path file-path
               :line-number (line-number-at-pos)
               :symbol-type 'function
               :context (buffer-substring-no-properties
                         (line-beginning-position)
                         (line-end-position)))
              symbols))
      
      ;; Find ids
      (goto-char (point-min))
      (while (re-search-forward "^\\s-*id:\\s-*\\([a-zA-Z_]\\w*\\)" nil t)
        (push (make-bungee-symbol
               :name (match-string 1)
               :file-path file-path
               :line-number (line-number-at-pos)
               :symbol-type 'variable
               :context (buffer-substring-no-properties
                         (line-beginning-position)
                         (line-end-position)))
              symbols)))
    
    (nreverse symbols)))

(defun bungee--parse-cpp-file (file-path)
  "Parse a C++ file and extract symbols."
  (let ((symbols '())
        (case-fold-search nil))
    (with-temp-buffer
      (insert-file-contents file-path)
      (goto-char (point-min))
      
      ;; Find classes/structs
      (save-excursion
        (while (re-search-forward "^\\s-*\\(?:class\\|struct\\|union\\)\\s-+\\([A-Za-z_]\\w*\\)" nil t)
          (push (make-bungee-symbol
                 :name (match-string 1)
                 :file-path file-path
                 :line-number (line-number-at-pos)
                 :symbol-type 'class
                 :context (buffer-substring-no-properties
                           (line-beginning-position)
                           (line-end-position)))
                symbols)))
      
      ;; Find namespaces
      (goto-char (point-min))
      (while (re-search-forward "^\\s-*namespace\\s-+\\([A-Za-z_]\\w*\\)" nil t)
        (push (make-bungee-symbol
               :name (match-string 1)
               :file-path file-path
               :line-number (line-number-at-pos)
               :symbol-type 'namespace
               :context (buffer-substring-no-properties
                         (line-beginning-position)
                         (line-end-position)))
              symbols))
      
      ;; Find enums
      (goto-char (point-min))
      (while (re-search-forward "^\\s-*enum\\s-+\\(?:class\\s-+\\)?\\([A-Za-z_]\\w*\\)" nil t)
        (push (make-bungee-symbol
               :name (match-string 1)
               :file-path file-path
               :line-number (line-number-at-pos)
               :symbol-type 'enum
               :context (buffer-substring-no-properties
                         (line-beginning-position)
                         (line-end-position)))
              symbols))
      
      ;; Basic function detection (simplified)
      (goto-char (point-min))
      (while (re-search-forward "^\\s-*\\(?:\\w+\\s-+\\)*\\([A-Za-z_]\\w*\\)\\s-*([^)]*)[^;{]*{" nil t)
        (let ((name (match-string 1)))
          (unless (member name '("if" "while" "for" "switch" "catch"))
            (push (make-bungee-symbol
                   :name name
                   :file-path file-path
                   :line-number (line-number-at-pos)
                   :symbol-type 'function
                   :context (buffer-substring-no-properties
                             (line-beginning-position)
                             (line-end-position)))
                  symbols)))))
    
    (nreverse symbols)))

(defun bungee--index-file (file-path &optional force)
  "Index FILE-PATH. If FORCE is non-nil, reindex even if cached."
  (bungee--ensure-cache)
  (when (or force (not (bungee--file-cached-p file-path)))
    (let ((symbols (cond
                    ((string-match-p "\\.qml\\'" file-path)
                     (bungee--parse-qml-file file-path))
                    ((string-match-p "\\.\\(cpp\\|cc\\|cxx\\|c\\+\\+\\|hpp\\|h\\|hh\\|hxx\\|h\\+\\+\\)\\'" file-path)
                     (bungee--parse-cpp-file file-path))
                    (t nil))))
      (when symbols
        (puthash file-path symbols bungee--symbol-cache)
        (puthash file-path (float-time (nth 5 (file-attributes file-path))) bungee--index-cache)
        (message "Indexed %s: %d symbols" file-path (length symbols))))))

;; Symbol lookup functions
(defun bungee-find-symbol (symbol-name &optional exact-match)
  "Find all symbols matching SYMBOL-NAME. If EXACT-MATCH is non-nil, use exact matching."
  (bungee--ensure-cache)
  (let ((results '()))
    (maphash (lambda (_ symbols)
               (dolist (symbol symbols)
                 (when (if exact-match
                           (string= (bungee-symbol-name symbol) symbol-name)
                         (string-match-p (regexp-quote symbol-name)
                                         (bungee-symbol-name symbol)))
                   (push symbol results))))
             bungee--symbol-cache)
    (sort results (lambda (a b)
                    (or (string< (bungee-symbol-name a) (bungee-symbol-name b))
                        (< (bungee-symbol-line-number a) (bungee-symbol-line-number b)))))))

(defun bungee-find-definition (symbol-name)
  "Find the most likely definition of SYMBOL-NAME."
  (let ((symbols (bungee-find-symbol symbol-name t)))
    ;; Prioritize by symbol type
    (or (cl-find-if (lambda (s) (eq (bungee-symbol-symbol-type s) 'class)) symbols)
        (cl-find-if (lambda (s) (eq (bungee-symbol-symbol-type s) 'namespace)) symbols)
        (cl-find-if (lambda (s) (eq (bungee-symbol-symbol-type s) 'function)) symbols)
        (cl-find-if (lambda (s) (eq (bungee-symbol-symbol-type s) 'property)) symbols)
        (car symbols))))

;; Interactive commands
(defun bungee-jump-to-definition ()
  "Jump to definition of symbol at point."
  (interactive)
  (let* ((symbol-name (or (thing-at-point 'symbol t)
                          (read-string "Symbol: ")))
         (symbol (bungee-find-definition symbol-name)))
    (if symbol
        (progn
          (push-mark)
          (find-file (bungee-symbol-file-path symbol))
          (goto-char (point-min))
          (forward-line (1- (bungee-symbol-line-number symbol)))
          (when (fboundp 'pulse-momentary-highlight-one-line)
            (pulse-momentary-highlight-one-line (point)))
          (message "Found: %s" (bungee-symbol-context symbol)))
      (message "No definition found for '%s'" symbol-name))))

(defun bungee-find-references ()
  "Find all references to symbol at point."
  (interactive)
  (let* ((symbol-name (or (thing-at-point 'symbol t)
                          (read-string "Find references to: ")))
         (symbols (bungee-find-symbol symbol-name)))
    (if symbols
        (let ((buffer (get-buffer-create "*Bungee References*")))
          (with-current-buffer buffer
            (let ((inhibit-read-only t))
              (erase-buffer)
              (insert (format "References to '%s':\n\n" symbol-name))
              (dolist (symbol symbols)
                (insert (format "%s:%d: %s [%s]\n"
                                (bungee-symbol-file-path symbol)
                                (bungee-symbol-line-number symbol)
                                (bungee-symbol-context symbol)
                                (bungee-symbol-symbol-type symbol))))
              (goto-char (point-min))
              (grep-mode)))
          (display-buffer buffer))
      (message "No references found for '%s'" symbol-name))))

(defun bungee-index-directory (&optional directory force)
  "Index all files in DIRECTORY. If FORCE is non-nil, reindex all files."
  (interactive "DDirectory to index: \nP")
  (let* ((dir (or directory default-directory))
         (files (directory-files-recursively
                 dir
                 "\\.\\(qml\\|cpp\\|cc\\|cxx\\|c\\+\\+\\|hpp\\|h\\|hh\\|hxx\\|h\\+\\+\\)\\'"
                 nil
                 (lambda (d) (not (or (string-match-p "/\\." d)
                                      (string-match-p "/node_modules" d)
                                      (string-match-p "/CMakeFiles" d))))))
         (count 0))
    (dolist (file files)
      (when (or force (not (bungee--file-cached-p file)))
        (bungee--index-file file force)
        (setq count (1+ count))))
    (bungee--save-cache)
    (message "Indexed %d files" count)))

(defun bungee-index-current-file ()
  "Index or reindex the current file."
  (interactive)
  (when buffer-file-name
    (bungee--index-file buffer-file-name t)
    (bungee--save-cache)))

(defun bungee-cache-status ()
  "Show cache status."
  (interactive)
  (bungee--ensure-cache)
  (let ((file-count (hash-table-count bungee--index-cache))
        (symbol-count 0))
    (maphash (lambda (_ symbols)
               (setq symbol-count (+ symbol-count (length symbols))))
             bungee--symbol-cache)
    (message "Bungee cache: %d files, %d symbols" file-count symbol-count)))

(defun bungee-clear-cache ()
  "Clear the in-memory cache."
  (interactive)
  (setq bungee--symbol-cache nil
        bungee--index-cache nil
        bungee--cache-loaded nil)
  (message "Bungee cache cleared"))

;; Python indexer integration (optional)
(defun bungee-index-with-python (&optional force)
  "Index using Python script if configured."
  (interactive "P")
  (if bungee-python-indexer
      (let* ((root (or (when (fboundp 'project-root)
                         (car (project-roots (project-current))))
                       default-directory))
             (cmd (format "python3 %s --index %s --root %s --cache-dir %s"
                          (shell-quote-argument bungee-python-indexer)
                          (if force "--force" "")
                          (shell-quote-argument root)
                          (shell-quote-argument (bungee--cache-dir)))))
        (message "Running: %s" cmd)
        (shell-command cmd)
        (bungee-clear-cache)
        (bungee--load-cache))
    (message "Python indexer not configured. Use `bungee-index-directory' instead.")))

;; Auto-update on save
(defun bungee--after-save-hook ()
  "Update index after saving a file."
  (when (and bungee-auto-update
             buffer-file-name
             (string-match-p "\\.\\(qml\\|cpp\\|cc\\|cxx\\|c\\+\\+\\|hpp\\|h\\|hh\\|hxx\\|h\\+\\+\\)\\'"
                             buffer-file-name))
    (bungee--index-file buffer-file-name t)
    (bungee--save-cache)))

;; Minor mode
(defvar bungee-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "M-.") 'bungee-jump-to-definition)
    (define-key map (kbd "M-?") 'bungee-find-references)
    (define-key map (kbd "C-c b i") 'bungee-index-directory)
    (define-key map (kbd "C-c b f") 'bungee-index-current-file)
    (define-key map (kbd "C-c b s") 'bungee-cache-status)
    (define-key map (kbd "C-c b c") 'bungee-clear-cache)
    (define-key map (kbd "C-c b p") 'bungee-index-with-python)
    map)
  "Keymap for bungee-mode.")

;;;###autoload
(define-minor-mode bungee-mode
  "Minor mode for fast symbol navigation with caching."
  :lighter " Bungee"
  :keymap bungee-mode-map
  (if bungee-mode
      (add-hook 'after-save-hook 'bungee--after-save-hook nil t)
    (remove-hook 'after-save-hook 'bungee--after-save-hook t)))

;;;###autoload
(define-globalized-minor-mode global-bungee-mode
  bungee-mode
  (lambda ()
    (when (and (not (minibufferp))
               buffer-file-name
               (string-match-p "\\.\\(qml\\|cpp\\|cc\\|cxx\\|c\\+\\+\\|hpp\\|h\\|hh\\|hxx\\|h\\+\\+\\)\\'"
                               buffer-file-name))
      (bungee-mode 1))))

;; xref integration
(when (fboundp 'xref-make)
  (defun bungee-xref-backend () 'bungee)
  
  (cl-defmethod xref-backend-identifier-at-point ((_backend (eql bungee)))
    (thing-at-point 'symbol t))
  
  (cl-defmethod xref-backend-definitions ((_backend (eql bungee)) identifier)
    (let ((symbol (bungee-find-definition identifier)))
      (when symbol
        (list (xref-make (bungee-symbol-context symbol)
                         (xref-make-file-location
                          (bungee-symbol-file-path symbol)
                          (bungee-symbol-line-number symbol)
                          0))))))
  
  (cl-defmethod xref-backend-references ((_backend (eql bungee)) identifier)
    (mapcar (lambda (symbol)
              (xref-make (bungee-symbol-context symbol)
                         (xref-make-file-location
                          (bungee-symbol-file-path symbol)
                          (bungee-symbol-line-number symbol)
                          0)))
            (bungee-find-symbol identifier)))
  
  (add-hook 'bungee-mode-hook
            (lambda ()
              (add-hook 'xref-backend-functions 'bungee-xref-backend nil t))))

(provide 'bungee)
;;; bungee.el ends here