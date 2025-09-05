;;; elfeed-config.el --- Elfeed RSS reader configuration

;;; Code:

;; Install required packages if not already installed
(use-package elfeed
  :ensure t
  :bind (("C-x w" . elfeed))
  :config
  ;; Set default search filter to show entries from last 2 weeks
  (setq elfeed-search-filter "@2-weeks-ago +unread")
  
  ;; Store database in .emacs.d
  (setq elfeed-db-directory (expand-file-name "elfeed" user-emacs-directory))
  
  ;; Refresh feeds on startup
  (setq elfeed-search-remain-on-entry t)
  
  ;; Set update interval
  (setq elfeed-search-title-max-width 100)
  (setq elfeed-search-title-min-width 30)
  
  ;; Sorting configuration
  (setq elfeed-sort-order 'descending)
  (setq elfeed-search-clipboard-type 'CLIPBOARD)
  (setq elfeed-search-date-format '("%F %R" 16 :left))
  
  ;; Face customization for different tags
  (defface elfeed-face-tag-news
    '((t :foreground "#8B4513"))
    "Face for news-tagged entries")
  
  (defface elfeed-face-tag-tech
    '((t :foreground "#4682B4"))
    "Face for tech-tagged entries")
  
  (defface elfeed-face-tag-security
    '((t :foreground "#DC143C"))
    "Face for security-tagged entries")
  
  (defface elfeed-face-tag-programming
    '((t :foreground "#228B22"))
    "Face for programming-tagged entries")
  
  (defface elfeed-face-tag-opensource
    '((t :foreground "#FF8C00"))
    "Face for opensource-tagged entries")
  
  ;; Apply faces to tags
  (setq elfeed-search-face-alist
        '((news elfeed-face-tag-news)
          (tech elfeed-face-tag-tech)
          (security elfeed-face-tag-security)
          (programming elfeed-face-tag-programming)
          (opensource elfeed-face-tag-opensource))))

(use-package elfeed-org
  :ensure t
  :after elfeed
  :config
  ;; Load feeds from org file
  (setq rmh-elfeed-org-files (list (expand-file-name "elfeed.org" user-emacs-directory)))
  (elfeed-org))

;; Optional: Web browser for opening links
(setq browse-url-browser-function 'browse-url-firefox)

;; Keybindings for elfeed
(with-eval-after-load 'elfeed
  (define-key elfeed-search-mode-map (kbd "j") 'next-line)
  (define-key elfeed-search-mode-map (kbd "k") 'previous-line)
  (define-key elfeed-search-mode-map (kbd "m") 'elfeed-search-toggle-all-star)
  (define-key elfeed-search-mode-map (kbd "u") 'elfeed-search-toggle-all-unread)
  (define-key elfeed-search-mode-map (kbd "U") 'elfeed-update)
  (define-key elfeed-search-mode-map (kbd "f") 'elfeed-search-live-filter))

;; Update feeds every hour
(run-at-time 0 (* 60 60) 'elfeed-update)

;; Sorting functions
(defun elfeed-sort-by-date-ascending ()
  "Sort elfeed entries by date ascending."
  (interactive)
  (setq elfeed-sort-order 'ascending)
  (elfeed-search-update :force))

(defun elfeed-sort-by-date-descending ()
  "Sort elfeed entries by date descending."
  (interactive)
  (setq elfeed-sort-order 'descending)
  (elfeed-search-update :force))

(defun elfeed-sort-by-title ()
  "Sort elfeed entries by title."
  (interactive)
  (setf elfeed-search-sort-function
        (lambda (a b)
          (string< (elfeed-entry-title a)
                   (elfeed-entry-title b))))
  (elfeed-search-update :force))

(defun elfeed-sort-by-feed ()
  "Sort elfeed entries by feed source."
  (interactive)
  (setf elfeed-search-sort-function
        (lambda (a b)
          (string< (elfeed-feed-title (elfeed-entry-feed a))
                   (elfeed-feed-title (elfeed-entry-feed b)))))
  (elfeed-search-update :force))

;; Helper function to show only specific categories
(defun elfeed-show-news ()
  "Show only news entries."
  (interactive)
  (elfeed-search-set-filter "@2-weeks-ago +unread +news"))

(defun elfeed-show-tech ()
  "Show only tech entries."
  (interactive)
  (elfeed-search-set-filter "@2-weeks-ago +unread +tech"))

(defun elfeed-show-security ()
  "Show only security entries."
  (interactive)
  (elfeed-search-set-filter "@2-weeks-ago +unread +security"))

(defun elfeed-show-programming ()
  "Show only programming entries."
  (interactive)
  (elfeed-search-set-filter "@2-weeks-ago +unread +programming"))

(defun elfeed-show-opensource ()
  "Show only open source entries."
  (interactive)
  (elfeed-search-set-filter "@2-weeks-ago +unread +opensource"))

;; Location-based filters
(defun elfeed-show-munich ()
  "Show only Munich/Bavaria news."
  (interactive)
  (elfeed-search-set-filter "@2-weeks-ago +unread +munich"))

(defun elfeed-show-bavaria ()
  "Show only Bavaria news."
  (interactive)
  (elfeed-search-set-filter "@2-weeks-ago +unread +bavaria"))

(defun elfeed-show-germany ()
  "Show only German news."
  (interactive)
  (elfeed-search-set-filter "@2-weeks-ago +unread +germany"))

(defun elfeed-show-europe ()
  "Show only European/EU news."
  (interactive)
  (elfeed-search-set-filter "@2-weeks-ago +unread +eu"))

(defun elfeed-show-us ()
  "Show only US news."
  (interactive)
  (elfeed-search-set-filter "@2-weeks-ago +unread +us"))

(defun elfeed-show-world ()
  "Show only world news."
  (interactive)
  (elfeed-search-set-filter "@2-weeks-ago +unread +world"))

;; Language filters
(defun elfeed-show-german-language ()
  "Show only German language feeds."
  (interactive)
  (elfeed-search-set-filter "@2-weeks-ago +unread +de"))

;; Programming language filters
(defun elfeed-show-cpp ()
  "Show only C++ entries."
  (interactive)
  (elfeed-search-set-filter "@2-weeks-ago +unread +cpp"))

(defun elfeed-show-python ()
  "Show only Python entries."
  (interactive)
  (elfeed-search-set-filter "@2-weeks-ago +unread +python"))

(defun elfeed-show-qt ()
  "Show only Qt entries."
  (interactive)
  (elfeed-search-set-filter "@2-weeks-ago +unread +qt"))

;; Combined filters
(defun elfeed-show-today ()
  "Show entries from today only."
  (interactive)
  (elfeed-search-set-filter "@1-day-ago +unread"))

(defun elfeed-show-starred ()
  "Show starred entries."
  (interactive)
  (elfeed-search-set-filter "+star"))

(defun elfeed-show-all ()
  "Show all unread entries."
  (interactive)
  (elfeed-search-set-filter "@2-weeks-ago +unread"))

(defun elfeed-mark-all-read ()
  "Mark all entries as read."
  (interactive)
  (mark-whole-buffer)
  (elfeed-search-untag-all-unread))

;; Advanced filter prompt
(defun elfeed-filter-by-source ()
  "Filter by specific feed source."
  (interactive)
  (let* ((feeds (elfeed-feed-list))
         (titles (mapcar (lambda (url)
                          (or (plist-get (elfeed-db-get-feed url) :title)
                              url))
                        feeds))
         (selected (completing-read "Select feed: " titles)))
    (elfeed-search-set-filter
     (format "@2-weeks-ago +unread =%s" selected))))

;; Custom search function
(defun elfeed-search-custom ()
  "Prompt for a custom search filter."
  (interactive)
  (let ((filter (read-string "Enter filter: " elfeed-search-filter)))
    (elfeed-search-set-filter filter)))

;; Article reading functions
(defun elfeed-show-entry-in-eww ()
  "Open the current elfeed entry in eww browser."
  (interactive)
  (let ((entry (elfeed-search-selected :single)))
    (when entry
      (eww (elfeed-entry-link entry))
      (add-hook 'eww-after-render-hook 'eww-readable nil t))))

(defun elfeed-search-eww-open (&optional use-generic-p)
  "Open the current elfeed entry in eww.
If USE-GENERIC-P is non-nil, use eww-readable after loading."
  (interactive "P")
  (let ((entries (elfeed-search-selected)))
    (cl-loop for entry in entries
             do (elfeed-untag entry 'unread)
             when (elfeed-entry-link entry)
             do (eww (elfeed-entry-link entry)))
    (when use-generic-p
      (add-hook 'eww-after-render-hook 'eww-readable nil t))
    (unless (use-region-p) (forward-line))
    (elfeed-search-update-entry)))

(defun elfeed-show-eww-open (&optional use-generic-p)
  "Open the current elfeed show entry in eww.
If USE-GENERIC-P is non-nil, use eww-readable after loading."
  (interactive "P")
  (let ((link (elfeed-entry-link elfeed-show-entry)))
    (when link
      (eww link)
      (when use-generic-p
        (add-hook 'eww-after-render-hook 'eww-readable nil t)))))

;; Fetch and display article content in elfeed-show buffer
(defun elfeed-show-refresh-mail-style ()
  "Refresh the current elfeed entry, fetching full content and displaying it."
  (interactive)
  (let ((link (elfeed-entry-link elfeed-show-entry)))
    (when link
      (message "Fetching full article content...")
      (url-retrieve
       link
       (lambda (status)
         (if (plist-get status :error)
             (message "Error fetching article: %s" (plist-get status :error))
           (let ((html (buffer-string))
                 (inhibit-read-only t))
             (with-current-buffer (get-buffer "*elfeed-entry*")
               (erase-buffer)
               (insert (format "Title: %s\n" (elfeed-entry-title elfeed-show-entry)))
               (insert (format "Feed: %s\n" (elfeed-feed-title (elfeed-entry-feed elfeed-show-entry))))
               (insert (format "Date: %s\n" (format-time-string "%Y-%m-%d %H:%M" (elfeed-entry-date elfeed-show-entry))))
               (insert (format "Link: %s\n\n" link))
               (insert "--- Full Article ---\n\n")
               (let ((shr-width (- (window-width) 5))
                     (shr-max-image-proportion 0.7))
                 (shr-render-region (point) (point-max)))
               (goto-char (point-min))))))))))

;; Enhanced readable mode for elfeed
(defun elfeed-show-readable ()
  "Make the current elfeed entry more readable by extracting main content."
  (interactive)
  (let ((inhibit-read-only t))
    (save-excursion
      (goto-char (point-min))
      (when (search-forward "\n\n" nil t)
        (let ((shr-width (min 80 (- (window-width) 5)))
              (shr-max-image-proportion 0.6)
              (shr-use-fonts nil))
          (shr-render-region (point) (point-max)))))
    (text-scale-increase 1)
    (olivetti-mode 1)))

;; Toggle between summary and full article
(defvar-local elfeed-show-full-article-p nil
  "Whether the full article is currently displayed.")

(defun elfeed-show-toggle-full-article ()
  "Toggle between entry summary and full article content."
  (interactive)
  (if elfeed-show-full-article-p
      (progn
        (elfeed-show-refresh)
        (setq-local elfeed-show-full-article-p nil)
        (message "Showing summary"))
    (let ((link (elfeed-entry-link elfeed-show-entry)))
      (when link
        (message "Fetching full article...")
        (url-retrieve
         link
         (lambda (status)
           (if (plist-get status :error)
               (message "Error fetching article: %s" (plist-get status :error))
             (let ((html (buffer-substring (point) (point-max)))
                   (inhibit-read-only t))
               (with-current-buffer (get-buffer "*elfeed-entry*")
                 (let ((pos (point)))
                   (erase-buffer)
                   (elfeed-show-refresh)
                   (goto-char (point-max))
                   (insert "\n\n--- Full Article ---\n\n")
                   (let ((start (point)))
                     (insert html)
                     (shr-render-region start (point-max))
                     (goto-char pos))
                   (setq-local elfeed-show-full-article-p t)
                   (message "Showing full article")))))))))))

;; Open in external browser as fallback
(defun elfeed-open-in-browser ()
  "Open current entry in external browser."
  (interactive)
  (let ((entry (if (eq major-mode 'elfeed-show-mode)
                   elfeed-show-entry
                 (elfeed-search-selected :single))))
    (when entry
      (browse-url (elfeed-entry-link entry)))))

;; Bind keys for article viewing
(with-eval-after-load 'elfeed
  ;; In search mode
  (define-key elfeed-search-mode-map (kbd "E") 'elfeed-search-eww-open)
  (define-key elfeed-search-mode-map (kbd "B") 'elfeed-open-in-browser)
  
  ;; In show mode
  (define-key elfeed-show-mode-map (kbd "E") 'elfeed-show-eww-open)
  (define-key elfeed-show-mode-map (kbd "R") 'elfeed-show-readable)
  (define-key elfeed-show-mode-map (kbd "F") 'elfeed-show-toggle-full-article)
  (define-key elfeed-show-mode-map (kbd "B") 'elfeed-open-in-browser))

;; Disable line numbers in elfeed buffers
(add-hook 'elfeed-show-mode-hook (lambda () (display-line-numbers-mode 0)))
(add-hook 'elfeed-search-mode-hook (lambda () (display-line-numbers-mode 0)))

;; Disable line numbers in EWW
(add-hook 'eww-mode-hook (lambda () (display-line-numbers-mode 0)))

(provide 'elfeed-config)
;;; elfeed-config.el ends here