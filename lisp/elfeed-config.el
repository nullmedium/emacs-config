;;; elfeed-config.el --- Elfeed RSS reader configuration -*- lexical-binding: t -*-

;;; Commentary:
;; Configuration for Elfeed RSS reader with custom faces and filtering functions

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
  
  ;; Async configuration for non-blocking updates
  ;; Use curl for better performance and async fetching
  (setq elfeed-use-curl t)
  (elfeed-set-timeout 30)
  
  ;; Increase number of concurrent fetches for faster updates
  (setq elfeed-curl-max-connections 10)
  
  ;; Don't block Emacs while fetching
  (setq elfeed-curl-extra-arguments '("--insecure" "--location"))
  
  ;; Make search updates async
  (setq elfeed-search-update-hook nil)
  
  ;; Background update function that doesn't block UI
  (defun elfeed-update-async ()
    "Update elfeed feeds asynchronously without blocking the UI."
    (interactive)
    (message "Starting background feed update...")
    (elfeed-update)
    (run-with-timer 1 nil
                    (lambda ()
                      (message "Feed update complete!"))))
  
  ;; Store timer references so we can cancel them
  (defvar elfeed-update-timer-30min nil
    "Timer for 30-minute elfeed updates.")
  
  ;; Auto-update feeds every 30 minutes in the background
  ;; Delayed start to avoid impacting startup performance
  (setq elfeed-update-timer-30min
        (run-with-timer (* 5 60) (* 30 60) #'elfeed-update-async))
  
  ;; Custom function for fuzzy relative timestamps
  (defun my-elfeed-search-format-date (date)
    "Format DATE as a fuzzy relative time string."
    (let* ((now (float-time))
           (time (float-time date))
           (diff (- now time))
           (sec diff)
           (min (/ diff 60))
           (hour (/ diff 3600))
           (day (/ diff 86400))
           (week (/ diff 604800))
           (month (/ diff 2592000))
           (year (/ diff 31536000)))
      (cond
       ((< sec 60) "just now")
       ((< min 2) "1 min ago")
       ((< min 60) (format "%d mins ago" (truncate min)))
       ((< hour 2) "1 hour ago")
       ((< hour 24) (format "%d hours ago" (truncate hour)))
       ((< day 2) "yesterday")
       ((< day 7) (format "%d days ago" (truncate day)))
       ((< week 2) "1 week ago")
       ((< week 4) (format "%d weeks ago" (truncate week)))
       ((< month 2) "1 month ago")
       ((< month 12) (format "%d months ago" (truncate month)))
       ((< year 2) "1 year ago")
       (t (format "%d years ago" (truncate year))))))
  
  ;; Override the elfeed print function after elfeed loads
  (with-eval-after-load 'elfeed-search
    (defun elfeed-search-print-entry--default (entry)
      "Print ENTRY to the buffer with custom date format."
      (let* ((date (elfeed-entry-date entry))
             (date-str (my-elfeed-search-format-date date))
             (date-str (elfeed-format-column date-str 15 :left))
             (title (or (elfeed-meta entry :title) (elfeed-entry-title entry) ""))
             (title-faces (elfeed-search--faces (elfeed-entry-tags entry)))
             (feed (elfeed-entry-feed entry))
             (feed-title
              (when feed
                (or (elfeed-meta feed :title) (elfeed-feed-title feed))))
             (tags (mapcar #'symbol-name (elfeed-entry-tags entry)))
             (tags-str (mapconcat
                        (lambda (s) (propertize s 'face 'elfeed-search-tag-face))
                        tags ","))
             (title-width (- (window-width) 10 elfeed-search-trailing-width 15))
             (title-column (elfeed-format-column
                            title (elfeed-clamp
                                   elfeed-search-title-min-width
                                   title-width
                                   elfeed-search-title-max-width)
                            :left)))
        (insert (propertize date-str 'face 'elfeed-search-date-face) " ")
        (insert (propertize title-column 'face title-faces 'kbd-help title) " ")
        (when feed-title
          (insert (propertize feed-title 'face 'elfeed-search-feed-face) " "))
        (when tags
          (insert "(" tags-str ")"))))
    
    ;; Set our custom function as the printer
    (setq elfeed-search-print-entry-function #'elfeed-search-print-entry--default))
  
  ;; Use standard date format (this is just for the column specification)
  (setq elfeed-search-date-format '("%Y-%m-%d" 15 :left))
  
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
  :init
  ;; Ensure elfeed-feeds variable exists
  (defvar elfeed-feeds nil)
  :config
  ;; Load feeds from org file
  (setq rmh-elfeed-org-files (list (expand-file-name "elfeed.org" user-emacs-directory)))
  ;; Initialize elfeed-org properly
  (elfeed-org)
  ;; Process the org files to populate elfeed-feeds
  (when (fboundp 'rmh-elfeed-org-process)
    (rmh-elfeed-org-process rmh-elfeed-org-files rmh-elfeed-org-tree-id)))

;; Optional: Web browser for opening links
;; Detect the operating system and set the appropriate browser
(cond
 ((eq system-type 'darwin)  ; macOS
  (setq browse-url-browser-function 'browse-url-default-macosx-browser))
 ((eq system-type 'gnu/linux)  ; Linux
  (setq browse-url-browser-function 'browse-url-firefox))
 (t  ; Default fallback
  (setq browse-url-browser-function 'browse-url-default-browser)))

;; Keybindings for elfeed
(with-eval-after-load 'elfeed
  ;; Disable CUA mode in elfeed buffers to allow single-key commands
  (add-hook 'elfeed-search-mode-hook
            (lambda ()
              (setq-local cua-mode nil)
              (setq-local cua-enable-cua-keys nil)))
  
  (add-hook 'elfeed-show-mode-hook
            (lambda ()
              (setq-local cua-mode nil)
              (setq-local cua-enable-cua-keys nil)))
  
  ;; Define keybindings
  (define-key elfeed-search-mode-map (kbd "j") 'next-line)
  (define-key elfeed-search-mode-map (kbd "k") 'previous-line)
  (define-key elfeed-search-mode-map (kbd "m") 'elfeed-search-toggle-all-star)
  (define-key elfeed-search-mode-map (kbd "u") 'elfeed-search-toggle-all-unread)
  (define-key elfeed-search-mode-map (kbd "U") 'elfeed-update-async)
  (define-key elfeed-search-mode-map (kbd "f") 'elfeed-search-live-filter)
  (define-key elfeed-search-mode-map (kbd "g") 'elfeed-search-update--force)
  (define-key elfeed-search-mode-map (kbd "G") 'elfeed-search-fetch)
  (define-key elfeed-search-mode-map (kbd "r") 'elfeed-search-untag-all-unread)
  (define-key elfeed-search-mode-map (kbd "s") 'elfeed-search-live-filter))

;; Function to reload elfeed-org configuration
(defun elfeed-org-reload ()
  "Reload elfeed feeds from org files."
  (interactive)
  (when (featurep 'elfeed-org)
    (setq elfeed-feeds nil)
    (rmh-elfeed-org-process rmh-elfeed-org-files rmh-elfeed-org-tree-id)
    (message "Elfeed feeds reloaded from org files. %d feeds loaded." (length elfeed-feeds))))

;; Store timer reference for hourly updates
(defvar elfeed-update-timer-hourly nil
  "Timer for hourly elfeed updates.")

;; Update feeds every hour
(setq elfeed-update-timer-hourly
      (run-at-time 0 (* 60 60) 'elfeed-update))

;; Functions to control auto-updates
(defun elfeed-stop-auto-updates ()
  "Stop all automatic elfeed feed updates."
  (interactive)
  (when (timerp elfeed-update-timer-30min)
    (cancel-timer elfeed-update-timer-30min)
    (setq elfeed-update-timer-30min nil))
  (when (timerp elfeed-update-timer-hourly)
    (cancel-timer elfeed-update-timer-hourly)
    (setq elfeed-update-timer-hourly nil))
  (message "Elfeed auto-updates stopped."))

(defun elfeed-start-auto-updates ()
  "Start automatic elfeed feed updates."
  (interactive)
  (elfeed-stop-auto-updates) ; Stop any existing timers first
  (setq elfeed-update-timer-30min
        (run-with-timer (* 5 60) (* 30 60) #'elfeed-update-async))
  (setq elfeed-update-timer-hourly
        (run-at-time 0 (* 60 60) 'elfeed-update))
  (message "Elfeed auto-updates started."))

;; Sorting functions
(defun elfeed-sort-by-date-ascending ()
  "Sort elfeed entries by date ascending (oldest first)."
  (interactive)
  (setq elfeed-sort-order 'ascending)
  (setf elfeed-search-sort-function nil)  ; nil means use default date sorting
  (elfeed-search-update :force)
  (message "Sorted by date: oldest first"))

(defun elfeed-sort-by-date-descending ()
  "Sort elfeed entries by date descending (newest first)."
  (interactive)
  (setq elfeed-sort-order 'descending)
  (setf elfeed-search-sort-function nil)  ; nil means use default date sorting
  (elfeed-search-update :force)
  (message "Sorted by date: newest first"))

(defun elfeed-sort-by-title ()
  "Sort elfeed entries alphabetically by title."
  (interactive)
  (setf elfeed-search-sort-function
        (lambda (a b)
          (string< (downcase (elfeed-entry-title a))
                   (downcase (elfeed-entry-title b)))))
  (elfeed-search-update :force)
  (message "Sorted alphabetically by title"))

(defun elfeed-sort-by-title-reverse ()
  "Sort elfeed entries reverse alphabetically by title."
  (interactive)
  (setf elfeed-search-sort-function
        (lambda (a b)
          (string> (downcase (elfeed-entry-title a))
                   (downcase (elfeed-entry-title b)))))
  (elfeed-search-update :force)
  (message "Sorted reverse alphabetically by title"))

(defun elfeed-sort-by-feed ()
  "Sort elfeed entries by feed source name."
  (interactive)
  (setf elfeed-search-sort-function
        (lambda (a b)
          (let ((feed-a (elfeed-feed-title (elfeed-entry-feed a)))
                (feed-b (elfeed-feed-title (elfeed-entry-feed b))))
            (or (string< feed-a feed-b)
                (and (string= feed-a feed-b)
                     (> (elfeed-entry-date a) (elfeed-entry-date b)))))))
  (elfeed-search-update :force)
  (message "Sorted by feed source"))

(defun elfeed-sort-by-tags ()
  "Sort elfeed entries by their first tag alphabetically."
  (interactive)
  (setf elfeed-search-sort-function
        (lambda (a b)
          (let ((tags-a (mapcar #'symbol-name (elfeed-entry-tags a)))
                (tags-b (mapcar #'symbol-name (elfeed-entry-tags b))))
            (string< (or (car (sort tags-a #'string<)) "")
                     (or (car (sort tags-b #'string<)) "")))))
  (elfeed-search-update :force)
  (message "Sorted by tags"))

(defun elfeed-sort-by-author ()
  "Sort elfeed entries by author (if available)."
  (interactive)
  (setf elfeed-search-sort-function
        (lambda (a b)
          (let ((author-a (or (elfeed-meta a :author) ""))
                (author-b (or (elfeed-meta b :author) "")))
            (string< author-a author-b))))
  (elfeed-search-update :force)
  (message "Sorted by author"))

(defun elfeed-sort-by-unread-first ()
  "Sort elfeed entries with unread entries first, then by date."
  (interactive)
  (setf elfeed-search-sort-function
        (lambda (a b)
          (let ((a-unread (member 'unread (elfeed-entry-tags a)))
                (b-unread (member 'unread (elfeed-entry-tags b))))
            (cond
             ((and a-unread (not b-unread)) t)
             ((and (not a-unread) b-unread) nil)
             (t (> (elfeed-entry-date a) (elfeed-entry-date b)))))))
  (elfeed-search-update :force)
  (message "Sorted: unread first"))

(defun elfeed-sort-by-starred-first ()
  "Sort elfeed entries with starred entries first, then by date."
  (interactive)
  (setf elfeed-search-sort-function
        (lambda (a b)
          (let ((a-star (member 'star (elfeed-entry-tags a)))
                (b-star (member 'star (elfeed-entry-tags b))))
            (cond
             ((and a-star (not b-star)) t)
             ((and (not a-star) b-star) nil)
             (t (> (elfeed-entry-date a) (elfeed-entry-date b)))))))
  (elfeed-search-update :force)
  (message "Sorted: starred first"))

(defun elfeed-sort-reset ()
  "Reset to default sorting (by date, newest first)."
  (interactive)
  (setq elfeed-sort-order 'descending)
  (setf elfeed-search-sort-function nil)  ; nil means use default date sorting
  (elfeed-search-update :force)
  (message "Reset to default sorting (newest first)"))

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

;; Help function to show available filters
(defun elfeed-show-filter-help ()
  "Show available filter and sorting keybindings."
  (interactive)
  (with-current-buffer (get-buffer-create "*Elfeed Help*")
    (erase-buffer)
    (insert "=== Elfeed Keybindings ===\n\n")
    (insert "LOCATION FILTERS (l + key):\n")
    (insert "  l m - Munich news\n")
    (insert "  l b - Bavaria news\n")
    (insert "  l g - Germany news\n")
    (insert "  l e - Europe/EU news\n")
    (insert "  l u - US news\n")
    (insert "  l w - World news\n")
    (insert "  l l - German language feeds\n")
    (insert "  l a - All unread entries\n\n")
    
    (insert "CATEGORY FILTERS (C + key):\n")
    (insert "  C n - News\n")
    (insert "  C t - Technology\n")
    (insert "  C s - Security\n")
    (insert "  C p - Programming\n")
    (insert "  C o - Open Source\n")
    (insert "  C c - C++\n")
    (insert "  C y - Python\n")
    (insert "  C q - Qt\n")
    (insert "  C * - Starred entries\n")
    (insert "  C d - Today's entries\n")
    (insert "  C a - All unread\n\n")
    
    (insert "ORDERING/SORTING (o + key):\n")
    (insert "  o d - Date descending (newest first)\n")
    (insert "  o D - Date ascending (oldest first)\n")
    (insert "  o t - Title (A-Z)\n")
    (insert "  o T - Title (Z-A)\n")
    (insert "  o f - Feed source\n")
    (insert "  o g - Tags/categories\n")
    (insert "  o a - Author\n")
    (insert "  o u - Unread first\n")
    (insert "  o s - Starred first\n")
    (insert "  o r - Reset to default\n\n")
    
    (insert "OTHER KEYS:\n")
    (insert "  RET - Read entry\n")
    (insert "  r   - Mark as read\n")
    (insert "  u   - Mark as unread\n")
    (insert "  m   - Toggle star\n")
    (insert "  s   - Live filter\n")
    (insert "  S   - Set filter\n")
    (insert "  c   - Clear filter\n")
    (insert "  g   - Refresh\n")
    (insert "  G   - Fetch feeds\n")
    (insert "  U   - Update feeds\n")
    (insert "  b   - Browse URL\n")
    (insert "  B   - Open in browser\n")
    (insert "  E   - Open in EWW\n")
    (insert "  q   - Quit\n")
    (insert "  ?   - This help\n")
    
    (goto-char (point-min))
    (special-mode)
    (display-buffer (current-buffer))))

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

(defun elfeed-show-open-image-at-point ()
  "Open image at point in external viewer (useful in terminal mode)."
  (interactive)
  (let ((url (or (get-text-property (point) 'image-url)
                 (get-text-property (point) 'shr-url)
                 (thing-at-point 'url))))
    (if url
        (progn
          (if (display-graphic-p)
              ;; In GUI mode, try to display inline
              (browse-url url)
            ;; In terminal mode, open with external viewer
            (start-process "image-viewer" nil "xdg-open" url))
          (message "Opening image: %s" url))
      (message "No image found at point"))))

;; Create ordering/sorting keymap
(defvar elfeed-ordering-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "d") 'elfeed-sort-by-date-descending)
    (define-key map (kbd "D") 'elfeed-sort-by-date-ascending)
    (define-key map (kbd "t") 'elfeed-sort-by-title)
    (define-key map (kbd "T") 'elfeed-sort-by-title-reverse)
    (define-key map (kbd "f") 'elfeed-sort-by-feed)
    (define-key map (kbd "g") 'elfeed-sort-by-tags)
    (define-key map (kbd "a") 'elfeed-sort-by-author)
    (define-key map (kbd "u") 'elfeed-sort-by-unread-first)
    (define-key map (kbd "s") 'elfeed-sort-by-starred-first)
    (define-key map (kbd "r") 'elfeed-sort-reset)
    map)
  "Keymap for ordering/sorting entries in elfeed.")

;; Create location filter keymap
(defvar elfeed-location-filter-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "m") 'elfeed-show-munich)
    (define-key map (kbd "b") 'elfeed-show-bavaria)
    (define-key map (kbd "g") 'elfeed-show-germany)
    (define-key map (kbd "e") 'elfeed-show-europe)
    (define-key map (kbd "u") 'elfeed-show-us)
    (define-key map (kbd "w") 'elfeed-show-world)
    (define-key map (kbd "a") 'elfeed-show-all)
    (define-key map (kbd "l") 'elfeed-show-german-language)
    map)
  "Keymap for location-based filters in elfeed.")

;; Create category filter keymap
(defvar elfeed-category-filter-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "n") 'elfeed-show-news)
    (define-key map (kbd "t") 'elfeed-show-tech)
    (define-key map (kbd "s") 'elfeed-show-security)
    (define-key map (kbd "p") 'elfeed-show-programming)
    (define-key map (kbd "o") 'elfeed-show-opensource)
    (define-key map (kbd "c") 'elfeed-show-cpp)
    (define-key map (kbd "y") 'elfeed-show-python)
    (define-key map (kbd "q") 'elfeed-show-qt)
    (define-key map (kbd "*") 'elfeed-show-starred)
    (define-key map (kbd "a") 'elfeed-show-all)
    (define-key map (kbd "d") 'elfeed-show-today)
    map)
  "Keymap for category-based filters in elfeed.")

;; Bind keys for article viewing
(with-eval-after-load 'elfeed
  ;; In search mode
  (define-key elfeed-search-mode-map (kbd "E") 'elfeed-search-eww-open)
  (define-key elfeed-search-mode-map (kbd "B") 'elfeed-open-in-browser)
  
  ;; Bind location filters with 'l' prefix
  (define-key elfeed-search-mode-map (kbd "l") elfeed-location-filter-map)
  
  ;; Bind category filters with 'c' prefix (note: 'c' currently clears filter, so using 'C')
  (define-key elfeed-search-mode-map (kbd "C") elfeed-category-filter-map)
  
  ;; Bind ordering/sorting with 'o' prefix
  (define-key elfeed-search-mode-map (kbd "o") elfeed-ordering-map)
  
  ;; Bind help function
  (define-key elfeed-search-mode-map (kbd "?") 'elfeed-show-filter-help)
  
  ;; In show mode
  (define-key elfeed-show-mode-map (kbd "E") 'elfeed-show-eww-open)
  (define-key elfeed-show-mode-map (kbd "R") 'elfeed-show-readable)
  (define-key elfeed-show-mode-map (kbd "F") 'elfeed-show-toggle-full-article)
  (define-key elfeed-show-mode-map (kbd "B") 'elfeed-open-in-browser)
  (define-key elfeed-show-mode-map (kbd "I") 'elfeed-show-open-image-at-point))

;; Disable line numbers in elfeed buffers and increase font size
(add-hook 'elfeed-show-mode-hook 
          (lambda () 
            (display-line-numbers-mode -1)
            (setq-local display-line-numbers nil)
            (text-scale-set 1)  ; Increase font size by 1 step
            ;; Handle images based on display type
            (if (display-graphic-p)
                ;; GUI mode - show images inline
                (progn
                  (setq-local shr-inhibit-images nil)
                  (setq-local shr-blocked-images nil))
              ;; Terminal mode - show placeholders
              (setq-local shr-inhibit-images t))))
            
(add-hook 'elfeed-search-mode-hook 
          (lambda () 
            (display-line-numbers-mode -1)
            (setq-local display-line-numbers nil)))

;; Disable line numbers in EWW
(add-hook 'eww-mode-hook 
          (lambda () 
            (display-line-numbers-mode -1)
            (setq-local display-line-numbers nil)))

;; Additional function to force disable line numbers in elfeed
(defun elfeed-disable-line-numbers ()
  "Forcefully disable line numbers in elfeed buffers."
  (interactive)
  (when (or (eq major-mode 'elfeed-search-mode)
            (eq major-mode 'elfeed-show-mode))
    (display-line-numbers-mode -1)
    (setq-local display-line-numbers nil)
    (message "Line numbers disabled in elfeed")))

(provide 'elfeed-config)
;;; elfeed-config.el ends here