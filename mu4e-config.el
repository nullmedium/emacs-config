;;; mu4e-config.el --- mu4e email configuration -*- lexical-binding: t; -*-

;; mu4e should already be loaded from .emacs before this file is loaded
;; If not loaded, try to load it
(add-to-list 'load-path "/usr/local/share/emacs/site-lisp/mu4e/")
(require 'mu4e)

;; HTML rendering configuration for mu4e 1.12

;; Use shr as the default renderer
(setq mm-text-html-renderer 'shr)

;; Configure shr for better plain text display
;; (setq shr-use-colors nil)        ; No colors
;; (setq shr-use-fonts nil)         ; No variable fonts
;; (setq shr-width 80)              ; 80 column width
(setq shr-bullet "• ")            ; Nice bullet

;; Create a custom command to view HTML with pandoc
(defun mu4e-view-html-with-pandoc ()
  "View the HTML part of the current message using pandoc."
  (interactive)
  (let ((msg (mu4e-message-at-point)))
    (unless msg
      (mu4e-error "No message at point"))
    (let* ((path (mu4e-message-field msg :path))
           (pandoc-buffer "*mu4e-pandoc*")
           (temp-dir (make-temp-file "mu4e-extract" t)))
      (if path
          (progn
            (with-current-buffer (get-buffer-create pandoc-buffer)
              (erase-buffer)
              (insert "=== HTML Email Rendered with Pandoc ===\n\n")
              ;; Extract all parts
              (call-process "mu" nil nil nil
                            "extract" "--save-all"
                            (format "--target-dir=%s" temp-dir) path)
              ;; Find HTML files and convert them
              (let ((html-files (directory-files temp-dir t "\\.html?$")))
                (if html-files
                    (dolist (html-file html-files)
                      (insert (shell-command-to-string
                               (format "pandoc -f html -t markdown --wrap=auto --columns=80 '%s' 2>/dev/null"
                                       html-file))))
                  ;; No HTML files found, try extracting from message directly
                  (let ((raw-msg (shell-command-to-string (format "cat '%s'" path))))
                    ;; Look for HTML content between boundaries
                    (if (string-match "Content-Type: text/html" raw-msg)
                        (progn
                          (insert "Converting HTML content...\n\n")
                          (let ((temp-html (make-temp-file "mu4e-msg" nil ".html")))
                            (with-temp-file temp-html
                              (insert raw-msg))
                            (insert (shell-command-to-string
                                     (format "cat '%s' | sed -n '/Content-Type: text\\/html/,/^--/p' | sed '1,/^$/d' | sed '/^--/,$d' | pandoc -f html -t markdown --wrap=auto --columns=80 2>/dev/null"
                                             temp-html)))
                            (delete-file temp-html)))
                      (insert "No HTML content found in this message.\n")))))
              ;; Clean up temp directory
              (delete-directory temp-dir t)
              (goto-char (point-min))
              (view-mode)
              (display-buffer (current-buffer))))
        (mu4e-warn "Cannot access message file")))))

;; Simpler approach: Add action to view HTML with pandoc
(add-to-list 'mu4e-view-actions
             '("pandoc" . (lambda (msg)
                           (let* ((path (mu4e-message-field msg :path))
                                  (pandoc-buffer "*mu4e-pandoc*"))
                             (when path
                               (with-current-buffer (get-buffer-create pandoc-buffer)
                                 (erase-buffer)
                                 (insert "=== HTML Email Rendered with Pandoc ===\n\n")
                                 (let ((html-content
                                        (shell-command-to-string
                                         (format "mu view '%s' 2>/dev/null | awk '/text\\/html/{flag=1; next} flag && /^--/{exit} flag' | pandoc -f html -t markdown --wrap=auto --columns=80 2>/dev/null" path))))
                                   (if (and html-content (> (length html-content) 0))
                                       (insert html-content)
                                     ;; If no HTML part found, show message
                                     (insert "No HTML content found in this message.\n\n")
                                     (insert (shell-command-to-string
                                              (format "mu view '%s'" path)))))
                                 (goto-char (point-min))
                                 (view-mode)
                                 (display-buffer (current-buffer)))))))
             t)

;; Add keybinding for pandoc view
(with-eval-after-load 'mu4e-view
  (define-key mu4e-view-mode-map (kbd "H") 'mu4e-view-html-with-pandoc))

(message "mu4e: HTML rendering configured. Press 'H' in message view to render with pandoc.")

;; Basic mu4e settings
(setq mu4e-maildir "~/Maildir"
      ;; Use our processing script instead of plain mbsync
      ;; This will sync mail and fix List-Id headers
      mu4e-get-mail-command (expand-file-name "process-mail.sh" user-emacs-directory)
      mu4e-update-interval 300           ; Update every 5 minutes
      mu4e-compose-signature-auto-include nil
      mu4e-view-show-images t
      mu4e-view-show-addresses t
      mu4e-change-filenames-when-moving t  ; Needed for mbsync
      mu4e-index-cleanup t                 ; Clean up after moving
      mu4e-index-lazy-check nil            ; Don't be lazy about indexing
      mu4e-hide-index-messages t)          ; Hide indexing messages to avoid errors

;; Function to get current context's maildir prefix
(defun mu4e-current-context-maildir-prefix ()
  "Get the maildir prefix for the current context."
  (if mu4e-context-current
      (let ((context-name (mu4e-context-name mu4e-context-current)))
        (format "maildir:/%s/*" context-name))
    ""))

;; Bookmarks (shortcuts to common searches)
;; Use setq to define the complete list at once
(setq mu4e-bookmarks
      '(;; Basic views - work in current context
        (:name "Unread messages"
         :query "flag:unread AND NOT flag:trashed"
         :key ?u)
        (:name "Today's messages"
         :query "date:today..now"
         :key ?t)
        (:name "Last 7 days"
         :query "date:7d..now"
         :key ?w)

        ;; Smart mailboxes - search across all contexts
        (:name "📌 Important"
         :query "(flag:flagged OR prio:high OR from:/boss|manager|ceo|director|important/ OR subject:/urgent|important|critical|asap/) AND NOT flag:trashed"
         :key ?i)

        (:name "📰 Newsletters"
         :query "(from:/newsletter|news|digest|update|weekly|daily|monthly|bulletin|announcement/ OR subject:/newsletter|digest|update|weekly|edition/) AND NOT flag:trashed AND NOT flag:flagged"
         :key ?n)

        (:name "🛍️ Purchases & Orders"
         :query "(from:/amazon|ebay|paypal|stripe|shopify|order|store|shop|invoice|receipt/ OR subject:/order|invoice|receipt|purchase|payment|confirmation|shipping|delivery|tracking/) AND NOT flag:trashed"
         :key ?p)

        (:name "📎 Attachments"
         :query "flag:attach AND NOT flag:trashed"
         :key ?a)

        (:name "🎫 Travel & Tickets"
         :query "(from:/airline|hotel|booking|expedia|airbnb|uber|lyft|train|eventbrite|ticketmaster/ OR subject:/booking|reservation|ticket|flight|itinerary|confirmation/) AND NOT flag:trashed"
         :key ?v)

        (:name "💰 Finance & Banking"
         :query "(from:/bank|credit|visa|mastercard|amex|insurance|tax|accountant/ OR subject:/statement|balance|transaction|payment/) AND NOT flag:trashed"
         :key ?f)

        (:name "👥 Social & Forums"
         :query "(from:/facebook|twitter|linkedin|instagram|reddit|github|gitlab|discourse|forum/ OR subject:/commented|replied|mentioned|tagged|followed/) AND NOT flag:trashed"
         :key ?s)

        ;; Mailing Lists (Personal context)
        (:name "📋 All Mailing Lists"
         :query "maildir:/Personal/Lists AND NOT flag:trashed"
         :key ?L)

        (:name "📋 C++ std-discussion"
         :query "list:std-discussion.lists.isocpp.org AND NOT flag:trashed"
         :key ?C)

        (:name "📋 Qt Interest"  
         :query "list:interest.qt-project.org AND NOT flag:trashed"
         :key ?q)

        (:name "📋 Boost"
         :query "list:boost.lists.boost.org AND NOT flag:trashed"
         :key ?b)

        (:name  "📋 GCC"
         :query "list:gcc.gnu.gcc.org AND NOT flag:trashed"
         :key ?G)

        (:name "📋 LKML"
         :query "list:linux-kernel.vger.kernel.org AND NOT flag:trashed"
         :key ?K)))

(setq mu4e-maildir-shortcuts
      '(("/Personal/INBOX"    . ?i)
        ("/Personal/Sent"     . ?s)
        ("/Personal/Trash"    . ?t)
        ("/Personal/Drafts"   . ?d)
        ("/Personal/Spam   "  . ?S)
        ("/Personal/Archive"  . ?a)
        ("/Personal/Lists"    . ?l)
        ("/IONOS/INBOX"       . ?I)
        ("/IONOS/Sent"        . ?S)
        ("/IONOS/Trash"       . ?T)
        ("/IONOS/Drafts"      . ?D)
        ("/IONOS/Archive"     . ?A)))

;; UI Configuration - use default headers for now
;; (setq mu4e-headers-fields
;;       '((:human-date . 12)
;;         (:flags . 6)
;;         (:from-or-to . 22)
;;         (:subject)))

;; Make mu4e respect the current color theme
(setq mu4e-view-use-gnus t)  ; Use Gnus article mode for better theme support
(setq shr-use-colors nil)    ; Let the theme handle colors in HTML emails

;; Use full window for reading emails
(setq mu4e-split-view nil)   ; Don't split, use full window for message view

;; Make headers/search view respect theme
(setq mu4e-headers-unread-face 'bold)  ; Use theme's bold face instead of custom color
(setq mu4e-headers-highlight-face 'highlight)  ; Use theme's highlight face
(setq mu4e-headers-flagged-face 'font-lock-warning-face)  ; Use theme's warning face

;; Enable inline images
(setq mu4e-view-show-images t)
(when (fboundp 'imagemagick-register-types)
  (imagemagick-register-types))

;; Colorize inline patches in emails
(require 'diff-mode)

(defun mu4e-colorize-patch ()
  "Colorize patches in mu4e view buffers."
  (save-excursion
    (goto-char (point-min))
    ;; Look for patch sections (starting with diff, ---, or @@)
    (while (re-search-forward "^\\(diff \\|--- \\|\\+\\+\\+ \\|@@ \\)" nil t)
      (let ((patch-start (match-beginning 0)))
        ;; Find the end of the patch
        (if (re-search-forward "^[^-+@ \t]" nil t)
            (backward-char)
          (goto-char (point-max)))
        (let ((patch-end (point))
              (inhibit-read-only t))
          ;; Apply diff-mode font-lock to the patch region
          (add-text-properties patch-start patch-end
                               '(face nil)) ; Reset face first
          (save-restriction
            (narrow-to-region patch-start patch-end)
            (diff-mode)
            (font-lock-fontify-region patch-start patch-end)
            (widen))))))
  ;; Also colorize simple diff lines
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "^\\(-.*\\)$" nil t)
      (let ((inhibit-read-only t))
        (add-face-text-property (match-beginning 1) (match-end 1)
                                 'diff-removed)))
    (goto-char (point-min))
    (while (re-search-forward "^\\(\\+.*\\)$" nil t)
      (let ((inhibit-read-only t))
        (add-face-text-property (match-beginning 1) (match-end 1)
                                 'diff-added)))
    (goto-char (point-min))
    (while (re-search-forward "^\\(@@.*@@\\).*$" nil t)
      (let ((inhibit-read-only t))
        (add-face-text-property (match-beginning 1) (match-end 1)
                                 'diff-hunk-header)))))

;; Hook to colorize patches when viewing messages
(add-hook 'mu4e-view-mode-hook 'mu4e-colorize-patch)

;; For Gnus article mode (when mu4e-view-use-gnus is t)
(with-eval-after-load 'gnus-art
  (add-hook 'gnus-article-mode-hook 'mu4e-colorize-patch))

;; Use mu4e for composing email
(setq mail-user-agent 'mu4e-user-agent)

;; Keybindings
(global-set-key (kbd "C-x m") 'mu4e)
(global-set-key (kbd "C-x M") 'mu4e-compose-new)

;; Double-check pandoc is available and being used
(if (executable-find "pandoc")
    (message "Pandoc found at: %s" (executable-find "pandoc"))
  (message "WARNING: Pandoc not found!"))

;; Prefer plain text when available, but show HTML when it's the only option
(setq mu4e-view-prefer-html nil)

;; Actions
(add-to-list 'mu4e-view-actions
             '("ViewInBrowser" . mu4e-action-view-in-browser) t)

;; Don't keep message buffers around
(setq message-kill-buffer-on-exit t)

;; For Proton Bridge: Comment out custom marks for now to avoid errors
;; The default behavior will be:
;; d - move to trash
;; D - delete permanently
;; r - refile/archive

;; If you want to customize deletion behavior, uncomment and adjust:
;; (with-eval-after-load 'mu4e
;;   ;; Make 'd' archive instead of trash
;;   (setf (alist-get 'trash mu4e-marks)
;;         (list :char '("d" . "▼")
;;               :prompt "archive"
;;               :dyn-target (lambda (target msg)
;;                            (mu4e-get-refile-folder msg))
;;               :action (lambda (docid msg target)
;;                        (mu4e--server-move docid
;;                                          (mu4e--mark-check-target target)
;;                                          "-N")))))

;; Test function to verify pandoc is working
(defun mu4e-test-pandoc ()
  "Test if pandoc is being used for HTML rendering."
  (interactive)
  (let ((test-html "<html><body><h1>Test Header</h1><p>This is a <strong>test</strong> paragraph with <em>emphasis</em>.</p><ul><li>Item 1</li><li>Item 2</li></ul></body></html>")
        (temp-file (make-temp-file "mu4e-pandoc-test" nil ".html")))
    (with-temp-file temp-file
      (insert test-html))
    (message "Testing pandoc with command: %s" mu4e-html2text-command)
    (message "Input HTML:\n%s" test-html)
    (message "Pandoc output:\n%s"
             (shell-command-to-string
              (format "%s < %s" mu4e-html2text-command temp-file)))
    (delete-file temp-file)))

;; Mailing lists configuration
(setq mu4e-mailing-lists
      '((:list-id "linux-kernel.vger.linux.org" :name "linux-kernel")
        (:list-id "std-discussion.lists.isocpp.org" :name "std-discussion")
        (:list-id "gcc.gnu.gcc.org" :name "gnu-gcc")
        (:list-id "interest.qt-project.org" :name "Qt-Interest")
        (:list-id "boost.lists.boost.org" :name "Boost")
        (:list-id "boost-announce.lists.boost.org" :name "Boost-Announce")
        (:list-id "boost-interest.lists.boost.org" :name "Boost-Interest")))

;; Signature
(setq mu4e-compose-signature
      "Jens")

;; SMTP Configuration for sending mail
(require 'smtpmail)
(setq message-send-mail-function 'smtpmail-send-it)

;; Default SMTP settings (will be overridden by context)
(setq smtpmail-stream-type 'starttls
      smtpmail-smtp-service 587
      smtpmail-debug-info t
      smtpmail-debug-verb t
      smtpmail-auth-credentials "~/.authinfo")

;; Configure context policy
(setq mu4e-context-policy 'pick-first
      mu4e-compose-context-policy 'ask-if-none)

;; Function to set SMTP parameters based on From address
(defun my-mu4e-set-smtp-params ()
  "Set SMTP parameters based on the From address."
  (let ((from (message-field-value "From")))
    (cond
     ;; Personal account - also handles alias addresses @luedicke.me and @luedicke.xyz
     ((or (string-match "@luedicke\\.me" from)
          (string-match "@luedicke\\.xyz" from))
      (message "Setting SMTP for Personal account (Proton Bridge)...")
      (setq smtpmail-smtp-server "127.0.0.1"
            smtpmail-smtp-service 1025
            smtpmail-stream-type 'starttls
            smtpmail-smtp-user "jens@luedicke.me"  ; Always use main account for auth
            smtpmail-auth-credentials "~/.authinfo"
            smtpmail-smtp-timeout 30))  ; 30 second timeout
     ((string-match "jens@luedicke.cloud" from)
      (message "Setting SMTP for IONOS account...")
      (setq smtpmail-smtp-server "smtp.ionos.de"
            smtpmail-smtp-service 587
            smtpmail-stream-type 'starttls
            smtpmail-smtp-user "jens@luedicke.cloud"
            smtpmail-auth-credentials "~/.authinfo"
            smtpmail-smtp-timeout 30))  ; 30 second timeout
     (t
      (error "Unknown sender address: %s" from)))))

;; Hook to set SMTP params before sending
(add-hook 'message-send-hook 'my-mu4e-set-smtp-params)

;; Alias email addresses configuration
;; Define your email aliases here
(setq my-email-aliases
      '(("std-discussion@luedicke.xyz" . "std-discussion.lists.isocpp.org")
        ("gnu-gcc@luedicke.xyz" . "gcc.gcc.gnu.org")
        ("qt-interest@luedicke.xyz" . "interest.qt-project.org")
        ("boost@luedicke.xyz" . "boost.lists.boost.org")
        ("jens@luedicke.me" . "linux-kernel.vger.kernel.org")))

;; Variable to store the desired From address
(defvar my-mu4e-reply-address nil
  "Stores the email address to use for replies to mailing lists.")

;; Function to determine the reply address based on recipient
(defun my-mu4e-set-from-address ()
  "Set the From address based on the original recipient.
If the message was sent to one of our aliases (via mailing list),
use that alias as the From address."
  (setq my-mu4e-reply-address nil)  ; Reset first
  (let ((msg mu4e-compose-parent-message))
    (when msg
      (let ((list-id (mu4e-message-field msg :list))
            (to (mu4e-message-field msg :to))
            (cc (mu4e-message-field msg :cc)))
        ;; Check if this message came from a mailing list we have an alias for
        (dolist (alias-pair my-email-aliases)
          (when (and list-id
                     (string-match-p (regexp-quote (cdr alias-pair)) list-id)
                     (not my-mu4e-reply-address))
            ;; Store the alias to use
            (setq my-mu4e-reply-address (car alias-pair))
            (message "Will use alias address: %s" my-mu4e-reply-address)))))))

;; Function to actually set the From header
(defun my-mu4e-compose-set-from ()
  "Set the From address in the compose buffer."
  (when my-mu4e-reply-address
    ;; For aliases, we need to keep the authenticated address in From
    ;; but add a Reply-To with the alias address
    (save-excursion
      ;; Keep the main address in From (for SMTP authentication)
      (message-remove-header "From")
      (message-add-header (format "From: %s <%s>" user-full-name "jens@luedicke.me"))

      ;; Add Reply-To with the alias address so replies come back to the right address
      (message-remove-header "Reply-To")
      (message-add-header (format "Reply-To: %s <%s>" user-full-name my-mu4e-reply-address))

      ;; Optionally add a comment in the From field to show which list this is for
      ;; This helps you see which alias you're using
      (goto-char (point-min))
      (when (re-search-forward "^From: \\(.*\\) <\\(.*\\)>$" nil t)
        (replace-match (format "From: %s (via %s) <%s>"
                               user-full-name
                               my-mu4e-reply-address
                               "jens@luedicke.me"))))
    (message "Using Reply-To address: %s" my-mu4e-reply-address)))

;; Hook to set the From address when composing replies
(add-hook 'mu4e-compose-pre-hook 'my-mu4e-set-from-address)
;; Run after a short delay to ensure context switching is complete
(add-hook 'mu4e-compose-mode-hook
          (lambda ()
            (run-at-time 0.1 nil 'my-mu4e-compose-set-from)))

;; Update contexts to include SMTP settings
(setq mu4e-contexts
      `(,(make-mu4e-context
          :name "Personal"
          :match-func (lambda (msg)
                        (when msg
                          (string-prefix-p "/Personal" (mu4e-message-field msg :maildir))))
          :vars '((user-mail-address . "jens@luedicke.me")
                  (user-full-name . "Jens Luedicke")
                  (mu4e-drafts-folder . "/Personal/Drafts")
                  (mu4e-sent-folder . "/Personal/Sent")
                  (mu4e-trash-folder . "/Personal/Trash")
                  (mu4e-refile-folder . "/Personal/Archive")))
        ,(make-mu4e-context
          :name "IONOS"
          :match-func (lambda (msg)
                        (when msg
                          (string-prefix-p "/IONOS" (mu4e-message-field msg :maildir))))
          :vars '((user-mail-address . "jens@luedicke.cloud")
                  (user-full-name . "Jens Luedicke")
                  (mu4e-drafts-folder . "/IONOS/Drafts")
                  (mu4e-sent-folder . "/IONOS/Sent")
                  (mu4e-trash-folder . "/IONOS/Trash")
                  (mu4e-refile-folder . "/IONOS/Archive")))))

;; Optional: Auto-filing rules for incoming mail
;; Uncomment and customize these to automatically move messages to folders
;; (setq mu4e-headers-auto-update t)  ; Auto-update headers buffer

;; Example auto-filing with mu4e-marks
;; This runs when indexing new mail
(defun my-mu4e-auto-file ()
  "Auto-file certain messages to specific folders."
  (when (mu4e-message-field mu4e-compose-parent-message :subject)
    (let ((subject (mu4e-message-field mu4e-compose-parent-message :subject))
          (from (mu4e-message-field mu4e-compose-parent-message :from)))

      ;; Auto-file newsletters
      (when (or (string-match-p "newsletter\\|digest\\|weekly" subject)
                (string-match-p "noreply\\|no-reply\\|newsletter" (car from)))
        (mu4e-message-field mu4e-compose-parent-message :maildir "/Newsletters"))

      ;; Auto-file purchase receipts
      (when (string-match-p "order\\|receipt\\|invoice\\|purchase" subject)
        (mu4e-message-field mu4e-compose-parent-message :maildir "/Purchases")))))

;; Hook to run auto-filing after updating
;; (add-hook 'mu4e-index-updated-hook 'my-mu4e-auto-file)

;; Custom search query functions for advanced users
(defun mu4e-search-important ()
  "Search for important messages."
  (interactive)
  (mu4e-search "(flag:flagged OR prio:high OR from:/boss|manager|ceo|director/) AND NOT flag:trashed"))

(defun mu4e-search-newsletters ()
  "Search for newsletters."
  (interactive)
  (mu4e-search "(list:/.+/ OR from:/newsletter|news|digest/ OR body:/unsubscribe/) AND NOT flag:trashed"))

(defun mu4e-search-purchases ()
  "Search for purchase-related emails."
  (interactive)
  (mu4e-search "(from:/amazon|ebay|paypal|order|shop/ OR subject:/order|invoice|receipt|purchase/) AND NOT flag:trashed"))

;; Customization tips:
;; 1. To add more bookmarks, add entries to mu4e-bookmarks above
;; 2. To customize search patterns, modify the :query strings
;; 3. To change keyboard shortcuts, modify the :key values
;; 4. To add sender-specific rules, add from:/sender@domain/ patterns
;; 5. To exclude certain messages, add AND NOT conditions

;; VIP sender list example (uncomment and customize):
;; (setq my-vip-senders '("boss@company.com" "important@client.com"))
;; Then use in queries: (member-if (lambda (vip) (string-match-p vip from)) my-vip-senders)

(provide 'mu4e-config)
;;; mu4e-config.el ends here
