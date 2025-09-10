;;; portfolio-tracker-v2.el --- Portfolio tracking with transaction history -*- lexical-binding: t -*-

;;; Commentary:
;; Enhanced portfolio tracker with full transaction history and cost basis tracking
;; Supports buy/sell transactions, dividends, and multiple lots (FIFO/LIFO)

;;; Code:

(require 'cl-lib)
(require 'tabulated-list)
(require 'url)
(require 'json)

(defgroup portfolio-tracker nil
  "Portfolio tracking with live price updates."
  :group 'applications)

(defcustom portfolio-tracker-update-interval 300
  "Interval in seconds between automatic price updates."
  :type 'integer
  :group 'portfolio-tracker)

(defcustom portfolio-tracker-cost-basis-method 'fifo
  "Method for calculating cost basis: fifo or lifo."
  :type '(choice (const :tag "FIFO" fifo)
                 (const :tag "LIFO" lifo))
  :group 'portfolio-tracker)

;; Data structures
(cl-defstruct portfolio-transaction
  date
  type           ; buy, sell, dividend, split
  symbol
  shares
  price
  fees
  notes)

(cl-defstruct portfolio-lot
  symbol
  shares         ; remaining shares in this lot
  purchase-date
  purchase-price
  fees)

(cl-defstruct portfolio-holding
  symbol
  name
  total-shares
  lots           ; list of portfolio-lot structs
  average-cost
  current-price
  previous-close
  value
  unrealized-gain
  unrealized-gain-percent
  realized-gain  ; from sell transactions
  dividends      ; total dividends received
  type)          ; stock, etf, crypto

(defvar portfolio-tracker-transactions nil
  "List of all transactions.")

(defvar portfolio-tracker-holdings nil
  "Current holdings calculated from transactions.")

(defvar portfolio-tracker-prices-cache nil
  "Cache of current prices.")

;; Transaction processing
(defun portfolio-tracker-process-transactions ()
  "Process all transactions to calculate current holdings."
  (let ((holdings-table (make-hash-table :test 'equal))
        (realized-gains (make-hash-table :test 'equal))
        (dividends-table (make-hash-table :test 'equal)))
    
    ;; Sort transactions by date
    (setq portfolio-tracker-transactions
          (sort portfolio-tracker-transactions
                (lambda (a b)
                  (string< (portfolio-transaction-date a)
                          (portfolio-transaction-date b)))))
    
    ;; Process each transaction
    (dolist (txn portfolio-tracker-transactions)
      (let ((symbol (portfolio-transaction-symbol txn))
            (type (portfolio-transaction-type txn)))
        
        (cond
         ;; Buy transaction - add lot
         ((eq type 'buy)
          (let* ((holding (gethash symbol holdings-table))
                 (new-lot (make-portfolio-lot
                          :symbol symbol
                          :shares (portfolio-transaction-shares txn)
                          :purchase-date (portfolio-transaction-date txn)
                          :purchase-price (portfolio-transaction-price txn)
                          :fees (or (portfolio-transaction-fees txn) 0))))
            (if holding
                (push new-lot (portfolio-holding-lots holding))
              (puthash symbol
                      (make-portfolio-holding
                       :symbol symbol
                       :lots (list new-lot)
                       :realized-gain 0
                       :dividends 0)
                      holdings-table))))
         
         ;; Sell transaction - remove shares using FIFO/LIFO
         ((eq type 'sell)
          (let ((holding (gethash symbol holdings-table))
                (shares-to-sell (portfolio-transaction-shares txn))
                (sell-price (portfolio-transaction-price txn))
                (realized 0))
            (when holding
              (let ((lots (if (eq portfolio-tracker-cost-basis-method 'lifo)
                             (reverse (portfolio-holding-lots holding))
                           (portfolio-holding-lots holding))))
                (dolist (lot lots)
                  (when (> shares-to-sell 0)
                    (let ((shares-from-lot (min shares-to-sell
                                               (portfolio-lot-shares lot))))
                      (setq realized
                            (+ realized
                               (* shares-from-lot
                                  (- sell-price (portfolio-lot-purchase-price lot)))))
                      (setf (portfolio-lot-shares lot)
                            (- (portfolio-lot-shares lot) shares-from-lot))
                      (setq shares-to-sell (- shares-to-sell shares-from-lot)))))
                ;; Remove empty lots
                (setf (portfolio-holding-lots holding)
                      (cl-remove-if (lambda (lot) (<= (portfolio-lot-shares lot) 0))
                                    (portfolio-holding-lots holding)))
                ;; Add to realized gains
                (setf (portfolio-holding-realized-gain holding)
                      (+ (portfolio-holding-realized-gain holding) realized))))))
         
         ;; Dividend transaction
         ((eq type 'dividend)
          (let ((holding (gethash symbol holdings-table)))
            (when holding
              (setf (portfolio-holding-dividends holding)
                    (+ (portfolio-holding-dividends holding)
                       (* (portfolio-transaction-shares txn)
                          (portfolio-transaction-price txn)))))))))
    
    ;; Calculate totals for each holding
    (maphash (lambda (symbol holding)
               (let ((total-shares 0)
                     (total-cost 0))
                 (dolist (lot (portfolio-holding-lots holding))
                   (setq total-shares (+ total-shares (portfolio-lot-shares lot)))
                   (setq total-cost (+ total-cost
                                      (* (portfolio-lot-shares lot)
                                         (portfolio-lot-purchase-price lot)))))
                 (setf (portfolio-holding-total-shares holding) total-shares)
                 (setf (portfolio-holding-average-cost holding)
                       (if (> total-shares 0)
                           (/ total-cost total-shares)
                         0))))
             holdings-table)
    
    ;; Convert hash table to list
    (setq portfolio-tracker-holdings nil)
    (maphash (lambda (symbol holding)
               (when (> (portfolio-holding-total-shares holding) 0)
                 (push holding portfolio-tracker-holdings)))
             holdings-table)))

;; Yahoo Finance integration
(defun portfolio-tracker-fetch-price (symbol callback)
  "Fetch current price for SYMBOL and call CALLBACK with the result."
  (let ((url (format "https://query1.finance.yahoo.com/v8/finance/chart/%s" symbol)))
    (url-retrieve url
                  (lambda (status)
                    (if (plist-get status :error)
                        (message "Error fetching price for %s" symbol)
                      (goto-char (point-min))
                      (re-search-forward "^$")
                      (let* ((json-object-type 'plist)
                             (json-array-type 'list)
                             (json (json-read))
                             (result (plist-get json :chart))
                             (data (car (plist-get result :result)))
                             (meta (plist-get data :meta))
                             (price (plist-get meta :regularMarketPrice))
                             (prev-close (plist-get meta :previousClose))
                             (name (plist-get meta :longName)))
                        (funcall callback symbol price prev-close name))))
                  nil t)))

(defun portfolio-tracker-update-prices ()
  "Update prices for all holdings."
  (interactive)
  (message "Updating prices...")
  (dolist (holding portfolio-tracker-holdings)
    (portfolio-tracker-fetch-price
     (portfolio-holding-symbol holding)
     (lambda (symbol price prev-close name)
       (let ((h (cl-find symbol portfolio-tracker-holdings
                        :key #'portfolio-holding-symbol
                        :test #'string=)))
         (when h
           (setf (portfolio-holding-current-price h) price)
           (setf (portfolio-holding-previous-close h) prev-close)
           (when name
             (setf (portfolio-holding-name h) name))
           ;; Calculate unrealized gains
           (let ((total-cost 0))
             (dolist (lot (portfolio-holding-lots h))
               (setq total-cost (+ total-cost
                                  (* (portfolio-lot-shares lot)
                                     (portfolio-lot-purchase-price lot)))))
             (setf (portfolio-holding-value h)
                   (* (portfolio-holding-total-shares h) price))
             (setf (portfolio-holding-unrealized-gain h)
                   (- (portfolio-holding-value h) total-cost))
             (setf (portfolio-holding-unrealized-gain-percent h)
                   (if (> total-cost 0)
                       (* 100 (/ (portfolio-holding-unrealized-gain h) total-cost))
                     0))))
       (portfolio-tracker-refresh-display))))))

;; Display functions
(eval-and-compile
  (defvar portfolio-tracker-mode-map
    (let ((map (make-sparse-keymap)))
      ;; Use string keys directly for single characters
      (define-key map "g" 'portfolio-tracker-refresh)
      (define-key map "a" 'portfolio-tracker-add-transaction)
      (define-key map "t" 'portfolio-tracker-show-transactions)
      (define-key map "h" 'portfolio-tracker-refresh-display)
      (define-key map "s" 'portfolio-tracker-save)
      (define-key map "l" 'portfolio-tracker-load)
      (define-key map "r" 'portfolio-tracker-refresh)
      (define-key map "q" 'quit-window)
      map)
    "Keymap for portfolio-tracker-mode.")
  
  (define-derived-mode portfolio-tracker-mode tabulated-list-mode "Portfolio"
    "Major mode for tracking investment portfolio.
\\{portfolio-tracker-mode-map}"
    :keymap portfolio-tracker-mode-map
    ;; Disable CUA mode to allow single-key commands
    (setq-local cua-mode nil)
    (setq-local cua-enable-cua-keys nil)
    (setq tabulated-list-format
          [("Symbol" 8 t)
           ("Name" 20 t)
           ("Shares" 10 t)
           ("Avg Cost" 10 nil)
           ("Current" 10 nil)
           ("Value" 12 nil)
           ("Unrealized" 12 nil)
           ("Realized" 10 nil)
           ("Dividends" 10 nil)
           ("Total %" 8 nil)])
    (setq tabulated-list-padding 2)
    (setq tabulated-list-sort-key (cons "Symbol" nil))
    (tabulated-list-init-header)))

(define-derived-mode portfolio-transactions-mode tabulated-list-mode "Transactions"
  "Major mode for viewing portfolio transactions."
  (setq tabulated-list-format
        [("Date" 12 t)
         ("Type" 8 t)
         ("Symbol" 8 t)
         ("Shares" 10 nil)
         ("Price" 10 nil)
         ("Total" 12 nil)
         ("Fees" 8 nil)
         ("Notes" 30 nil)])
  (setq tabulated-list-padding 2)
  (setq tabulated-list-sort-key (cons "Date" t))
  (tabulated-list-init-header))

(defun portfolio-tracker-refresh-display ()
  "Refresh the holdings display."
  (interactive)
  (let ((buf (get-buffer "*Portfolio Holdings*")))
    (when buf
      (with-current-buffer buf
        (let ((inhibit-read-only t))
          (setq tabulated-list-entries
                (mapcar (lambda (holding)
                     (list holding
                           (vector
                            (portfolio-holding-symbol holding)
                            (or (portfolio-holding-name holding) "")
                            (format "%.2f" (portfolio-holding-total-shares holding))
                            (format "$%.2f" (portfolio-holding-average-cost holding))
                            (if (portfolio-holding-current-price holding)
                                (format "$%.2f" (portfolio-holding-current-price holding))
                              "...")
                            (format "$%.2f" (or (portfolio-holding-value holding) 0))
                            (propertize (format "$%.2f" (or (portfolio-holding-unrealized-gain holding) 0))
                                       'face (if (>= (or (portfolio-holding-unrealized-gain holding) 0) 0)
                                                'success 'error))
                            (format "$%.2f" (portfolio-holding-realized-gain holding))
                            (format "$%.2f" (portfolio-holding-dividends holding))
                            (propertize (format "%.1f%%" (or (portfolio-holding-unrealized-gain-percent holding) 0))
                                       'face (if (>= (or (portfolio-holding-unrealized-gain-percent holding) 0) 0)
                                                'success 'error)))))
                       portfolio-tracker-holdings))
          (tabulated-list-print t)
          ;; Add summary at the bottom
          (portfolio-tracker-display-summary)))))))

(defun portfolio-tracker-display-summary ()
  "Display portfolio summary with totals at bottom of buffer."
  (let ((inhibit-read-only t)
        (total-cost 0)
        (total-value 0)
        (total-unrealized 0)
        (total-realized 0)
        (total-dividends 0))
    ;; Calculate totals
    (dolist (holding portfolio-tracker-holdings)
      (let ((cost (* (portfolio-holding-total-shares holding)
                     (portfolio-holding-average-cost holding))))
        (setq total-cost (+ total-cost cost))
        (setq total-value (+ total-value (or (portfolio-holding-value holding) cost)))
        (setq total-unrealized (+ total-unrealized (or (portfolio-holding-unrealized-gain holding) 0)))
        (setq total-realized (+ total-realized (portfolio-holding-realized-gain holding)))
        (setq total-dividends (+ total-dividends (portfolio-holding-dividends holding)))))
    
    ;; Display summary
    (goto-char (point-max))
    (insert "\n" (make-string 100 ?-) "\n")
    (insert (propertize "PORTFOLIO TOTALS\n" 'face 'bold))
    (insert (format "  Total Buy-In (Cost Basis):  %s%.2f\n" 
                    (propertize "$" 'face 'default)
                    total-cost))
    (insert (format "  Current Market Value:       %s%.2f\n"
                    (propertize "$" 'face 'default)
                    total-value))
    (insert (format "  Unrealized Gain/Loss:       %s\n"
                    (propertize (format "$%.2f (%.1f%%)" 
                                       total-unrealized
                                       (if (> total-cost 0)
                                           (* 100 (/ total-unrealized total-cost))
                                         0))
                               'face (if (>= total-unrealized 0) 'success 'error))))
    (insert (format "  Realized Gain/Loss:         %s\n"
                    (propertize (format "$%.2f" total-realized)
                               'face (if (>= total-realized 0) 'success 'error))))
    (insert (format "  Total Dividends Received:   %s%.2f\n"
                    (propertize "$" 'face 'default)
                    total-dividends))
    (insert (make-string 100 ?-) "\n")
    (let ((total-gain (+ total-unrealized total-realized total-dividends))
          (total-return-pct (if (> total-cost 0)
                               (* 100 (/ (+ total-unrealized total-dividends) total-cost))
                             0)))
      (insert (format "  TOTAL RETURN:               %s\n"
                     (propertize (format "$%.2f (%.1f%%)" 
                                        total-gain
                                        total-return-pct)
                                'face (if (>= total-gain 0) 'success 'error))))
      (insert (make-string 100 ?=) "\n"))
    (insert "\nKeys: [g] Refresh | [a] Add Transaction | [t] Show Transactions | [s] Save | [l] Load | [q] Quit\n")))

(defun portfolio-tracker-refresh ()
  "Refresh prices and redisplay."
  (interactive)
  (portfolio-tracker-process-transactions)
  (portfolio-tracker-update-prices)
  (portfolio-tracker-refresh-display))

(defun portfolio-tracker-add-transaction ()
  "Add a new transaction interactively."
  (interactive)
  (let* ((date (read-string "Date (YYYY-MM-DD): " (format-time-string "%Y-%m-%d")))
         (type (intern (completing-read "Type: " '("buy" "sell" "dividend") nil t)))
         (symbol (upcase (read-string "Symbol: ")))
         (shares (read-number "Shares: "))
         (price (read-number "Price per share: "))
         (fees (read-number "Fees (0 if none): " 0))
         (notes (read-string "Notes (optional): ")))
    (push (make-portfolio-transaction
           :date date
           :type type
           :symbol symbol
           :shares shares
           :price price
           :fees fees
           :notes notes)
          portfolio-tracker-transactions)
    (portfolio-tracker-process-transactions)
    (portfolio-tracker-update-prices)))

(defun portfolio-tracker-show-transactions ()
  "Show all transactions in a separate buffer."
  (interactive)
  (let ((buf (get-buffer-create "*Portfolio Transactions*")))
    (with-current-buffer buf
      (portfolio-transactions-mode)
      (setq tabulated-list-entries
            (mapcar (lambda (txn)
                     (list txn
                           (vector
                            (portfolio-transaction-date txn)
                            (symbol-name (portfolio-transaction-type txn))
                            (portfolio-transaction-symbol txn)
                            (format "%.2f" (portfolio-transaction-shares txn))
                            (format "$%.2f" (portfolio-transaction-price txn))
                            (format "$%.2f" (* (portfolio-transaction-shares txn)
                                             (portfolio-transaction-price txn)))
                            (format "$%.2f" (or (portfolio-transaction-fees txn) 0))
                            (or (portfolio-transaction-notes txn) ""))))
                   (reverse portfolio-tracker-transactions)))
      (tabulated-list-print))
    (switch-to-buffer buf)))

(defun portfolio-tracker-save (file)
  "Save portfolio data to FILE."
  (interactive "FSave portfolio to: ")
  (with-temp-file file
    (insert ";; Portfolio Tracker Data\n")
    (insert ";; Transaction history and settings\n\n")
    (insert "(setq portfolio-tracker-transactions\n")
    (pp portfolio-tracker-transactions (current-buffer))
    (insert ")\n"))
  (message "Portfolio saved to %s" file))

(defun portfolio-tracker-load (file)
  "Load portfolio data from FILE."
  (interactive "fLoad portfolio from: ")
  (load-file file)
  (portfolio-tracker-process-transactions)
  (portfolio-tracker-refresh-display)
  (portfolio-tracker-update-prices)
  (message "Portfolio loaded from %s" file))

;;;###autoload
(defun portfolio-tracker ()
  "Start the portfolio tracker."
  (interactive)
  (require 'tabulated-list)
  (require 'cl-lib)
  (let ((buf (get-buffer-create "*Portfolio Holdings*")))
    (switch-to-buffer buf)
    (unless (eq major-mode 'portfolio-tracker-mode)
      (portfolio-tracker-mode))
    (when portfolio-tracker-transactions
      (portfolio-tracker-process-transactions)
      (portfolio-tracker-refresh-display))
    (message "Portfolio Tracker: 'a' add transaction | 't' show transactions | 'g' refresh prices | 'l' load portfolio")))

(provide 'portfolio-tracker-v2)
;;; portfolio-tracker-v2.el ends here
