;;; init-seq-fix.el --- Fix seq library issues -*- lexical-binding: t -*-
;;; Commentary:
;;; Fix for cl-no-applicable-method errors with seq-empty-p
;;; This particularly affects Emacs 31 development versions

;;; Code:

;; Ensure seq library is properly loaded
(require 'seq)
(require 'cl-lib)
(require 'cl-generic)  ; Needed for method dispatch in Emacs 31

;; Fix potential issues with seq-empty-p being called on strings
;; This can happen with input methods or certain packages
(defadvice seq-empty-p (before fix-string-input activate)
  "Handle string inputs that should be converted to sequences."
  (when (and (ad-get-arg 0)
             (stringp (ad-get-arg 0))
             (not (sequencep (ad-get-arg 0))))
    (ad-set-arg 0 (append (ad-get-arg 0) nil))))

;; Alternative fix: Override seq-empty-p to handle strings properly
(with-eval-after-load 'seq
  (defun seq-empty-p-fixed (orig-fun sequence)
    "Fixed version of seq-empty-p that handles strings."
    (cond
     ((stringp sequence) (string-empty-p sequence))
     ((sequencep sequence) (funcall orig-fun sequence))
     (t (funcall orig-fun sequence))))
  
  ;; Only apply if we're having issues
  (when (condition-case nil
            (progn (seq-empty-p "test") nil)
          (error t))
    (advice-add 'seq-empty-p :around #'seq-empty-p-fixed)))

;; Ensure proper loading order for seq-related packages
(with-eval-after-load 'company
  (require 'seq))

(with-eval-after-load 'lsp-mode
  (require 'seq))

;; Fix for input method issues that might trigger this error
(setq default-input-method nil)

;; Specific fix for the "latin" string issue
;; This often comes from font or input method configurations
(with-eval-after-load 'mule
  (when (and (boundp 'current-input-method)
             (stringp current-input-method)
             (string= current-input-method "latin"))
    (setq current-input-method nil)))

;; Prevent problematic input method activation
(defadvice activate-input-method (before check-input-method activate)
  "Prevent activation of problematic input methods."
  (when (and (ad-get-arg 0)
             (stringp (ad-get-arg 0))
             (string= (ad-get-arg 0) "latin"))
    (ad-set-arg 0 nil)))

;; Ensure strings are properly handled in various contexts
(defun ensure-seq-compatibility ()
  "Ensure seq library compatibility across Emacs."
  (unless (fboundp 'string-empty-p)
    (defun string-empty-p (string)
      "Check whether STRING is empty."
      (string= string ""))))

(ensure-seq-compatibility)

(provide 'init-seq-fix)
;;; init-seq-fix.el ends here