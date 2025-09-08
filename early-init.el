;;; early-init.el --- Early initialization settings -*- lexical-binding: t -*-
;;; Commentary:
;;; This file is loaded before the package system and GUI is initialized.
;;; Use it for settings that need to be set very early in the startup process.

;;; Code:

;; Defer garbage collection further back in the startup process
;; This makes startup faster by preventing frequent GC
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

;; Store default values to restore after init
(defvar default-gc-cons-threshold 16777216) ; 16MB is a good default
(defvar default-gc-cons-percentage 0.1)

;; In Emacs 27+, package initialization occurs before `user-init-file' is loaded,
;; but after `early-init-file'. We handle package initialization ourselves.
(setq package-enable-at-startup nil)

;; Prevent the glimpse of un-styled Emacs by disabling these UI elements early.
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

;; Resizing the Emacs frame can be a terribly expensive part of changing the
;; font. By inhibiting this, we easily halve startup times with fonts that are
;; larger than the system default.
(setq frame-inhibit-implied-resize t)

;; Native compilation settings for Emacs 28+
(when (and (fboundp 'native-comp-available-p)
           (native-comp-available-p))
  ;; Silence compiler warnings as they can be pretty disruptive
  (setq native-comp-async-report-warnings-errors nil)
  ;; Make native compilation happen asynchronously
  (setq native-comp-deferred-compilation t)
  ;; Set the right directory for native compilation cache
  (add-to-list 'native-comp-eln-load-path (expand-file-name "eln-cache/" user-emacs-directory))
  ;; Compile AOT all the .el files in the configuration
  (setq native-comp-speed 2)  ; Max optimization
  (setq native-comp-jit-compilation t))  ; JIT compile loaded files

;; Disable some warnings during initialization
(setq byte-compile-warnings '(not obsolete))
(setq warning-suppress-log-types '((comp) (bytecomp)))

(provide 'early-init)
;;; early-init.el ends here