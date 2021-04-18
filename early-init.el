;;; early-init.el -*- lexical-binding: t; -*-

;; Emacs 27.1 introduced early-init.el, which is run before init.el, before
;; package and UI initialization happens, and before site files are loaded.

;; A big contributor to startup times is garbage collection. We up the gc
;; threshold to temporarily prevent it from running, then reset it later by
;; enabling `gcmh-mode'. Not resetting it will cause stuttering/freezes.
(setq gc-cons-threshold most-positive-fixnum)

;; In noninteractive sessions, prioritize non-byte-compiled source files to
;; prevent the use of stale byte-code. Otherwise, it saves us a little IO time
;; to skip the mtime checks on every *.elc file.
(setq load-prefer-newer noninteractive)

;; In Emacs 27+, package initialization occurs before `user-init-file' is
;; loaded, but after `early-init-file'. Doom handles package initialization, so
;; we must prevent Emacs from doing it early!
(setq package-enable-at-startup nil)
(fset #'package--ensure-init-file #'ignore)  ; DEPRECATED Removed in 28

;; `file-name-handler-alist' is consulted on every `require', `load' and various
;; path/io functions. You get a minor speed up by nooping this. However, this
;; may cause problems on builds of Emacs where its site lisp files aren't
;; byte-compiled and we're forced to load the *.el.gz files (e.g. on Alpine)
(unless (daemonp)
  (defvar doom--initial-file-name-handler-alist file-name-handler-alist)
  (setq file-name-handler-alist nil)
  ;; Restore `file-name-handler-alist' later, because it is needed for handling
  ;; encrypted or compressed files, among other things.
  (defun doom-reset-file-handler-alist-h ()
    ;; Re-add rather than `setq', because changes to `file-name-handler-alist'
    ;; since startup ought to be preserved.
    (dolist (handler file-name-handler-alist)
      (add-to-list 'doom--initial-file-name-handler-alist handler))
    (setq file-name-handler-alist doom--initial-file-name-handler-alist))
  (add-hook 'emacs-startup-hook #'doom-reset-file-handler-alist-h))

;; Ensure Doom is running out of this file's directory
(setq user-emacs-directory (file-name-directory load-file-name))

;; Load the heart of Doom Emacs
(load (concat user-emacs-directory "core/core") nil 'nomessage)
