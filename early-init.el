;;; early-init.el -*- lexical-binding: t; -*-

;; Emacs HEAD (27+) introduces early-init.el, which is run before init.el,
;; before package and UI initialization happens. We can use this opportunity to
;; cull parts of the startup process early and optimize Emacs startup ASAP.

(unless noninteractive
  (defvar doom--file-name-handler-alist
    file-name-handler-alist)
  (unless after-init-time
    ;; A big contributor to long startup times is the garbage collector, so we
    ;; up its memory threshold, temporarily and reset it later in
    ;; `doom|finalize'.
    (setq gc-cons-threshold 402653184
          gc-cons-percentage 1.0
          ;; consulted on every `require', `load' and various file reading
          ;; functions. You get a minor speed up by nooping this.
          file-name-handler-alist nil))

  (defun doom|finalize ()
    "Resets garbage collection settings to reasonable defaults (if you don't do
this, you'll get stuttering and random freezes) and resets
`file-name-handler-alist'."
    (setq file-name-handler-alist doom--file-name-handler-alist
          gc-cons-threshold 16777216
          gc-cons-percentage 0.15))

  (add-hook 'emacs-startup-hook #'doom|finalize)
  (add-hook 'doom-reload-hook   #'doom|finalize))

;; Ensure Doom is always running out of this file's directory
(setq user-emacs-directory (file-name-directory load-file-name)
      ;; In noninteractive sessions, we hope that non-byte-compiled files will
      ;; take precedence over byte-compiled ones, however, if you're getting odd
      ;; recursive load errors, it may help to set this to nil.
      load-prefer-newer noninteractive
      ;; Package initialize occurs automatically, before `user-init-file' is
      ;; loaded, but after `early-init-file'. Doom handles package
      ;; initialization, so we must prevent Emacs from doing it early!
      package-enable-at-startup nil)

;; Prevent the glimpse of un-styled Emacs by setting these early.
(if (fboundp 'tool-bar-mode)   (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode)   (menu-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; TODO Once Emacs 27 hits stable, perhaps replace init.el with early-init.el
