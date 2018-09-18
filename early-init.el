;;; early-init.el -*- lexical-binding: t; -*-

;; Emacs HEAD (27+) introduces early-init.el, which is run before init.el,
;; before package and UI initialization happens.


(defconst doom-gc-cons-upper-limit 268435456 ; 256mb
          "The temporary value for `gc-cons-threshold' to defer it.")
;; This reduces gcs-done from 4 to 1 on startup
(setq gc-cons-threshold doom-gc-cons-upper-limit)

;; Package initialize occurs automatically, before `user-init-file' is
;; loaded, but after `early-init-file'. Doom handles package
;; initialization, so we must prevent Emacs from doing it early!
(setq package-enable-at-startup nil)

;; Prevent the glimpse of un-styled Emacs by setting these early.
(add-to-list 'default-frame-alist '(tool-bar-lines 0))
(add-to-list 'default-frame-alist '(menu-bar-lines 0))
(add-to-list 'default-frame-alist '(vertical-scroll-bars))
