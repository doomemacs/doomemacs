;;; early-init.el -*- lexical-binding: t; -*-

;; Emacs HEAD (27+) introduces early-init.el, which is run before init.el,
;; before most of its package and UI initialization happens. We can use this
;; opportunity to cull parts of the startup process early.

;; Package initialize occurs automatically, before `user-init-file' is loaded,
;; but after `early-init-file'. Doom handles package initialization, so we must
;; prevent Emacs from doing it early!
(setq package-enable-at-startup nil)

;; Faster to disable these here (before they've been initialized)
(setq tool-bar-mode nil
      menu-bar-mode nil)
(set-scroll-bar-mode nil)

;; TODO Once Emacs 27 hits stable, perhaps replace init.el with early-init.el
