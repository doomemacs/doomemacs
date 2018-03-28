;;; early-init.el -*- lexical-binding: t; -*-

;; Later versions of Emacs 27 (on master) introduce a new behavior:
;;
;; Package initialize occurs automatically, before `user-init-file' is loaded,
;; but after `early-init-file'. Doom handles package initialization, so we must
;; prevent Emacs from doing it early!
(setq package-enable-at-startup nil)

;; Faster to disable these here (before they've been initialized)
(setq tool-bar-mode nil
      menu-bar-mode nil)
(set-scroll-bar-mode nil)
