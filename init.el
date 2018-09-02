;;; init.el -*- lexical-binding: t; -*-
;;
;; Author:  Henrik Lissner <henrik@lissner.net>
;; URL:     https://github.com/hlissner/doom-emacs
;;
;;   =================     ===============     ===============   ========  ========
;;   \\ . . . . . . .\\   //. . . . . . .\\   //. . . . . . .\\  \\. . .\\// . . //
;;   ||. . ._____. . .|| ||. . ._____. . .|| ||. . ._____. . .|| || . . .\/ . . .||
;;   || . .||   ||. . || || . .||   ||. . || || . .||   ||. . || ||. . . . . . . ||
;;   ||. . ||   || . .|| ||. . ||   || . .|| ||. . ||   || . .|| || . | . . . . .||
;;   || . .||   ||. _-|| ||-_ .||   ||. . || || . .||   ||. _-|| ||-_.|\ . . . . ||
;;   ||. . ||   ||-'  || ||  `-||   || . .|| ||. . ||   ||-'  || ||  `|\_ . .|. .||
;;   || . _||   ||    || ||    ||   ||_ . || || . _||   ||    || ||   |\ `-_/| . ||
;;   ||_-' ||  .|/    || ||    \|.  || `-_|| ||_-' ||  .|/    || ||   | \  / |-_.||
;;   ||    ||_-'      || ||      `-_||    || ||    ||_-'      || ||   | \  / |  `||
;;   ||    `'         || ||         `'    || ||    `'         || ||   | \  / |   ||
;;   ||            .===' `===.         .==='.`===.         .===' /==. |  \/  |   ||
;;   ||         .=='   \_|-_ `===. .==='   _|_   `===. .===' _-|/   `==  \/  |   ||
;;   ||      .=='    _-'    `-_  `='    _-'   `-_    `='  _-'   `-_  /|  \/  |   ||
;;   ||   .=='    _-'          '-__\._-'         '-_./__-'         `' |. /|  |   ||
;;   ||.=='    _-'                                                     `' |  /==.||
;;   =='    _-'                                                            \/   `==
;;   \   _-'                                                                `-_   /
;;    `''                                                                      ``'
;;
;; These demons are not part of GNU Emacs.
;;
;;; License: MIT

(defvar doom-gc-cons-threshold (* 8 1024 1024)
  "The default value to use for `gc-cons-threshold'.")


(defvar doom--file-name-handler-alist file-name-handler-alist)
(unless after-init-time
  ;; A big contributor to long startup times is the garbage collector, so we up
  ;; its memory threshold, temporarily and reset it later in
  ;; `doom|disable-startup-optimizations'.
  (setq gc-cons-threshold most-positive-fixnum)
  ;; This is consulted on every `require', `load' and various file reading
  ;; functions. You get a minor speed up by nooping this.
  (setq file-name-handler-alist nil))

(defun doom|disable-startup-optimizations ()
  "Resets garbage collection settings to reasonable defaults (if you don't do
this, you'll get stuttering and random freezes) and resets
`file-name-handler-alist'."
  (setq file-name-handler-alist doom--file-name-handler-alist
        gc-cons-threshold doom-gc-cons-threshold)
  (makunbound 'doom--file-name-handler-alist))

(add-hook 'emacs-startup-hook #'doom|disable-startup-optimizations)
(add-hook 'doom-reload-hook   #'doom|disable-startup-optimizations)


;; Ensure Doom is always running out of this file's directory
(setq user-emacs-directory (file-name-directory load-file-name))
;; In noninteractive sessions, we hope that non-byte-compiled files will take
;; precedence over byte-compiled ones, however, if you're getting odd recursive
;; load errors, it may help to set this to nil.
(setq load-prefer-newer noninteractive)

;; Let 'er rip!
(require 'core (concat user-emacs-directory "core/core"))
