;;; private/hlissner/init.el -*- lexical-binding: t; -*-

;; I've swapped these keys on my keyboard
(setq x-super-keysym 'alt
      x-alt-keysym   'meta)

;; This is a special file, unique to private modules, that is loaded after DOOM
;; core but before any module is activated, giving you an opportunity to
;; overwrite variables or settings before initialization.

;; host-specific settings
(load "~/.emacs.local.el" t t)

(setq user-mail-address "henrik@lissner.net"
      user-full-name    "Henrik Lissner")

(pcase (system-name)
  ("proteus"
   (setq +doom-modeline-height 25
         +doom-font (font-spec :family "Fira Mono" :size 10)
         +doom-variable-pitch-font (font-spec :family "Fira Sans" :size 10)
         +doom-unicode-font (font-spec :family "DejaVu Sans Mono" :size 10)
         nlinum-format "%3d "))
  ("halimede"
   (setq +doom-modeline-height 27)))
