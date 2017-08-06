;;; private/hlissner/init.el -*- lexical-binding: t; -*-

;; I've swapped these keys on my keyboard
(setq x-super-keysym 'alt
      x-alt-keysym   'meta

      user-mail-address "henrik@lissner.net"
      user-full-name    "Henrik Lissner")

;; An extra measure to prevent the flash of unstyled mode-line while Emacs is
;; booting up (when Doom is byte-compiled).
(setq-default mode-line-format nil)

;; host-specific settings
(pcase (system-name)
  ;; ("triton")
  ((or "proteus" "halimede")
   ;; smaller screen, smaller fonts
   (set! :font "Fira Mono" :size 10)
   (set! :variable-font "Fira Sans" :size 10)
   (set! :unicode-font "DejaVu Sans Mono" :size 10)
   (setq +doom-modeline-height 25))
  ;; ("nereid")
  ;; ("io")
  ;; ("sao")
  )
