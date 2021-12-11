;; -*- no-byte-compile: t; -*-
;;; ui/pixel-scroll/packages.el

(when (not (or (> emacs-major-version 28)  (boundp 'mac-mouse-wheel-smooth-scroll)))
  (package! good-scroll :pin "8530d6697b15e0"))
