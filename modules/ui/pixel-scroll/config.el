;;; ui/pixel-scroll/config.el -*- lexical-binding: t; -*-

(if (boundp 'mac-mouse-wheel-smooth-scroll)
  (setq  mac-mouse-wheel-smooth-scroll t)
  (if (> emacs-major-version 28)
    (pixel-scroll-precision-mode)
    (use-package! good-scroll
      :config
      (good-scroll-mode 1))))
