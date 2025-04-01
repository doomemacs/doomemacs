;;; ui/smooth-scroll/config.el -*- lexical-binding: t; -*-

(use-package! ultra-scroll
  :hook (doom-first-input . ultra-scroll-mode)
  :hook (doom-first-file . ultra-scroll-mode)
  :init
  (setq scroll-conservatively 101
        scroll-margin 0))
