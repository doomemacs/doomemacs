;;; ui/smooth-scroll/config.el -*- lexical-binding: t; -*-

(use-package! ultra-scroll
  :when (fboundp 'pixel-scroll-precision-mode)
  :hook (doom-first-input . ultra-scroll-mode)
  :hook (doom-first-file . ultra-scroll-mode)
  :init
  (setq scroll-conservatively 101
        scroll-margin 0)
  :config
  (add-hook 'ultra-scroll-hide-functions #'hl-todo-mode)
  (add-hook 'ultra-scroll-hide-functions #'diff-hl-flydiff-mode)
  (add-hook 'ultra-scroll-hide-functions #'jit-lock-mode))
