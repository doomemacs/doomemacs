;;; tools/term/config.el -*- lexical-binding: t; -*-

(def-package! multi-term
  :commands (multi-term multi-term-next multi-term-prev)
  :config
  (setq multi-term-program (getenv "SHELL")
        multi-term-dedicated-window-height 20
        multi-term-switch-after-close 'PREVIOUS))
