;;; term/term/config.el -*- lexical-binding: t; -*-

;;;###package multi-term
(setq multi-term-dedicated-window-height 20
      multi-term-switch-after-close 'PREVIOUS)

;;;###package term
(add-hook 'term-mode-hook #'doom|mark-buffer-as-real)
