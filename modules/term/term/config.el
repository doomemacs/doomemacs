;;; term/term/config.el -*- lexical-binding: t; -*-

;;;###package term
(add-hook 'term-mode-hook #'doom-mark-buffer-as-real-h)
(add-hook 'term-mode-hook #'hide-mode-line-mode)


;;;###package multi-term
(setq multi-term-dedicated-window-height 20
      multi-term-switch-after-close 'PREVIOUS)

;; REVIEW Fixes 'multi-term-switch-buffer: Symbol’s function definition is void:
;;        tramp-tramp-file-p' error. Remove this once
;;        manateelazycat/multi-term#2 is merged.
(after! multi-term (require 'tramp))
