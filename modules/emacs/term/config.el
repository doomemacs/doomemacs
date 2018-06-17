;;; emacs/term/config.el -*- lexical-binding: t; -*-

;; `multi-term'
(setq multi-term-dedicated-window-height 20
      multi-term-switch-after-close 'PREVIOUS)

;; `term' (built-in)
(after! term
  (set-env! "SHELL")

  ;; Consider term buffers real
  (add-hook 'term-mode-hook #'doom|mark-buffer-as-real))
