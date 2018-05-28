;;; emacs/term/config.el -*- lexical-binding: t; -*-

;; `multi-term'
(setq multi-term-dedicated-window-height 20
      multi-term-switch-after-close 'PREVIOUS)

;; `term' (built-in)
(after! term
  (set! :env "SHELL")

  ;; Consider term buffers real
  (defun +term-p (buf)
    (eq (buffer-local-value 'major-mode buf) 'term-mode))
  (add-to-list 'doom-real-buffer-functions #'+term-p nil #'eq))
