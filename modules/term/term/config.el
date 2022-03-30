;;; term/term/config.el -*- lexical-binding: t; -*-

;;;###package term
(add-hook 'term-mode-hook #'doom-mark-buffer-as-real-h)
(add-hook 'term-mode-hook #'hide-mode-line-mode)


;;;###package multi-term
(setq multi-term-dedicated-window-height 20
      multi-term-switch-after-close 'PREVIOUS
      multi-term-buffer-name "doom:terminal")

;; Remove hscroll-margin in shells, otherwise you get jumpiness when the cursor
;; comes close to the left/right edges of the window.
(setq-hook! 'term-mode-hook hscroll-margin 0)
