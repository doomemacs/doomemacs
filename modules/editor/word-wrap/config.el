;;; editor/word-wrap/config.el -*- lexical-binding: t; -*-

(defvar +word-wrap-extra-indent 'double
  "The amount of extra indentation for wrapped code lines.

When 'double, indent by twice the major-mode indentation.
When 'single, indent by the major-mode indentation.
When a positive integer, indent by this fixed amount.
When a negative integer, dedent by this fixed amount.

Otherwise no extra indentation will be used.")

(defvar +word-wrap-fill-style nil
  "How to handle `fill-column' in `+word-wrap-mode'.

When 'auto, long lines will soft-wrap at `fill-column'. If `auto-fill-mode' is
enabled, its behaviour will not be affected.

When 'soft, long lines will soft-wrap at `fill-column' and `auto-fill-mode' will
be forcibly disabled.

Otherwise long lines will soft-wrap at the window margin and `auto-fill-mode'
will not be affected.")

(defvar +word-wrap-disabled-modes
  '(fundamental-mode so-long-mode)
  "Major-modes where `+global-word-wrap-mode' should not enable
`+word-wrap-mode'.")

(defvar +word-wrap-visual-modes
  '(org-mode)
  "Major-modes where `+word-wrap-mode' should not use
`adaptive-wrap-prefix-mode'.")

(defvar +word-wrap-text-modes
  '(text-mode markdown-mode markdown-view-mode gfm-mode gfm-view-mode rst-mode
    latex-mode LaTeX-mode)
  "Major-modes where `+word-wrap-mode' should not provide extra indentation.")

(when (memq 'visual-line-mode text-mode-hook)
  (remove-hook 'text-mode-hook #'visual-line-mode)
  (add-hook 'text-mode-hook #'+word-wrap-mode))
