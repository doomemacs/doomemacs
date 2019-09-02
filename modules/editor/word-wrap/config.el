;;; editor/word-wrap/config.el -*- lexical-binding: t; -*-

(defvar +word-wrap-extra-indent 'double
  "The amount of extra indentation for wrapped non-comment lines.

When 'double, indent by twice the major-mode indentation.
When 'single, indent by the major-mode indentation.
When a positive integer, indent by this fixed amount.
When a negative integer, dedent by this fixed amount.
Otherwise no extra indentation will be used.")

(defvar +word-wrap-disabled-modes
  '(fundamental-mode so-long-mode)
  "Major-modes where `+global-word-wrap-mode' should not enable
  `+word-wrap-mode'.")
