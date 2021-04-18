;;; term/shell/config.el -*- lexical-binding: t; -*-

;;;###package shell
(add-hook 'shell-mode-hook #'doom-mark-buffer-as-real-h)
(add-hook 'shell-mode-hook #'hide-mode-line-mode)
