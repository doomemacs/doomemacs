;;; editor/god/config.el -*- lexical-binding: t; -*-

(use-package! god-mode
  :commands god-local-mode
  :hook (doom-after-init-modules . god-mode-all)
  :config
  (setq god-exempt-major-modes +god-default-exempt-major-modes)
  (add-hook! 'post-command-hook #'+god--configure-cursor-and-modeline-h)
  (add-hook! 'overwrite-mode-hook #'+god--toggle-on-overwrite-h))
