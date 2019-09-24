;;; editor/god/config.el -*- lexical-binding: t; -*-

(use-package! god-mode
  :commands (god-local-mode god-mode-all)
  :hook ((after-init . god-mode-all)
         (post-command . god--configure-cursor-and-modeline)
         (overwrite-mode . god--toggle-on-overwrite))
  :config
  (setq god-exempt-major-modes god-default-exempt-major-modes))
