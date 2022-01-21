;;; config/tutorial/config.el -*- lexical-binding: t; -*-

(use-package! evil-tutor
  :commands evil-tutor-start
  :config
  (setq evil-tutor-working-directory (expand-file-name "tutor" doom-cache-dir)))
