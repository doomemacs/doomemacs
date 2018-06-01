;;; ui/vinegar/config.el -*- lexical-binding: t; -*-

(defun +vinegar|dired-setup ()
  "Setup custom dired settings for vinegar"
  (setq dired-omit-verbose nil)
  (setq dired-hide-details-hide-symlink-targets nil)
  (make-local-variable 'dired-hide-symlink-targets)
  (dired-hide-details-mode t))

(after! dired
  :config
  (add-hook 'dired-mode-hook '+vinegar|dired-setup))

(map! :n "-" 'dired-jump)
