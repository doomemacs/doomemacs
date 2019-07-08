;;; tools/direnv/config.el -*- lexical-binding: t; -*-

(def-package! direnv
  :after-call (after-find-file dired-initial-position-hook)
  :config
  (defun +direnv|init ()
    "Instead of checking for direnv on `post-command-hook', check on
buffer/window/frame switch, which is less expensive."
    (direnv--disable)
    (when direnv-mode
      (add-hook 'doom-switch-buffer-hook #'direnv--maybe-update-environment)
      (add-hook 'doom-switch-window-hook #'direnv--maybe-update-environment)
      (add-hook 'focus-in-hook #'direnv--maybe-update-environment)))
  (add-hook 'direnv-mode-hook #'+direnv|init)

  (when (executable-find "direnv")
    (direnv-mode +1)))
