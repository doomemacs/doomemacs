;;; tools/direnv/config.el -*- lexical-binding: t; -*-

(def-package! direnv
  :after-call (after-find-file dired-initial-position-hook)
  :config
  (when (executable-find "direnv")
    (direnv-mode +1)))
