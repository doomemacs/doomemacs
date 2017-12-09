;;; ui/vi-tilde-fringe/config.el -*- lexical-binding: t; -*-

;; indicators for empty lines past EOF
(def-package! vi-tilde-fringe
  :commands (global-vi-tilde-fringe-mode vi-tilde-fringe-mode)
  :hook (doom-init-ui . global-vi-tilde-fringe-mode)
  :init
  :config
  (defun +vi-tilde-fringe|disable ()
    (vi-tilde-fringe-mode -1)))

