;;; ui/vi-tilde-fringe/config.el -*- lexical-binding: t; -*-

;; indicators for empty lines past EOF
(def-package! vi-tilde-fringe
  :commands (global-vi-tilde-fringe-mode vi-tilde-fringe-mode)
  :init
  (add-hook 'doom-init-ui-hook #'global-vi-tilde-fringe-mode)
  :config
  (defun +vi-tilde-fringe|disable ()
    (vi-tilde-fringe-mode -1)))

