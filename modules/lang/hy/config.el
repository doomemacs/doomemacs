;;; lang/hy/config.el -*- lexical-binding: t; -*-

(def-package! hy-mode
  :mode "\\.hy\\'"
  :interpreter "hy"
  :config
  (set-repl-handler! 'hy-mode #'hy-shell-start-or-switch-to-shell)
  (set-company-backend! 'hy-mode 'company-hy))
