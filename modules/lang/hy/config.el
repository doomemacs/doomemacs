;;; lang/hy/config.el -*- lexical-binding: t; -*-

(use-package! hy-mode
  :mode "\\.hy\\'"
  :interpreter "hy"
  :config
  (set-repl-handler! 'hy-mode #'hy-shell-start-or-switch-to-shell)
  (set-formatter! 'lisp-indent #'apheleia-indent-lisp-buffer :modes '(hy-mode))
  (set-company-backend! 'hy-mode 'company-hy))
