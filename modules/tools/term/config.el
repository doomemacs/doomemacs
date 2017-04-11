;;; tools/term/config.el

(def-package! multi-term
  :commands (multi-term multi-term-next multi-term-prev)
  :config
  (setq multi-term-program (getenv "SHELL")
        multi-term-switch-after-close nil)

  (add-hook 'term-mode-hook 'doom-hide-modeline-mode))
