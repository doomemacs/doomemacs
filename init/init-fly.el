(provide 'init-fly)

(use-package flycheck
  :config
  (setq flycheck-indication-mode 'right-fringe
        ;; Removed checks on idle/change for snappiness
        flycheck-check-syntax-automatically
        '(save new-line mode-enabled)
        flycheck-disabled-checkers
        '(emacs-lisp emacs-lisp-checkdoc))
  :idle
  (add-hook 'after-init-hook #'global-flycheck-mode))

(use-package flyspell
  :commands flyspell-mode
  :diminish (flyspell-mode . " @")
  :config
  (setq ispell-program-name "aspell"
        ispell-list-command "--list"))
