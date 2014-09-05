(provide 'init-fly)

(use-package flycheck
    :init (add-hook 'after-init-hook #'global-flycheck-mode)
    :config
    (progn
      ;; Removed checks on idle/change for snappiness
      (setq flycheck-check-syntax-automatically '(save new-line mode-enabled))
      (setq flycheck-indication-mode 'right-fringe)
      (setq-default flycheck-disabled-checkers '(emacs-lisp emacs-lisp-checkdoc))))

(use-package flyspell
    :commands flyspell-mode
    :diminish (flyspell-mode . " @")
    :config
    (setq ispell-program-name "aspell"
          ispell-list-command "--list"))
