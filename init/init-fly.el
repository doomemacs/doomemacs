(provide 'init-fly)

(use-package flycheck
  :init
  (progn
    (setq-default flycheck-indication-mode 'right-fringe
                  ;; Removed checks on idle/change for snappiness
                  flycheck-check-syntax-automatically
                  '(save new-line mode-enabled)
                  flycheck-disabled-checkers
                  '(emacs-lisp emacs-lisp-checkdoc))
    (add-hook 'after-init-hook #'global-flycheck-mode)))

(use-package flyspell
  :commands flyspell-mode
  :config
  (setq ispell-program-name "aspell"
        ispell-list-command "--list"))
