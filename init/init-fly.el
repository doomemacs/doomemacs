(provide 'init-fly)

(use-package flycheck
  :init
  (progn
    (setq-default flycheck-indication-mode 'right-fringe
                  ;; Removed checks on idle/change for snappiness
                  flycheck-check-syntax-automatically
                  '(save mode-enabled)
                  flycheck-disabled-checkers
                  '(emacs-lisp-checkdoc make))
    (add-hook 'after-init-hook #'global-flycheck-mode)))

(use-package flyspell :commands flyspell-mode)
