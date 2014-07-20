(require-packages
 '(flycheck     ; syntax checker
   flyspell     ; spell checker
   ))

(diminish 'flyspell-mode " ?")

(setq ispell-program-name "aspell")
(setq ispell-list-command "--list")
(setq flycheck-indication-mode 'right-fringe)

(setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc))

(add-hook 'after-init-hook (lambda() (global-flycheck-mode 1)))
(dolist (hook '(markdown-mode-hook git-commit-mode-hook org-mode-hook))
  (add-hook hook (lambda() (flyspell-mode 1))))


;;
(provide 'mod-fly)
