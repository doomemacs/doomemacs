(require-packages
 '(flycheck     ; syntax checker
   flyspell     ; spell checker
   ))

(setq ispell-program-name "aspell")
(setq ispell-list-command "--list")
(setq flycheck-indication-mode 'right-fringe)

(setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc))

(add-hook 'prog-mode-hook #'global-flycheck-mode)
(add-hook 'text-mode-hook (lambda () (flyspell-mode 1)))
(add-hook 'conf-mode-hook (lambda () (flyspell-mode 1)))

;;
(provide 'mod-fly)
