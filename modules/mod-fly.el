(require-packages
 '(flycheck     ; syntax checker
   flyspell     ; spell checker
   ))

;;;#flyspell
(diminish 'flyspell-mode " ?")
(setq ispell-program-name "aspell")
(setq ispell-list-command "--list")
(dolist (hook '(markdown-mode-hook git-commit-mode-hook org-mode-hook))
  (add-hook hook (lambda() (flyspell-mode 1))))

;;;#flycheck
;; Removed checks on idle/change for snappiness
(setq flycheck-check-syntax-automatically '(save new-line mode-enabled))
(setq flycheck-indication-mode 'right-fringe)
(setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc))

(global-flycheck-mode 1)

;;
(provide 'mod-fly)
