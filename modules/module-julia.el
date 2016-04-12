;;; module-julia.el

(use-package julia-mode
  :mode "\\.jl$"
  :interpreter "julia"
  :init
  (define-repl! julia-mode narf/julia-repl)
  (evil-set-initial-state 'inferior-julia-mode 'emacs)
  (add-to-list 'editorconfig-indentation-alist '(julia-mode julia-indent-offset)))

(provide 'module-julia)
;;; module-julia.el ends here
