;;; module-julia.el

(use-package julia-mode
  :mode "\\.jl$"
  :interpreter "julia"
  :init
  (define-repl! julia-mode narf/julia-repl)
  (evil-set-initial-state 'inferior-julia-mode 'emacs)
  (push '(julia-mode julia-indent-offset) editorconfig-indentation-alist))

(provide 'module-julia)
;;; module-julia.el ends here
