;;; module-julia.el

(use-package julia-mode
  :mode "\\.jl$"
  :interpreter "julia"
  :config (def-repl! julia-mode narf/julia-repl))

(provide 'module-julia)
;;; module-julia.el ends here
