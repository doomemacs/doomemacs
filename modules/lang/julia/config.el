;;; lang/julia/config.el

(use-package julia-mode
  :mode "\\.jl$"
  :interpreter "julia"
  :config
  (set! :repl 'julia-mode '+julia/repl))

