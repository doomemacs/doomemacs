;; -*- no-byte-compile: t; -*-
;;; lang/julia/packages.el

(package! julia-mode :pin "fe6f6f7a80f8d60ecffa5b2cb43667bb9dc11705")
(package! julia-repl :pin "79e686e3ebf164bd39fc2ea5cf09d38d0e1d763a")

(when (featurep! +lsp)
  (if (featurep! :tools lsp +eglot)
      (package! eglot-jl :pin "49f170e01c5a107c2cb662c00544d827eaa2c4d8")
    (package! lsp-julia :pin "809da95c05fe668acbae5a35e03082d9b9577728")))
