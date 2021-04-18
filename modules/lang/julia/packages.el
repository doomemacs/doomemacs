;; -*- no-byte-compile: t; -*-
;;; lang/julia/packages.el

(package! julia-mode :pin "fe6f6f7a80f8d60ecffa5b2cb43667bb9dc11705")
(package! julia-repl :pin "7ce38a9caf2a9c105afe66f464a2f30e816d69f3")

(when (featurep! +lsp)
  (if (featurep! :tools lsp +eglot)
      (package! eglot-jl :pin "84cff9d6ef1643f3eac6c9d620cc1e380a9847d9")
    (package! lsp-julia :pin "c487ed715c49d863e8a8e76d13b37b6e694520d4")))
