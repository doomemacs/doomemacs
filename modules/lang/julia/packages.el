;; -*- no-byte-compile: t; -*-
;;; lang/julia/packages.el

(package! julia-mode :pin "8ea90c7927f6d87a291cfb0216f34dacf43c722e")
(package! julia-repl :pin "7ce38a9caf2a9c105afe66f464a2f30e816d69f3")

(when (featurep! +lsp)
  (if (featurep! :tools lsp +eglot)
      (package! eglot-jl :pin "84cff9d6ef1643f3eac6c9d620cc1e380a9847d9")
    (package! lsp-julia :pin "c523c250c4bd2777203101ab417e9b7312472f46")))
