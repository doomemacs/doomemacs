;; -*- no-byte-compile: t; -*-
;;; lang/julia/packages.el

(package! julia-mode :pin "1c122f1dff")
(package! julia-repl :pin "5fa04de4e7")

(when (featurep! +lsp)
  (package! lsp-julia :recipe (:host github :repo "non-jedi/lsp-julia") :pin "9f158a2"))
