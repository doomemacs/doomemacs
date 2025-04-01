;; -*- no-byte-compile: t; -*-
;;; lang/julia/packages.el

(package! julia-mode :pin "0f4d74f9049df28e2f522733141bfc5b7a0f69a3")
(package! julia-repl :pin "317d56021889a336b4be241604ba71e46dc80581")

(when (modulep! +lsp)
  (if (modulep! :tools lsp +eglot)
      (package! eglot-jl :pin "7c968cc61fb64016ebe6dc8ff83fd05923db4374")
    (package! lsp-julia :pin "c869b2f6c05a97e5495ed3cc6710a33b4faf41a2")))

(when (modulep! +snail)
  (package! julia-snail :pin "24f79a067bfab177239833bb7d1d317b89d36741"))
