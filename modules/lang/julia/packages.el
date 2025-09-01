;; -*- no-byte-compile: t; -*-
;;; lang/julia/packages.el

(package! julia-mode :pin "5c940c4ba357d8361534f11169f3d40b2d7833fc")
(package! julia-repl :pin "317d56021889a336b4be241604ba71e46dc80581")

(when (and (modulep! +tree-sitter) (treesit-available-p))
  (package! julia-ts-mode :pin "d693c6b35d3aed986b2700a3b5f910de12d6c53c"))

(when (modulep! +lsp)
  (if (modulep! :tools lsp +eglot)
      (package! eglot-jl :pin "7c968cc61fb64016ebe6dc8ff83fd05923db4374")
    (package! lsp-julia :pin "c869b2f6c05a97e5495ed3cc6710a33b4faf41a2")))

(when (modulep! +snail)
  (package! julia-snail :pin "cacf52e4c8db76706e6aa336d38746d15a2b6fe2"))
