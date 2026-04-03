;; -*- no-byte-compile: t; -*-
;;; lang/julia/packages.el

(package! julia-mode :pin "1478898ea0ab1ae21ec053fcc81c6b42136a2224")
(package! julia-repl :pin "0173237a43d9a42f0d69a5405283fabe1ac602a0")

(when (and (modulep! +tree-sitter) (treesit-available-p))
  (package! julia-ts-mode :pin "d693c6b35d3aed986b2700a3b5f910de12d6c53c"))

(when (modulep! +lsp)
  (if (modulep! :tools lsp +eglot)
      (package! eglot-jl :pin "7c968cc61fb64016ebe6dc8ff83fd05923db4374")
    (package! lsp-julia :pin "c869b2f6c05a97e5495ed3cc6710a33b4faf41a2")))

(when (modulep! +snail)
  (package! julia-snail :pin "95905e9d6bbf5657b16a307b120d9efb10ac692b"))
