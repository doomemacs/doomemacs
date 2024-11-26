;; -*- no-byte-compile: t; -*-
;;; lang/julia/packages.el

(package! julia-mode :pin "2aca8cf585405867e80240da7432a5db6bc7864d")
(package! julia-repl :pin "bb90cc1fceccc8dfd0e4b60d624271e4aca6b9b8")

(when (modulep! +lsp)
  (if (modulep! :tools lsp +eglot)
      (package! eglot-jl :pin "7c968cc61fb64016ebe6dc8ff83fd05923db4374")
    (package! lsp-julia :pin "c869b2f6c05a97e5495ed3cc6710a33b4faf41a2")))

(when (modulep! +snail)
  (package! julia-snail :pin "dff92c4250e40a6cc106f0ea993f9631ad55eb7c"))
