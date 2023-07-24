;; -*- no-byte-compile: t; -*-
;;; lang/julia/packages.el

(package! julia-mode :pin "7a8c868e0d3e51ba4a2c621ee22ca9599e0e4bbb")
(package! julia-repl :pin "9503ef7110732e444e686e815c5b2ae8228d274d")

(when (modulep! +lsp)
  (if (modulep! :tools lsp +eglot)
      (package! eglot-jl :pin "7dc604fe42a459a987853d065cd6d0f3c4cbc02a")
    (package! lsp-julia :pin "c584f79c7fee6176bbb6120f4cb0f1001bcf8113")))

(when (modulep! +snail)
  (package! julia-snail :pin "18b891b4569096d541e996cf7e24da01efdf2e03"))
