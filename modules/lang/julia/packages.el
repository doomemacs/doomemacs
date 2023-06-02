;; -*- no-byte-compile: t; -*-
;;; lang/julia/packages.el

(package! julia-mode :pin "b35e668d6bcf0fb7de4a9ee1a72ae13aa4fb6562")
(package! julia-repl :pin "9503ef7110732e444e686e815c5b2ae8228d274d")

(when (modulep! +lsp)
  (if (modulep! :tools lsp +eglot)
      (package! eglot-jl :pin "7dc604fe42a459a987853d065cd6d0f3c4cbc02a")
    (package! lsp-julia :pin "c584f79c7fee6176bbb6120f4cb0f1001bcf8113")))

(when (featurep! +snail)
  (package! julia-snail :pin "c33663abf081a06e0fdbf5818255f082070d2160"))
