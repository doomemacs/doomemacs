;; -*- no-byte-compile: t; -*-
;;; lang/julia/packages.el

(package! julia-mode :pin "7aafa8e77df64a47fa4729a0c1ea572b5bc8e30e")
(package! julia-repl :pin "57a15dfafed680ad7d81f779d414e8cb6717417c")

(when (modulep! +lsp)
  (if (modulep! :tools lsp +eglot)
      (package! eglot-jl :pin "2e04597223553a369dd5b6520b6365b41e6ea508")
    (package! lsp-julia :pin "d6688bb131ff4a5a0201f6d3826ef0b018265389")))
