;; -*- no-byte-compile: t; -*-
;;; lang/julia/packages.el

(package! julia-mode :pin "47f43f7d839019cac3ba6559d93b29487ca118cb")
(package! julia-repl :pin "6625e95c3d0561c6966a13c2565d35d2ae6c07ca")

(when (modulep! +lsp)
  (if (modulep! :tools lsp +eglot)
      (package! eglot-jl :pin "2e35cf9768d97a0429a72deddbe30d6d7722d454")
    (package! lsp-julia :pin "d6688bb131ff4a5a0201f6d3826ef0b018265389")))
