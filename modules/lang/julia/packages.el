;; -*- no-byte-compile: t; -*-
;;; lang/julia/packages.el

(package! julia-mode :pin "47f43f7d839019cac3ba6559d93b29487ca118cb")
(package! julia-repl :pin "e90b1ed2cc806262b0ee772dcc88f8da693d9210")

(when (featurep! +lsp)
  (if (featurep! :tools lsp +eglot)
      (package! eglot-jl :pin "2e35cf9768d97a0429a72deddbe30d6d7722d454")
    (package! lsp-julia :pin "d6688bb131ff4a5a0201f6d3826ef0b018265389")))
