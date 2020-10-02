;; -*- no-byte-compile: t; -*-
;;; lang/julia/packages.el

(package! julia-mode :pin "b5f5983d2b232c8bba4c5eff75cccdb787c19d98")
(package! julia-repl :pin "d073acb6339e99edf77833f82277afd9a076f16a")

(when (featurep! +lsp)
  (package! lsp-julia :recipe (:nonrecursive t) :pin "11c701ac347da69b173b01d073efe0fb09f3298f"))
