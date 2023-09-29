;; -*- no-byte-compile: t; -*-
;;; lang/scala/packages.el

(package! sbt-mode :pin "9fe1e8807c22cc1dc56a6233e000969518907f4d")
(package! scala-mode :pin "5d7cf21c37e345c49f921fe5111a49fd54efd1e0")

(when (and (modulep! +lsp)
           (not (modulep! :tools lsp +eglot)))
  (package! lsp-metals :pin "4102602126210f03fff783040eeb6e04266b7af3"))
