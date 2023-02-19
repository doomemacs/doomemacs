;; -*- no-byte-compile: t; -*-
;;; lang/scala/packages.el

(package! sbt-mode :pin "9fe1e8807c22cc1dc56a6233e000969518907f4d")
(package! scala-mode :pin "5d7cf21c37e345c49f921fe5111a49fd54efd1e0")

(when (modulep! +lsp)
  (package! lsp-metals :pin "a2df7263ece6ac69214e41c52d66aab8d3f650eb"))
