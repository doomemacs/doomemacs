;; -*- no-byte-compile: t; -*-
;;; lang/scala/packages.el

(package! sbt-mode :pin "9fe1e8807c22cc1dc56a6233e000969518907f4d")
(package! scala-mode :pin "598cb680f321d9609295aa9b4679040cc703b602")

(when (modulep! +lsp)
  (package! lsp-metals :pin "097d6021a4ff0eae704cc3074e064c9509c5cafc"))
