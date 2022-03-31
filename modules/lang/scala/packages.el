;; -*- no-byte-compile: t; -*-
;;; lang/scala/packages.el

(package! sbt-mode :pin "9fe1e8807c22cc1dc56a6233e000969518907f4d")
(package! scala-mode :pin "598cb680f321d9609295aa9b4679040cc703b602")

(when (featurep! +lsp)
  (package! lsp-metals :pin "b7f77de69431786c54e9a57845e4f2d75fbee053"))
