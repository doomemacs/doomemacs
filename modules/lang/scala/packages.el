;; -*- no-byte-compile: t; -*-
;;; lang/scala/packages.el

(package! sbt-mode :pin "0bdc36ba3b3955c1106a5cda69be98bd38195cb6")
(package! scala-mode :pin "1ab5f645606e40db07b813a1600835d1442c060a")

(when (featurep! +lsp)
  (package! lsp-metals :pin "51a89c1861eb505882c20393227f303ac33276e4"))
