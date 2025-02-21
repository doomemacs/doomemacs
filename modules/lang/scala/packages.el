;; -*- no-byte-compile: t; -*-
;;; lang/scala/packages.el

(package! sbt-mode :pin "cc68728a6ef0600aad369157b3a2d0ce56afba9b")
(package! scala-mode :pin "661337d8aa0a0cb418184c83757661603de3b2e3")

(when (and (modulep! +lsp)
           (modulep! :tools lsp -eglot))
  (package! lsp-metals :pin "567089f7f42add43edbe840960d08d19f157f8e9"))
