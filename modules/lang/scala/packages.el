;; -*- no-byte-compile: t; -*-
;;; lang/scala/packages.el

(package! sbt-mode :pin "cc68728a6ef0600aad369157b3a2d0ce56afba9b")
(package! scala-mode :pin "bd0638c32ab0f2eadacf2809329abf5388211760")

(when (and (modulep! +lsp)
           (modulep! :tools lsp -eglot))
  (package! lsp-metals :pin "0dc938be1190d147e7013e3dce08ac8bff5d1662"))
