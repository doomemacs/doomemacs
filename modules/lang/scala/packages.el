;; -*- no-byte-compile: t; -*-
;;; lang/scala/packages.el

(package! sbt-mode :pin "cc68728a6ef0600aad369157b3a2d0ce56afba9b")
(package! scala-mode :pin "4c6d636b86e3bb1d95de819dc48dda92abdfbcf4")

(when (and (modulep! +lsp)
           (not (modulep! :tools lsp +eglot)))
  (package! lsp-metals :pin "e55d544996f7321622e1eeafdc3dd128f8e72ce5"))
