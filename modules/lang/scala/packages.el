;; -*- no-byte-compile: t; -*-
;;; lang/scala/packages.el

(package! sbt-mode :pin "bcf8d6040021013430b39d6f6766ce1aab0b691a")
(package! scala-mode :pin "4c6d636b86e3bb1d95de819dc48dda92abdfbcf4")

(when (and (modulep! +lsp)
           (not (modulep! :tools lsp +eglot)))
  (package! lsp-metals :pin "e55d544996f7321622e1eeafdc3dd128f8e72ce5"))
