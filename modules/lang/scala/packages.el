;; -*- no-byte-compile: t; -*-
;;; lang/scala/packages.el

(package! sbt-mode :pin "9fe1e8807c22cc1dc56a6233e000969518907f4d")
(package! scala-mode :pin "4c6d636b86e3bb1d95de819dc48dda92abdfbcf4")

(when (and (modulep! +lsp)
           (not (modulep! :tools lsp +eglot)))
  (package! lsp-metals :pin "da7e54ed65f4e153c94b9c54689908dce142ef37"))
