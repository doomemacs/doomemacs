;; -*- no-byte-compile: t; -*-
;;; lang/scala/packages.el

(package! sbt-mode :pin "9fe1e8807c22cc1dc56a6233e000969518907f4d")
(package! scala-mode :pin "5d7cf21c37e345c49f921fe5111a49fd54efd1e0")

(when (and (modulep! +lsp)
           (not (modulep! :tools lsp +eglot)))
  (package! lsp-metals :pin "da7e54ed65f4e153c94b9c54689908dce142ef37"))
