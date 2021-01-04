;; -*- no-byte-compile: t; -*-
;;; tools/lsp/packages.el

(if (featurep! +eglot)
    (progn
      (package! eglot :pin "4c85df2b04e467b8ed0eca68bd202fd0e7b671f4")
      (package! project :pin "a1997af9a6de5b10cebe8c06875508249ad559ea"))
  (package! lsp-mode :pin "dbfbe1a221de78e5d42e93ab2833d68c7f27f1b7")
  (package! lsp-ui :pin "94673cd08c35acc3b6c34301f74f1852487a5558")
  (when (featurep! :completion ivy)
    (package! lsp-ivy :pin "c70ee8b54357c56d1b972393ee53e57a2e545fbb"))
  (when (featurep! :completion helm)
    (package! helm-lsp :pin "fc09aa0903ee6abe4955e9a6062dcea667ebff5a")))
