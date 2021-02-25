;; -*- no-byte-compile: t; -*-
;;; tools/lsp/packages.el

(if (featurep! +eglot)
    (progn
      (package! eglot :pin "a2d1fc9c3c8a04072699713e8697cdeb92b2a4c7")
      (package! project :pin "2e7afbe7d0c67c13f86c908ae13f2694308d6ab8"))
  (package! lsp-mode :pin "c3cbadc095787f7cbf2922931a09afd5ac40ce5f")
  (package! lsp-ui :pin "0ac3e12138a7eeaf764845d1e7e61b02769003ec")
  (when (featurep! :completion ivy)
    (package! lsp-ivy :pin "c70ee8b54357c56d1b972393ee53e57a2e545fbb"))
  (when (featurep! :completion helm)
    (package! helm-lsp :pin "e934feaab89389e5c3503134bdba8c8637e99e25")))
