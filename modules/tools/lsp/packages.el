;; -*- no-byte-compile: t; -*-
;;; tools/lsp/packages.el

(if (featurep! +eglot)
    (progn
      (package! eglot :pin "398b81eeec44b35b39480a38f1b1357bc8550a1c")
      (package! project :pin "2e7afbe7d0c67c13f86c908ae13f2694308d6ab8"))
  (package! lsp-mode :pin "3ca25e61b419c4419a345d43594338b8f2ff295e")
  (package! lsp-ui :pin "732992aa41bb78b7341e28c980817de488b7a317")
  (when (featurep! :completion ivy)
    (package! lsp-ivy :pin "c70ee8b54357c56d1b972393ee53e57a2e545fbb"))
  (when (featurep! :completion helm)
    (package! helm-lsp :pin "fc09aa0903ee6abe4955e9a6062dcea667ebff5a")))
