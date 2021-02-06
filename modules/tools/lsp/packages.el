;; -*- no-byte-compile: t; -*-
;;; tools/lsp/packages.el

(if (featurep! +eglot)
    (progn
      (package! eglot :pin "398b81eeec44b35b39480a38f1b1357bc8550a1c")
      (package! project :pin "f743ca2e5c3343c71b85040aac6a94f1b123f832"))
  (package! lsp-mode :pin "62cd1b2e569c72638ba4bd42a0290192c224c28d")
  (package! lsp-ui :pin "732992aa41bb78b7341e28c980817de488b7a317")
  (when (featurep! :completion ivy)
    (package! lsp-ivy :pin "c70ee8b54357c56d1b972393ee53e57a2e545fbb"))
  (when (featurep! :completion helm)
    (package! helm-lsp :pin "fc09aa0903ee6abe4955e9a6062dcea667ebff5a")))
