;; -*- no-byte-compile: t; -*-
;;; tools/lsp/packages.el

(if (featurep! +eglot)
    (progn
      (package! eglot :pin "21726416e6e580b20dfa90833c6dab2a8a15ea48")
      (package! project :pin "67216fba813461a8b8d5db1a12aaca08a0389423"))
  (package! lsp-mode :pin "e1c42412eac0d742196efe64a82699326e72b391")
  (package! lsp-ui :pin "b1693d610c4d2c44305eba2719e8d4097fdcdcb8")
  (when (featurep! :completion ivy)
    (package! lsp-ivy :pin "c70ee8b54357c56d1b972393ee53e57a2e545fbb"))
  (when (featurep! :completion helm)
    (package! helm-lsp :pin "fc09aa0903ee6abe4955e9a6062dcea667ebff5a")))
