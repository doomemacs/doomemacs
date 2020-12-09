;; -*- no-byte-compile: t; -*-
;;; tools/lsp/packages.el

(if (featurep! +eglot)
    (progn
      (package! eglot :pin "21726416e6e580b20dfa90833c6dab2a8a15ea48")
      (package! project :pin "0003fe4f3f81be08a9427888ead29f182e0b2527"))
  (package! lsp-mode :pin "61443f32531d1e94026c379319c99f21414ee3e7")
  (package! lsp-ui :pin "49bc5134c15d92b866c389c16e8e551a9285961a")
  (when (featurep! :completion ivy)
    (package! lsp-ivy :pin "c70ee8b54357c56d1b972393ee53e57a2e545fbb"))
  (when (featurep! :completion helm)
    (package! helm-lsp :pin "fc09aa0903ee6abe4955e9a6062dcea667ebff5a")))
