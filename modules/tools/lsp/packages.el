;; -*- no-byte-compile: t; -*-
;;; tools/lsp/packages.el

(if (featurep! +eglot)
    (progn
      (package! eglot :pin "ac9239bed5e3bfbf057382d1a75cdfa23f2caddd")
      (package! project :pin "da0333a697b18f0a863c1b1523d2fc7991b31174"))
  (package! lsp-mode :pin "029ec54f79aeeaee3b2683ea9112887d811d9cfa")
  (package! lsp-ui :pin "1ee113cca78bcb680b9783d2d6253b2c54beeece")
  (when (featurep! :completion ivy)
    (package! lsp-ivy :pin "1f6d8777a3445d23678216fe1f46475de5f4fb0e"))
  (when (featurep! :completion helm)
    (package! helm-lsp :pin "5018af9c709a783de1b9e101e07c948cceed67f1")))
