;; -*- no-byte-compile: t; -*-
;;; tools/lsp/packages.el

(if (featurep! +eglot)
    (progn
      (package! eglot :pin "2fbcab293e11e1502a0128ca5f59de0ea7888a75")
      (package! project :pin "388ffdfc5cdd075fa868d472c57369fd955d1e6a"))
  (package! lsp-mode :pin "eda51c21662253fd05b4f3f20ad7b029d9c2aff7")
  (package! lsp-ui :pin "1dbea9ff206a626d5c2d735e3f8fcdac59827963")
  (when (featurep! :completion ivy)
    (package! lsp-ivy :pin "515e5977b3d1f6cb521984f084868f28efd47e72"))
  (when (featurep! :completion helm)
    (package! helm-lsp :pin "74a02f89088484c42ffc184ece338b73abd4d6f6")))
