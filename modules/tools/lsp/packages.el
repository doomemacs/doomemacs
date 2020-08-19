;; -*- no-byte-compile: t; -*-
;;; tools/lsp/packages.el

(if (featurep! +eglot)
    (progn
      (package! eglot :pin "5f873d288e1c5434c1640bef03555ed056cb0d35")
      (package! project :pin "da0333a697b18f0a863c1b1523d2fc7991b31174"))
  (package! lsp-mode :pin "4145a70ce1d4bfb2463606ba34c5965080b080d9")
  (package! lsp-ui :pin "c39ae3713f95a2d86e11fd1f77e89a671d08d18a")
  (when (featurep! :completion ivy)
    (package! lsp-ivy :pin "4cdb739fc2bc47f7d4dcad824f9240c70c4cb37d"))
  (when (featurep! :completion helm)
    (package! helm-lsp :pin "4263c967267b0579956b3b12ef32878a9ea80d97")))
