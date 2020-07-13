;; -*- no-byte-compile: t; -*-
;;; tools/lsp/packages.el

(if (featurep! +eglot)
    (progn
      (package! eglot :pin "6d9660152b6d86931a6b1dd25f81a354b77a5ae8")
      (package! project :pin "da0333a697b18f0a863c1b1523d2fc7991b31174"))
  (package! lsp-mode :pin "5ef8c1f586663ecf8a93a14248ddaed900b5b59b")
  (package! lsp-ui :pin "7d5326430eb88a58e111cb22ffa42c7d131e5052")
  (when (featurep! :completion ivy)
    (package! lsp-ivy :pin "4cdb739fc2bc47f7d4dcad824f9240c70c4cb37d"))
  (when (featurep! :completion helm)
    (package! helm-lsp :pin "5c960e7800dc8f4432f3a1466a637d484b87dc35")))
