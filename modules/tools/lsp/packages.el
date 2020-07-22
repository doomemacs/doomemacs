;; -*- no-byte-compile: t; -*-
;;; tools/lsp/packages.el

(if (featurep! +eglot)
    (progn
      (package! eglot :pin "e3ce64adc09922a87936617484e48cbecc5db729")
      (package! project :pin "da0333a697b18f0a863c1b1523d2fc7991b31174"))
  (package! lsp-mode :pin "f04d0182f08775529509cccc6079b5a8bdd77f9c")
  (package! lsp-ui :pin "1f3e9700395f4fae024ca45ca64c8d70e99b39d2")
  (when (featurep! :completion ivy)
    (package! lsp-ivy :pin "4cdb739fc2bc47f7d4dcad824f9240c70c4cb37d"))
  (when (featurep! :completion helm)
    (package! helm-lsp :pin "5c960e7800dc8f4432f3a1466a637d484b87dc35")))
