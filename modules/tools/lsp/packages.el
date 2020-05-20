;; -*- no-byte-compile: t; -*-
;;; tools/lsp/packages.el

(package! lsp-mode :pin "93057777c66e8f0e9140b8d0d3215beaa2c035a9")
(package! lsp-ui :pin "271b47cb33f11915295911f7cf8575f8a82a5e1c")
(when (featurep! :completion ivy)
  (package! lsp-ivy :pin "dce58b5509271bbedb53ba9d0278dcb563a43977"))
(when (featurep! :completion helm)
  (package! helm-lsp :pin "6b5ce182d7c94c62b55b8f7d0c7e643b2c30e560"))
