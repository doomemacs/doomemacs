;; -*- no-byte-compile: t; -*-
;;; tools/lsp/packages.el

(package! lsp-mode :pin "bac42c50b370f3716f258506dc1ae9f62906313f")
(package! lsp-ui :pin "ab55e306af9dd9eb62fe7463e4e05d948ad3dfc6")
(when (featurep! :completion ivy)
  (package! lsp-ivy :pin "81e81ced99829358674c5a6bbe2c3e15cecd4ed8"))
(when (featurep! :completion helm)
  (package! helm-lsp :pin "6b5ce182d7c94c62b55b8f7d0c7e643b2c30e560"))
