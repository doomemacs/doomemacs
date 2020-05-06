;; -*- no-byte-compile: t; -*-
;;; tools/lsp/packages.el

(package! lsp-mode :pin "941e6062a5b407675e13ba471e9878f4a2dbd10e")
(package! lsp-ui :pin "43f71e3837b07f377444ad631b12f8198c495aa7")
(when (featurep! :completion ivy)
  (package! lsp-ivy :pin "81e81ced99829358674c5a6bbe2c3e15cecd4ed8"))
(when (featurep! :completion helm)
  (package! helm-lsp :pin "6b5ce182d7c94c62b55b8f7d0c7e643b2c30e560"))
