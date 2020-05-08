;; -*- no-byte-compile: t; -*-
;;; tools/lsp/packages.el

(package! lsp-mode :pin "11750e7b118858b38417a538c1c6eff8759c12f3")
(package! lsp-ui :pin "1288be94b4c37f89e80a03b1cff1b81aba9560bb")
(when (featurep! :completion ivy)
  (package! lsp-ivy :pin "81e81ced99829358674c5a6bbe2c3e15cecd4ed8"))
(when (featurep! :completion helm)
  (package! helm-lsp :pin "6b5ce182d7c94c62b55b8f7d0c7e643b2c30e560"))
