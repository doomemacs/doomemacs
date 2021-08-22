;; -*- no-byte-compile: t; -*-
;;; tools/lsp/packages.el

(if (featurep! +eglot)
    (progn
      (package! eglot :pin "c17bdf6c98d6bf0f1a85f1175556e1038654402f")
      (when (featurep! :completion vertico)
        (package! consult-eglot :pin "a6aeb6fa078cc7ea6537793868f606b55ac63088")))
  (package! lsp-mode :pin "82fa7743602e9a6366ecd128efcd620ecc97fcf4")
  (package! lsp-ui :pin "b625f3cb5e88559ab99bec58f7a14272edb296bc")
  (when (featurep! :completion ivy)
    (package! lsp-ivy :pin "3e87441a625d65ced5a208a0b0442d573596ffa3"))
  (when (featurep! :completion helm)
    (package! helm-lsp :pin "c2c6974dadfac459b1a69a1217441283874cea92"))
  (when (featurep! :completion vertico)
    (package! consult-lsp :pin "e8a50f2c94f40c86934ca2eaff007f9c00586272")))
