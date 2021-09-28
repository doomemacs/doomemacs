;; -*- no-byte-compile: t; -*-
;;; tools/lsp/packages.el

(if (featurep! +eglot)
    (progn
      (package! eglot :pin "c17bdf6c98d6bf0f1a85f1175556e1038654402f")
      (when (featurep! :completion vertico)
        (package! consult-eglot :pin "28a09cc839a9010df1a00a55528697ab34e1b259")))
  (package! lsp-mode :pin "d5d7a54dee2622d3fd884638617f4957e1876018")
  (package! lsp-ui :pin "b625f3cb5e88559ab99bec58f7a14272edb296bc")
  (when (featurep! :completion ivy)
    (package! lsp-ivy :pin "3e87441a625d65ced5a208a0b0442d573596ffa3"))
  (when (featurep! :completion helm)
    (package! helm-lsp :pin "c2c6974dadfac459b1a69a1217441283874cea92"))
  (when (featurep! :completion vertico)
    (package! consult-lsp :pin "e8a50f2c94f40c86934ca2eaff007f9c00586272")))
