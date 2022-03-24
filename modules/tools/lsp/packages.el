;; -*- no-byte-compile: t; -*-
;;; tools/lsp/packages.el

(if (featurep! +eglot)
    (progn
      (package! eglot :pin "f77518711507810b779d87190d0ca0183fc02e10")
      (when (featurep! :completion vertico)
        (package! consult-eglot :pin "f93c571dc392a8b11d35541bffde30bd9f411d30")))
  (package! lsp-mode :pin "4acf72202d47dd7f0166c69220e1f734d133db89")
  (package! lsp-ui :pin "96b1ecbfbf87a775f05b5f0b55253376a3bd61e7")
  (when (featurep! :completion ivy)
    (package! lsp-ivy :pin "3e87441a625d65ced5a208a0b0442d573596ffa3"))
  (when (featurep! :completion helm)
    (package! helm-lsp :pin "c2c6974dadfac459b1a69a1217441283874cea92"))
  (when (featurep! :completion vertico)
    (package! consult-lsp :pin "f4f195046b97be5ce0406e0723921b3393d9442e")))
