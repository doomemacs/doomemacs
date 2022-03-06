;; -*- no-byte-compile: t; -*-
;;; tools/lsp/packages.el

(if (featurep! +eglot)
    (progn
      (package! eglot :pin "9389d2e4a0976068f129c9b4d20253bc0c562199")
      (when (featurep! :completion vertico)
        (package! consult-eglot :pin "f93c571dc392a8b11d35541bffde30bd9f411d30")))
  (package! lsp-mode :pin "3d2b8523d57be74b0f7d4966b9ee1f9d4b118852")
  (package! lsp-ui :pin "96b1ecbfbf87a775f05b5f0b55253376a3bd61e7")
  (when (featurep! :completion ivy)
    (package! lsp-ivy :pin "3e87441a625d65ced5a208a0b0442d573596ffa3"))
  (when (featurep! :completion helm)
    (package! helm-lsp :pin "c2c6974dadfac459b1a69a1217441283874cea92"))
  (when (featurep! :completion vertico)
    (package! consult-lsp :pin "f4f195046b97be5ce0406e0723921b3393d9442e")))
