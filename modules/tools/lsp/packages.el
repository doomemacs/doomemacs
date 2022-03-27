;; -*- no-byte-compile: t; -*-
;;; tools/lsp/packages.el

(if (featurep! +eglot)
    (progn
      (package! eglot :pin "f77518711507810b779d87190d0ca0183fc02e10")
      (when (featurep! :completion vertico)
        (package! consult-eglot :pin "f93c571dc392a8b11d35541bffde30bd9f411d30")))
  (package! lsp-mode :pin "c6482c1bbfa366a1fc52c32c03164ac77f297022")
  (package! lsp-ui :pin "96b1ecbfbf87a775f05b5f0b55253376a3bd61e7")
  (when (featurep! :completion ivy)
    (package! lsp-ivy :pin "3e87441a625d65ced5a208a0b0442d573596ffa3"))
  (when (featurep! :completion helm)
    (package! helm-lsp :pin "c2c6974dadfac459b1a69a1217441283874cea92"))
  (when (featurep! :completion vertico)
    (package! consult-lsp :pin "0dfc9d55876d4cf7c32f8a663fe6343927f78052")))
