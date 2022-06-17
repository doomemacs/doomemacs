;; -*- no-byte-compile: t; -*-
;;; tools/lsp/packages.el

(if (featurep! +eglot)
    (progn
      (package! eglot :pin "e835996e16610d0ded6d862214b3b452b8803ea8")
      (when (featurep! :completion vertico)
        (package! consult-eglot :pin "0da8801dd8435160ce1f62ad8066bd52e38f5cbd")))
  (package! lsp-mode :pin "6b6afc00deec6dacb78834c02ed5f262e72ce337")
  (package! lsp-ui :pin "a0b97db2ada163453c9072d3640202a0b27c29f5")
  (when (featurep! :completion ivy)
    (package! lsp-ivy :pin "3e87441a625d65ced5a208a0b0442d573596ffa3"))
  (when (featurep! :completion helm)
    (package! helm-lsp :pin "c2c6974dadfac459b1a69a1217441283874cea92"))
  (when (featurep! :completion vertico)
    (package! consult-lsp :pin "19606a03cf854e1b0930c4526ed92c4560dccdc2")))
