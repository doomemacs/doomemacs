;; -*- no-byte-compile: t; -*-
;;; tools/lsp/packages.el

(if (featurep! +eglot)
    (progn
      (package! eglot :pin "55c13a91378cdd7822c99bbbf340ea76b1f0bf38")
      (when (featurep! :completion vertico)
        (package! consult-eglot :pin "f93c571dc392a8b11d35541bffde30bd9f411d30")))
  (package! lsp-mode :pin "41173dca4d6a7fa381ba6dc154e7149cb113f7e1")
  (package! lsp-ui :pin "98d0ad00b8bf1d3a7cea490002169f2286d7208c")
  (when (featurep! :completion ivy)
    (package! lsp-ivy :pin "3e87441a625d65ced5a208a0b0442d573596ffa3"))
  (when (featurep! :completion helm)
    (package! helm-lsp :pin "c2c6974dadfac459b1a69a1217441283874cea92"))
  (when (featurep! :completion vertico)
    (package! consult-lsp :pin "aaa9a31bc82259d743186c53d8b01f043c6fec13")))
