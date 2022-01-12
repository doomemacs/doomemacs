;; -*- no-byte-compile: t; -*-
;;; tools/lsp/packages.el

(if (featurep! +eglot)
    (progn
      (package! eglot :pin "2c0f262c240222e42e3ec914307b9690354bb3a3")
      (when (featurep! :completion vertico)
        (package! consult-eglot :pin "f93c571dc392a8b11d35541bffde30bd9f411d30")))
  (package! lsp-mode :pin "c228bce435b9fb4b1f4d1bb89b99fd5315df15a9")
  (package! lsp-ui :pin "21ce926eedd41ef305c2d89412506ce59b1a7eac")
  (when (featurep! :completion ivy)
    (package! lsp-ivy :pin "3e87441a625d65ced5a208a0b0442d573596ffa3"))
  (when (featurep! :completion helm)
    (package! helm-lsp :pin "c2c6974dadfac459b1a69a1217441283874cea92"))
  (when (featurep! :completion vertico)
    (package! consult-lsp :pin "f4f195046b97be5ce0406e0723921b3393d9442e")))
