;; -*- no-byte-compile: t; -*-
;;; tools/lsp/packages.el

(if (featurep! +eglot)
    (package! eglot :pin "122dbb9d6ef477c5c5f0351895652b825d6447de")
  (package! lsp-mode :pin "591528daf0eed278e13500e058fde96bfa347b7d")
  (package! lsp-ui :pin "5d643fbb0c4ef5fc4ee93d9894bf68388095160a")
  (when (featurep! :completion ivy)
    (package! lsp-ivy :pin "bccd86028e669f5a1cad78364775fe7a0741ff93"))
  (when (featurep! :completion helm)
    (package! helm-lsp :pin "c2c6974dadfac459b1a69a1217441283874cea92"))
  (when (featurep! :completion vertico)
    (package! consult-lsp :pin "e8a50f2c94f40c86934ca2eaff007f9c00586272")))
