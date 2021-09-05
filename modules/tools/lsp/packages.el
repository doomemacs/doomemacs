;; -*- no-byte-compile: t; -*-
;;; tools/lsp/packages.el

(if (featurep! +eglot)
    (package! eglot :pin "9b63f21e75dc8222b5dfc4006ce8559bb73532cb")
  (package! lsp-mode :pin "d9c8ed3a932b568614d9bbf682271cf43bb8ec73")
  (package! lsp-ui :pin "b625f3cb5e88559ab99bec58f7a14272edb296bc")
  (when (featurep! :completion ivy)
    (package! lsp-ivy :pin "3e87441a625d65ced5a208a0b0442d573596ffa3"))
  (when (featurep! :completion helm)
    (package! helm-lsp :pin "c2c6974dadfac459b1a69a1217441283874cea92"))
  (when (featurep! :completion vertico)
    (package! consult-lsp :pin "e8a50f2c94f40c86934ca2eaff007f9c00586272")))
