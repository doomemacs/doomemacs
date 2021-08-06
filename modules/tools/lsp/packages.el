;; -*- no-byte-compile: t; -*-
;;; tools/lsp/packages.el

(if (featurep! +eglot)
    (package! eglot :pin "194b178ef41ccd3d937983f3829d44a546bb24d6")
  (package! lsp-mode :pin "32d34445f3c4da37840c0ebb3a12e2c0627782a8")
  (package! lsp-ui :pin "177c31e982345ba35dc7c5d90cb1f8e68585323a")
  (when (featurep! :completion ivy)
    (package! lsp-ivy :pin "bccd86028e669f5a1cad78364775fe7a0741ff93"))
  (when (featurep! :completion helm)
    (package! helm-lsp :pin "c2c6974dadfac459b1a69a1217441283874cea92"))
  (when (featurep! :completion vertico)
    (package! consult-lsp :pin "e8a50f2c94f40c86934ca2eaff007f9c00586272")))
