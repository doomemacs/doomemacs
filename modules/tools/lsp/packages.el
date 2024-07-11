;; -*- no-byte-compile: t; -*-
;;; tools/lsp/packages.el

(if (modulep! +eglot)
    (progn
      (package! eglot :pin "8008255edd74d8bda75e01ed747c2460e28404ac")
      (when (modulep! :completion vertico)
        (package! consult-eglot :pin "64262e72452f8fe6dd49d31bcdd4bd577b7d682d"))
      (when (and (modulep! :checkers syntax)
             (not (modulep! :checkers syntax +flymake)))
        (package! flycheck-eglot :pin "09e37f4c726d9b565b040ba9e89215158d3bd6b6")))
  (package! lsp-mode :pin "52987755175042b9569e32d72ae29eba119020d6")
  (package! lsp-ui :pin "00f1fecdfb41c30428734cf27e492f26f46627fb")
  (when (modulep! :completion ivy)
    (package! lsp-ivy :pin "9ecf4dd9b1207109802bd1882aa621eb1c385106"))
  (when (modulep! :completion helm)
    (package! helm-lsp :pin "c2c6974dadfac459b1a69a1217441283874cea92"))
  (when (modulep! :completion vertico)
    (package! consult-lsp :pin "68583913168bf66fd4d542b2517a2dcab19c447c")))
