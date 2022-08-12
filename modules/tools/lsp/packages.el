;; -*- no-byte-compile: t; -*-
;;; tools/lsp/packages.el

(if (modulep! +eglot)
    (progn
      (package! eglot :pin "bd970be047aba4513c2970b9ff2d842f0472cb0e")
      (when (modulep! :completion vertico)
        (package! consult-eglot :pin "0da8801dd8435160ce1f62ad8066bd52e38f5cbd")))
  (package! lsp-mode :pin "3fa645c0397b8f438f2db2dd288b899ba330d410")
  (package! lsp-ui :pin "0a6368d38dc32e52abcbd52b63d1e557e42c66e6")
  (when (modulep! :completion ivy)
    (package! lsp-ivy :pin "3e87441a625d65ced5a208a0b0442d573596ffa3"))
  (when (modulep! :completion helm)
    (package! helm-lsp :pin "c2c6974dadfac459b1a69a1217441283874cea92"))
  (when (modulep! :completion vertico)
    (package! consult-lsp :pin "19606a03cf854e1b0930c4526ed92c4560dccdc2")))
