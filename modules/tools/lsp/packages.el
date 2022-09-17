;; -*- no-byte-compile: t; -*-
;;; tools/lsp/packages.el

(if (modulep! +eglot)
    (progn
      (package! eglot :pin "aeea7c719a05f729af2df2f0dc6cbd4337df140f")
      (when (modulep! :completion vertico)
        (package! consult-eglot :pin "0da8801dd8435160ce1f62ad8066bd52e38f5cbd")))
  (package! lsp-mode :pin "68bdac0f806789c064856a47d0e413b65a260e1e")
  (package! lsp-ui :pin "d8cce7dc154aa3216c080dd5c6fb827bdba9a312")
  (when (modulep! :completion ivy)
    (package! lsp-ivy :pin "9ecf4dd9b1207109802bd1882aa621eb1c385106"))
  (when (modulep! :completion helm)
    (package! helm-lsp :pin "c2c6974dadfac459b1a69a1217441283874cea92"))
  (when (modulep! :completion vertico)
    (package! consult-lsp :pin "19606a03cf854e1b0930c4526ed92c4560dccdc2")))
