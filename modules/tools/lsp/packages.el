;; -*- no-byte-compile: t; -*-
;;; tools/lsp/packages.el

(if (modulep! +eglot)
    (progn
      (package! eglot :pin "678610fdc544f10ac757ab7acf88ac7c5815ed5a")
      (when (modulep! :completion vertico)
        (package! consult-eglot :pin "64262e72452f8fe6dd49d31bcdd4bd577b7d682d"))
      (when (and (modulep! :checkers syntax)
             (not (modulep! :checkers syntax +flymake)))
        (package! flycheck-eglot :pin "114e1315aaf0dc3196da67da426bbe2b46384fe2")))
  (package! lsp-mode :pin "57d08cd37232174654bdb8d58c5edb3c117b3718")
  (package! lsp-ui :pin "8aa8b175fc4cdf2d16f6f3fdb2904e8874610c8a")
  (when (modulep! :completion ivy)
    (package! lsp-ivy :pin "9ecf4dd9b1207109802bd1882aa621eb1c385106"))
  (when (modulep! :completion helm)
    (package! helm-lsp :pin "c2c6974dadfac459b1a69a1217441283874cea92"))
  (when (modulep! :completion vertico)
    (package! consult-lsp :pin "f8db3252c0daa41225ba4ed1c0d178b281cd3e90")))
