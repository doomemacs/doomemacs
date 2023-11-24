;; -*- no-byte-compile: t; -*-
;;; tools/lsp/packages.el

(if (modulep! +eglot)
    (progn
      (package! eglot :pin "2b145778ba5e57f393e50aea76b28e518c401828")
      (when (modulep! :completion vertico)
        (package! consult-eglot :pin "db9d41c9812a5a8a7b9a22fa7f3c314e37584d41"))
      (when (and (modulep! :checkers syntax)
             (not (modulep! :checkers syntax +flymake)))
        (package! flycheck-eglot :pin "9ff8d0068be59b1450964b390349d75a68af21ed")))
  (package! lsp-mode :pin "d441f3d268a31a4a514af2db506a5eec47ee617d")
  (package! lsp-ui :pin "0dd39900c8ed8145d207985cb2f65cedd1ffb410")
  (when (modulep! :completion ivy)
    (package! lsp-ivy :pin "9ecf4dd9b1207109802bd1882aa621eb1c385106"))
  (when (modulep! :completion helm)
    (package! helm-lsp :pin "c2c6974dadfac459b1a69a1217441283874cea92"))
  (when (modulep! :completion vertico)
    (package! consult-lsp :pin "f8db3252c0daa41225ba4ed1c0d178b281cd3e90")))
