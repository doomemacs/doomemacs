;; -*- no-byte-compile: t; -*-
;;; tools/lsp/packages.el

(if (modulep! +eglot)
    (progn
      (package! eglot :pin "850cbd043084d7cc571e1090a4812db6e3f7d0bc")
      (when (modulep! :completion vertico)
        (package! consult-eglot :pin "64262e72452f8fe6dd49d31bcdd4bd577b7d682d"))
      (when (and (modulep! :checkers syntax)
             (not (modulep! :checkers syntax +flymake)))
        (package! flycheck-eglot :pin "114e1315aaf0dc3196da67da426bbe2b46384fe2")))
  (package! lsp-mode :pin "cec9e56390e90d7ced3b18a922ab954e782b8628")
  (package! lsp-ui :pin "00f1fecdfb41c30428734cf27e492f26f46627fb")
  (when (modulep! :completion ivy)
    (package! lsp-ivy :pin "9ecf4dd9b1207109802bd1882aa621eb1c385106"))
  (when (modulep! :completion helm)
    (package! helm-lsp :pin "c2c6974dadfac459b1a69a1217441283874cea92"))
  (when (modulep! :completion vertico)
    (package! consult-lsp :pin "68583913168bf66fd4d542b2517a2dcab19c447c")))
