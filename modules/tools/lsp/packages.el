;; -*- no-byte-compile: t; -*-
;;; tools/lsp/packages.el

(if (featurep! +eglot)
    (progn
      (package! eglot :pin "fd9a5646d1b49ef5968713005d83131dd75a52ad")
      (when (featurep! :completion vertico)
        (package! consult-eglot :pin "f93c571dc392a8b11d35541bffde30bd9f411d30")))
  (package! lsp-mode :pin "1e7128c43f611ed78752130ce5ce67e641a276df")
  (package! lsp-ui :pin "96b1ecbfbf87a775f05b5f0b55253376a3bd61e7")
  (when (featurep! :completion ivy)
    (package! lsp-ivy :pin "3e87441a625d65ced5a208a0b0442d573596ffa3"))
  (when (featurep! :completion helm)
    (package! helm-lsp :pin "c2c6974dadfac459b1a69a1217441283874cea92"))
  (when (featurep! :completion vertico)
    (package! consult-lsp :pin "5a3c4e3f4233feff3f141df38f93d1be80259301")))
