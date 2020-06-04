;; -*- no-byte-compile: t; -*-
;;; tools/lsp/packages.el

(if (featurep! +eglot)
    (progn
      (package! eglot :pin "ac9239bed5e3bfbf057382d1a75cdfa23f2caddd")
      (package! project :recipe (:host github :repo "emacs-straight/project")
        :pin "da0333a697b18f0a863c1b1523d2fc7991b31174"))
  (package! lsp-mode :pin "d5f0410a88edcbcc183f5877a63b4896fd4f9941")
  (package! lsp-ui :pin "c3e7a3759b52fe0f9c6f0f6668f1d6d88e4f784a")
  (when (featurep! :completion ivy)
    (package! lsp-ivy :pin "dce58b5509271bbedb53ba9d0278dcb563a43977"))
  (when (featurep! :completion helm)
    (package! helm-lsp :pin "6b5ce182d7c94c62b55b8f7d0c7e643b2c30e560")))
