;; -*- no-byte-compile: t; -*-
;;; tools/lsp/packages.el

(if (featurep! +eglot)
    (package! eglot :pin "b06589b393844c31c17962497e621a72e0e78e01")
  (package! lsp-mode :pin "3dc87f6111ea2439a8d974a8954dcbb683144b92")
  (package! lsp-ui :pin "efae00eb6a733d5271cb33e9d92c3d8c2fa98dde")
  (when (featurep! :completion ivy)
    (package! lsp-ivy :pin "4dcb63533558a83de4a1b830835687e69474cd88"))
  (when (featurep! :completion helm)
    (package! helm-lsp :pin "c2c6974dadfac459b1a69a1217441283874cea92")))
