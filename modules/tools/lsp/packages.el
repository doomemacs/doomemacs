;; -*- no-byte-compile: t; -*-
;;; tools/lsp/packages.el

(package! lsp-mode :pin "fc812bea1f")
(package! lsp-ui :pin "da9788b427")
(when (featurep! :completion company)
  (package! company-lsp :pin "f921ffa0cd"))
(when (featurep! :completion ivy)
  (package! lsp-ivy :pin "78c1429c62"))
(when (featurep! :completion helm)
  (package! helm-lsp :pin "6f62659cc5"))
