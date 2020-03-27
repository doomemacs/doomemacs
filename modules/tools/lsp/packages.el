;; -*- no-byte-compile: t; -*-
;;; tools/lsp/packages.el

(package! lsp-mode :pin "76fe399b40")
(package! lsp-ui :pin "134d9b725d")
(when (featurep! :completion company)
  (package! company-lsp :pin "f921ffa0cd"))
(when (featurep! :completion ivy)
  (package! lsp-ivy :pin "39b90e7aef"))
(when (featurep! :completion helm)
  (package! helm-lsp :pin "6f62659cc5"))
