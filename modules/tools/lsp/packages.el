;; -*- no-byte-compile: t; -*-
;;; tools/lsp/packages.el

(package! lsp-mode :pin "2fc0963a50")
(package! lsp-ui :pin "e8200e3b72")
(when (featurep! :completion company)
  (package! company-lsp :pin "f921ffa0cd"))
(when (featurep! :completion ivy)
  (package! lsp-ivy :pin "78c1429c62"))
(when (featurep! :completion helm)
  (package! helm-lsp :pin "6f62659cc5"))
