;; -*- no-byte-compile: t; -*-
;;; tools/lsp/packages.el

(package! lsp-mode :pin "4898d35ace")
(package! lsp-ui :pin "242dfe859c")
(when (featurep! :completion company)
  (package! company-lsp :pin "f921ffa0cd"))
(when (featurep! :completion ivy)
  (package! lsp-ivy :pin "81e81ced99"))
(when (featurep! :completion helm)
  (package! helm-lsp :pin "db243993ea"))
