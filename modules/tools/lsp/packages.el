;; -*- no-byte-compile: t; -*-
;;; tools/lsp/packages.el

(package! lsp-mode :pin "401d72462f")
(package! lsp-ui :pin "242dfe859c")
(when (featurep! :completion company)
  (package! company-lsp :pin "f921ffa0cd"))
(when (featurep! :completion ivy)
  (package! lsp-ivy :pin "a6b7841e08"))
(when (featurep! :completion helm)
  (package! helm-lsp :pin "6f62659cc5"))
