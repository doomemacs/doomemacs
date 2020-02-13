;; -*- no-byte-compile: t; -*-
;;; tools/lsp/packages.el

(package! lsp-mode :pin "e7c7abf236")
(package! lsp-ui :pin "6caacc8c93")
(when (featurep! :completion company)
  (package! company-lsp :pin "f921ffa0cd"))
(when (featurep! :completion ivy)
  (package! lsp-ivy :pin "78c1429c62"))
(when (featurep! :completion helm)
  (package! helm-lsp :pin "6f62659cc5"))
