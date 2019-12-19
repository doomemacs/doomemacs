;; -*- no-byte-compile: t; -*-
;;; tools/lsp/packages.el

(package! lsp-mode)
(package! lsp-ui)
(when (featurep! :completion company)
  (package! company-lsp))
(when (featurep! :completion ivy)
  (package! lsp-ivy))
(when (featurep! :completion helm)
  (package! helm-lsp))
