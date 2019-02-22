;; -*- no-byte-compile: t; -*-
;;; tools/lsp/packages.el

(package! lsp-mode)
(package! lsp-ui)
(when (featurep! :completion company)
  (package! company-lsp))
