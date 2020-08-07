;; -*- no-byte-compile: t; -*-
;;; lang/windows-scripts/packages.el

(package! powershell-mode)
(when(featurep! +lsp)
  (package! lsp-mode))
