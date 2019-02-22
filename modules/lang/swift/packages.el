;; -*- no-byte-compile: t; -*-
;;; lang/swift/packages.el

(package! swift-mode)

(if (featurep! +lsp)
    (package! lsp-sourcekit)
  (when (featurep! :completion company)
    (package! company-sourcekit))
  (when (featurep! :feature syntax-checker)
    (package! flycheck-swift)))
