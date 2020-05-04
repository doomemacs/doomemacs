;; -*- no-byte-compile: t; -*-
;;; lang/swift/packages.el

(package! swift-mode :pin "2ab9ea1784a12a482ed9e3fb284b7a7658f40fff")

(if (featurep! +lsp)
    (package! lsp-sourcekit :pin "04d75b6a0be5894fea4a55fec0b2ccedf5b3be58")
  (when (featurep! :completion company)
    (package! company-sourcekit :pin "abf9bc5a0102eb666d3aa6d6bf22f6efcc852781"))
  (when (featurep! :checkers syntax)
    (package! flycheck-swift :pin "4c5ad401252400a78da395fd56a71e67ff8c2761")))
