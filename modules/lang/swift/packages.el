;; -*- no-byte-compile: t; -*-
;;; lang/swift/packages.el

(package! swift-mode :pin "ad12a3025156873995318b6a0480cd2459063bf7")

(if (featurep! +lsp)
    (package! lsp-sourcekit :pin "aafa9878a3df2f08e5a9c846d91fd53350ce3c99")
  (when (featurep! :completion company)
    (package! company-sourcekit :pin "abf9bc5a0102eb666d3aa6d6bf22f6efcc852781"))
  (when (featurep! :checkers syntax)
    (package! flycheck-swift :pin "4c5ad401252400a78da395fd56a71e67ff8c2761")))
