;; -*- no-byte-compile: t; -*-
;;; lang/swift/packages.el

(package! swift-mode :pin "d266fbd300a1bf1592e1462ead4be093b8b68f98")

(if (featurep! +lsp)
    (package! lsp-sourcekit :pin "130f7a8f7a37869515953aa7715b3b969c3d7a0b")
  (when (featurep! :completion company)
    (package! company-sourcekit :pin "abf9bc5a0102eb666d3aa6d6bf22f6efcc852781"))
  (when (featurep! :checkers syntax)
    (package! flycheck-swift :pin "4c5ad401252400a78da395fd56a71e67ff8c2761")))
