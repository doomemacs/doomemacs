;; -*- no-byte-compile: t; -*-
;;; lang/swift/packages.el

(package! swift-mode :pin "0d1ef0ef183398143b13fb8c76c1284df0d5e616")

(if (featurep! +lsp)
    (package! lsp-sourcekit :pin "97ff36b228a61e69734c7180f33cc6951b1a600f")
  (when (featurep! :completion company)
    (package! company-sourcekit :pin "a1860ad4dd3a542acd2fa0dfac2a388cbdf4af0c"))
  (when (featurep! :checkers syntax)
    (package! flycheck-swift :pin "4c5ad401252400a78da395fd56a71e67ff8c2761")))
