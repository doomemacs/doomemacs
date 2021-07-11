;; -*- no-byte-compile: t; -*-
;;; lang/swift/packages.el

(package! swift-mode :pin "1b47a09f1c0e15c543e0551e7f1e643f437e7711")

(if (featurep! +lsp)
    (package! lsp-sourcekit :pin "ae4aa8705cc3a27ed86f1e7ee04d5c8f0522d8c0")
  (when (featurep! :completion company)
    (package! company-sourcekit :pin "a1860ad4dd3a542acd2fa0dfac2a388cbdf4af0c"))
  (when (featurep! :checkers syntax)
    (package! flycheck-swift :pin "4c5ad401252400a78da395fd56a71e67ff8c2761")))
