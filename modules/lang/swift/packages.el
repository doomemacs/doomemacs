;; -*- no-byte-compile: t; -*-
;;; lang/swift/packages.el

(package! swift-mode :pin "e65a80a659c74d0a62b00dff183a0f7fc8385ce1")

(if (featurep! +lsp)
    (package! lsp-sourcekit :pin "948c3a35fd05496a77af2d8935e754db112cb4c3")
  (when (featurep! :completion company)
    (package! company-sourcekit :pin "abf9bc5a0102eb666d3aa6d6bf22f6efcc852781"))
  (when (featurep! :checkers syntax)
    (package! flycheck-swift :pin "4c5ad401252400a78da395fd56a71e67ff8c2761")))
