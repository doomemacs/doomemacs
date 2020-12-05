;; -*- no-byte-compile: t; -*-
;;; lang/swift/packages.el

(package! swift-mode :pin "0bc8d8d80736d6d874edf8f59c0b37618368b183")

(if (featurep! +lsp)
    (package! lsp-sourcekit :pin "948c3a35fd05496a77af2d8935e754db112cb4c3")
  (when (featurep! :completion company)
    (package! company-sourcekit :pin "abf9bc5a0102eb666d3aa6d6bf22f6efcc852781"))
  (when (featurep! :checkers syntax)
    (package! flycheck-swift :pin "4c5ad401252400a78da395fd56a71e67ff8c2761")))
