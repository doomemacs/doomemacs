;; -*- no-byte-compile: t; -*-
;;; lang/swift/packages.el

(package! swift-mode :pin "95ff0041370660e839ed06aa92330694d8590d62")

(if (featurep! +lsp)
    (package! lsp-sourcekit :pin "ff204ed820df8c3035ebdc4b5a583640d52caeeb")
  (when (featurep! :completion company)
    (package! company-sourcekit :pin "abf9bc5a0102eb666d3aa6d6bf22f6efcc852781"))
  (when (featurep! :checkers syntax)
    (package! flycheck-swift :pin "4c5ad401252400a78da395fd56a71e67ff8c2761")))
