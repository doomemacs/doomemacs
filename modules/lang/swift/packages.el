;; -*- no-byte-compile: t; -*-
;;; lang/swift/packages.el

(package! swift-mode :pin "1244ee48de1895d33f55fed81fc90acda0c901f1")

(if (modulep! +lsp)
    (package! lsp-sourcekit :pin "468c641e35877e4e843f6b7c52a35937de562995")
  (when (modulep! :completion company)
    (package! company-sourcekit :pin "a1860ad4dd3a542acd2fa0dfac2a388cbdf4af0c"))
  (when (modulep! :checkers syntax)
    (package! flycheck-swift :pin "4c5ad401252400a78da395fd56a71e67ff8c2761")))
