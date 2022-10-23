;; -*- no-byte-compile: t; -*-
;;; lang/swift/packages.el

(package! swift-mode :pin "f4529efa453f2f3051e2c8f772cd9da80147b93d")

(if (modulep! +lsp)
    (package! lsp-sourcekit :pin "f877659babd3b5f8ec09a8ad7d08193d95b6822e")
  (when (modulep! :completion company)
    (package! company-sourcekit :pin "a1860ad4dd3a542acd2fa0dfac2a388cbdf4af0c"))
  (when (modulep! :checkers syntax)
    (package! flycheck-swift :pin "4c5ad401252400a78da395fd56a71e67ff8c2761")))
