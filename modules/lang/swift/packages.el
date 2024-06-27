;; -*- no-byte-compile: t; -*-
;;; lang/swift/packages.el

(package! swift-mode :pin "b06c97b909418b5dca3d15351b2ce86a04df32fb")

(if (modulep! +lsp)
    (unless (modulep! :tools lsp +eglot)
      (package! lsp-sourcekit :pin "bb2b7e027824b8690cc6f95181d7abcec3c34a01"))
  (when (modulep! :completion company)
    (package! company-sourcekit :pin "a1860ad4dd3a542acd2fa0dfac2a388cbdf4af0c"))
  (when (and (modulep! :checkers syntax)
             (not (modulep! :checkers syntax +flymake)))
    (package! flycheck-swift :pin "4c5ad401252400a78da395fd56a71e67ff8c2761")))
