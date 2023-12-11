;; -*- no-byte-compile: t; -*-
;;; lang/swift/packages.el

(package! swift-mode :pin "e58417ec871d260fe47183ffe4b7e47ac09bc682")

(if (modulep! +lsp)
    (unless (modulep! :tools lsp +eglot)
      (package! lsp-sourcekit :pin "1cd5e7d2699598a97bdbcd289d9a88b249db474c"))
  (when (modulep! :completion company)
    (package! company-sourcekit :pin "a1860ad4dd3a542acd2fa0dfac2a388cbdf4af0c"))
  (when (and (modulep! :checkers syntax)
             (not (modulep! :checkers syntax +flymake)))
    (package! flycheck-swift :pin "4c5ad401252400a78da395fd56a71e67ff8c2761")))
