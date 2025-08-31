;; -*- no-byte-compile: t; -*-
;;; lang/swift/packages.el

(package! swift-mode :pin "e30b9d46e031fd25e794f00f23b6427f44f7d221")

(if (modulep! +lsp)
    (unless (modulep! :tools lsp +eglot)
      (package! lsp-sourcekit :pin "1fb230109e90303bd36ad75b31e673ddcf928629"))
  (when (modulep! :completion company)
    (package! company-sourcekit :pin "a1860ad4dd3a542acd2fa0dfac2a388cbdf4af0c"))
  (when (modulep! :checkers syntax -flymake)
    (package! flycheck-swift :pin "4c5ad401252400a78da395fd56a71e67ff8c2761")))

(when (modulep! +tree-sitter)
  (package! swift-ts-mode :pin "43a0be79f9758fc444f5fafdff6023c4c7bf80f7"))
