;; -*- no-byte-compile: t; -*-
;;; lang/swift/packages.el

(package! swift-mode :pin "fc7df7bd906a2bb04aac6e0de47fc7acf33ceed3")

(if (modulep! +lsp)
    (unless (modulep! :tools lsp +eglot)
      (package! lsp-sourcekit :pin "30918cd1aeeda5cfbc0fd615f97cf1bf388d8f2d"))
  (when (modulep! :completion company)
    (package! company-sourcekit :pin "a1860ad4dd3a542acd2fa0dfac2a388cbdf4af0c"))
  (when (modulep! :checkers syntax -flymake)
    (package! flycheck-swift :pin "4c5ad401252400a78da395fd56a71e67ff8c2761")))

(when (and (modulep! +tree-sitter) (treesit-available-p))
  (package! swift-ts-mode :pin "43a0be79f9758fc444f5fafdff6023c4c7bf80f7"))
