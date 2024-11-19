;; -*- no-byte-compile: t; -*-
;;; lang/swift/packages.el

(package! swift-mode :pin "ab189d6e89ac4c0f776d691a41ddeaf9730260d1")

(if (modulep! +lsp)
    (unless (modulep! :tools lsp +eglot)
      (package! lsp-sourcekit :pin "63ff1ab638b655089077d17fdd728a48f8906e02"))
  (when (modulep! :completion company)
    (package! company-sourcekit :pin "a1860ad4dd3a542acd2fa0dfac2a388cbdf4af0c"))
  (when (modulep! :checkers syntax -flymake)
    (package! flycheck-swift :pin "4c5ad401252400a78da395fd56a71e67ff8c2761")))
