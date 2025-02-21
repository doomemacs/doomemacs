;; -*- no-byte-compile: t; -*-
;;; lang/swift/packages.el

(package! swift-mode :pin "2c0b2b72dc908652914b62a1e64b1d30144839ce")

(if (modulep! +lsp)
    (unless (modulep! :tools lsp +eglot)
      (package! lsp-sourcekit :pin "63ff1ab638b655089077d17fdd728a48f8906e02"))
  (when (modulep! :completion company)
    (package! company-sourcekit :pin "a1860ad4dd3a542acd2fa0dfac2a388cbdf4af0c"))
  (when (modulep! :checkers syntax -flymake)
    (package! flycheck-swift :pin "4c5ad401252400a78da395fd56a71e67ff8c2761")))
