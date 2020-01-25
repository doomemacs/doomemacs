;; -*- no-byte-compile: t; -*-
;;; lang/swift/packages.el

(package! swift-mode :pin "1268425311")

(if (featurep! +lsp)
    (package! lsp-sourcekit :pin "04d75b6a0b")
  (when (featurep! :completion company)
    (package! company-sourcekit :pin "abf9bc5a01"))
  (when (featurep! :checkers syntax)
    (package! flycheck-swift :pin "4c5ad40125")))
