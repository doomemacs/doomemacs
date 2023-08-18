;; -*- no-byte-compile: t; -*-
;;; lang/haskell/packages.el

(package! haskell-mode :pin "3e146c1a89db257bb75c7b33fa2a5a1a85aabd51")

(when (and (modulep! +lsp)
           (not (modulep! :tools lsp +eglot)))
  (package! lsp-haskell :pin "918ffa2516a59c90f909b584f7c9968716c0e006"))
