;; -*- no-byte-compile: t; -*-
;;; lang/haskell/packages.el

(package! haskell-mode :pin "79eaf444a72109f93f552abb53f834cc63bbf9f2")

(when (and (modulep! +lsp)
           (not (modulep! :tools lsp +eglot)))
  (package! lsp-haskell :pin "89d16370434e9a247e95b8b701f524f5abfc884b"))
