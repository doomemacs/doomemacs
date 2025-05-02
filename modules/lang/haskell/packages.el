;; -*- no-byte-compile: t; -*-
;;; lang/haskell/packages.el

(package! haskell-mode :pin "e9c356739310332afe59b10ffa2e6c3e76f124e3")

(when (and (modulep! +lsp)
           (modulep! :tools lsp -eglot))
  (package! lsp-haskell :pin "081d5115ceb1f1647497a8a3de4ca0702aaadb48"))
