;; -*- no-byte-compile: t; -*-
;;; lang/haskell/packages.el

(package! haskell-mode :pin "98ba3922360199d5260d47f417f096730ad057c5")

(when (and (featurep! +lsp)
           (not (featurep! :tools lsp +eglot)))
  (package! lsp-haskell :pin "4e62cf897dd9e9fcef25c6e8e483490a07a5d439"))
