;; -*- no-byte-compile: t; -*-
;;; lang/haskell/packages.el

(package! haskell-mode :pin "8402caa341d90b4236f5c0a802751f9023ccfbe7")

(when (and (featurep! +lsp)
           (not (featurep! :tools lsp +eglot)))
  (package! lsp-haskell :pin "4e62cf897dd9e9fcef25c6e8e483490a07a5d439"))
