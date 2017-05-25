;; -*- no-byte-compile: t; -*-
;;; lang/haskell/packages.el

(package! haskell-mode)
(package! dante)

(when (featurep! :completion company)
  (package! company-ghc))
