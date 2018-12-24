;; -*- no-byte-compile: t; -*-
;;; lang/haskell/packages.el

(package! haskell-mode)

(cond
 ((featurep! +lsp) (depends-on! :tools lsp)
  (package! lsp-haskell)))
 ((featurep! +dante)
       (package! dante)
       (package! attrap)
       (when (featurep! :completion company)
         (package! company-ghc)))
      (t
       (package! intero)
       (package! hindent))
