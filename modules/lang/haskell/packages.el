;; -*- no-byte-compile: t; -*-
;;; lang/haskell/packages.el

(package! haskell-mode)
(when (featurep! :completion company)
  (package! company-ghc))

;;
(cond ((featurep! +dante)
       (package! dante))
      (t
       (package! intero)
       (package! hindent)))

