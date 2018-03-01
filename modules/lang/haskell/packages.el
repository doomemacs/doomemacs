;; -*- no-byte-compile: t; -*-
;;; lang/haskell/packages.el

(package! haskell-mode)

;;
(cond ((featurep! +dante)
       (package! dante)
       (when (featurep! :completion company)
         (package! company-ghc)))
      (t
       (package! intero)
       (package! hindent)))

