;; -*- no-byte-compile: t; -*-
;;; lang/haskell/packages.el

(package! haskell-mode)
(package! hindent)

(cond ((featurep! +dante)
       (package! dante)
       (package! attrap))
      ((package! intero)))

