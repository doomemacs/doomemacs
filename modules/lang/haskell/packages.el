;; -*- no-byte-compile: t; -*-
;;; lang/haskell/packages.el

(package! haskell-mode)

;;
(cond ((featurep! +dante)
       (package! dante))
      (t
       (package! intero)
       (package! hindent)))

