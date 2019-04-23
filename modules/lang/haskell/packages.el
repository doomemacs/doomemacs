;; -*- no-byte-compile: t; -*-
;;; lang/haskell/packages.el

(package! haskell-mode)

(cond ((featurep! +dante)
       (package! dante)
       (package! attrap))
      ((featurep! +intero)
       (package! intero))
      ((featurep! +lsp)
       (package! lsp-haskell)))
