;; -*- no-byte-compile: t; -*-
;;; lang/haskell/packages.el

(package! haskell-mode)

(cond ((featurep! +dante)
       (package! dante)
       (package! attrap))
      ((featurep! +lsp)
       (package! lsp-haskell))
      ((featurep! +intero)  ; DEPRECATED
       (package! intero)))
