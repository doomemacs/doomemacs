;; -*- no-byte-compile: t; -*-
;;; lang/haskell/packages.el

(package! haskell-mode)

(when (featurep! +dante)
  (package! dante)
  (package! attrap))
(when (featurep! +lsp)
  (package! lsp-haskell))
;; DEPRECATED
(when (featurep! +intero)
  (package! intero))
