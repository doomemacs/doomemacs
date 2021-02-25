;; -*- no-byte-compile: t; -*-
;;; lang/haskell/packages.el

(package! haskell-mode :pin "3a019e65b504861d7ea23afbfecd14e5ef63e846")

(when (featurep! +dante)
  (package! dante :pin "7b1ab644214e03b86dcf7436fd22a65cce2fa858")
  (package! attrap :pin "778382eba8e1a449862b1573e90c1e79cf5caeb1"))
(when (or (and (featurep! +lsp)
               (not (featurep! :tools lsp +eglot)))
          (featurep! +ghcide))
  (package! lsp-haskell :pin "7efbef3d206989faa8b691a4230a3ed872542187"))
