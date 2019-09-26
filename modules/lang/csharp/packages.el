;; -*- no-byte-compile: t; -*-
;;; lang/csharp/packages.el

(package! csharp-mode)

(unless (featurep! +lsp)
  (package! omnisharp))

(when (featurep! +unity)
  (package! shader-mode))
