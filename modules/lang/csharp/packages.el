;; -*- no-byte-compile: t; -*-
;;; lang/csharp/packages.el

(package! csharp-mode)
(package! omnisharp)

(when (featurep! +unity)
  (package! shader-mode))
