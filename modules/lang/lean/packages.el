;; -*- no-byte-compile: t; -*-
;;; lang/lean/packages.el

(package! nael)
(when (featurep! :tools lsp)
  (package! nael-lsp))

(when (featurep! +v3)
  (package! lean-mode :pin "99d6a34dc5b12f6e996e9217fa9f6fe4a6af037a"))
