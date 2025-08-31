;; -*- no-byte-compile: t; -*-
;;; lang/javascript/packages.el

;; Major modes
(unless (modulep! +tree-sitter)
  (package! typescript-mode :pin "481df3ad2cdf569d8e6697679669ff6206fbd2f9"))

;; Extensions
(package! nodejs-repl :pin "130d49b073a50b7aad472ae8cd05848a9840e480")
