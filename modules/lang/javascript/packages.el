;; -*- no-byte-compile: t; -*-
;;; lang/javascript/packages.el

;; Major modes
(unless (modulep! +tree-sitter)
  (package! typescript-mode :pin "481df3ad2cdf569d8e6697679669ff6206fbd2f9"))

;; Extensions
(package! nodejs-repl :pin "c232b4964bd1c9f202fd515685b42c69af415f19")
