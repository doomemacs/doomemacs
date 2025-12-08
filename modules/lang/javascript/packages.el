;; -*- no-byte-compile: t; -*-
;;; lang/javascript/packages.el

;; Major modes
(unless (modulep! +tree-sitter)
  (package! typescript-mode :pin "2535780bdb318d86761b9bd21b0347ca6a89628f"))

;; Extensions
(package! nodejs-repl :pin "c232b4964bd1c9f202fd515685b42c69af415f19")
