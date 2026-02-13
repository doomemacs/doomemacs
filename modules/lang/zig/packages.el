;; -*- no-byte-compile: t; -*-
;;; lang/zig/packages.el

(package! zig-mode :pin "20e395f940afe1e19e965050b0284ec418d6a9d5")

(when (and (modulep! +tree-sitter) (treesit-available-p))
  (package! zig-ts-mode :pin "64611c6d512e4c15e8d1e3fd0fde6bd47c6c8f50"))
