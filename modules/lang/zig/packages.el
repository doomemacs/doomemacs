;; -*- no-byte-compile: t; -*-
;;; lang/zig/packages.el

(package! zig-mode :pin "20e395f940afe1e19e965050b0284ec418d6a9d5")

(when (and (modulep! +tree-sitter) (treesit-available-p))
  (package! zig-ts-mode :pin "89b52c865c64d4f887c94445e3283486cf358782"))
