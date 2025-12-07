;; -*- no-byte-compile: t; -*-
;;; lang/zig/packages.el

(package! zig-mode :pin "20e395f940afe1e19e965050b0284ec418d6a9d5")

(when (and (modulep! +tree-sitter) (treesit-available-p))
  (package! zig-ts-mode :pin "3898b70d6f72da688e086323fa2922f1542d1318"))
