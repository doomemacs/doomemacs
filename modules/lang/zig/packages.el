;; -*- no-byte-compile: t; -*-
;;; lang/zig/packages.el

(package! zig-mode :pin "20e395f940afe1e19e965050b0284ec418d6a9d5")

(when (and (modulep! +tree-sitter) (treesit-available-p))
  (package! zig-ts-mode :pin "bb1e8287800868ee338e986bda5b5a1f5abf7445"))
