;; -*- no-byte-compile: t; -*-
;;; lang/zig/packages.el

(package! zig-mode :pin "dab3c37c2d045040e3a7ce970731b66c9d5fb29b")

(when (and (modulep! +tree-sitter) (treesit-available-p))
  (package! zig-ts-mode :pin "3898b70d6f72da688e086323fa2922f1542d1318"))
