;; -*- no-byte-compile: t; -*-
;;; lang/zig/packages.el

(package! zig-mode :pin "c46d024733b7c1d6af829bb610fc9629b060bc9e")

(when (and (modulep! +tree-sitter)
           (fboundp 'treesit-available-p))
  (package! zig-ts-mode :pin "3898b70d6f72da688e086323fa2922f1542d1318"))
