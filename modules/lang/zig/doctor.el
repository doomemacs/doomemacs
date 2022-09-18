;; -*- lexical-binding: t; no-byte-compile: t; -*-
;;; lang/zig/doctor.el

(assert! (or (not (modulep! +lsp))
             (modulep! :tools lsp))
         "This module requires (:tools lsp)")

(assert! (or (not (modulep! +tree-sitter))
             (modulep! :tools tree-sitter))
         "This module requires (:tools tree-sitter)")

(unless (executable-find "zig")
  (warn! "Couldn't find zig binary"))

(when (modulep! +lsp)
  (unless (executable-find "zls")
    (warn! "Couldn't find zls binary")))
