;; -*- lexical-binding: t; no-byte-compile: t; -*-
;;; lang/zig/doctor.el

(assert! (or (modulep! -lsp)
             (modulep! :tools lsp))
         "This module requires (:tools lsp)")

(assert! (or (modulep! -tree-sitter)
             (modulep! :tools tree-sitter))
         "This module requires (:tools tree-sitter)")

(unless (executable-find "zig")
  (warn! "Couldn't find zig binary")

  (unless (modulep! :editor format)
    (warn! "Formatting will be disabled")))

(when (modulep! +lsp)
  (unless (executable-find "zls")
    (warn! "Couldn't find zls binary")))
