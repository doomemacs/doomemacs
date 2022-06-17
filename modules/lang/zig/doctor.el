;; -*- lexical-binding: t; no-byte-compile: t; -*-
;;; lang/zig/doctor.el

(assert! (or (not (featurep! +lsp))
             (featurep! :tools lsp))
         "This module requires (:tools lsp)")

(assert! (or (not (featurep! +tree-sitter))
             (featurep! :tools tree-sitter))
         "This module requires (:tools tree-sitter)")

(unless (executable-find "zig")
  (warn! "Couldn't find zig binary"))

(when (featurep! +lsp)
  (unless (executable-find "zls")
    (warn! "Couldn't find zls binary")))
