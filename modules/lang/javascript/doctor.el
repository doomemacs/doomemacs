;; -*- lexical-binding: t; no-byte-compile: t; -*-
;;; lang/javascript/doctor.el

(assert! (or (not (modulep! +lsp))
             (modulep! :tools lsp))
         "This module requires (:tools lsp)")

(assert! (or (not (modulep! +tree-sitter))
             (modulep! :tools tree-sitter))
         "This module requires (:tools tree-sitter)")

(unless (modulep! +tree-sitter)
  (warn! "Typescript support is degraded without +tree-sitter (and Emacs 29+)")
  (warn! "No JSX/TSX support without +tree-sitter (and Emacs 29+)"))
