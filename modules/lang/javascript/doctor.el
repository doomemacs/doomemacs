;; -*- lexical-binding: t; no-byte-compile: t; -*-
;;; lang/javascript/doctor.el

(assert! (or (not (modulep! +lsp))
             (modulep! :tools lsp))
         "This module requires (:tools lsp)")

(assert! (or (not (modulep! +tree-sitter))
             (modulep! :tools tree-sitter))
         "This module requires (:tools tree-sitter)")

(when (not (modulep! +tree-sitter))
  (warn! "You are using the JavaScript module without tree sitter. You should enable tree sitter for much better support, including TypeScript and JSX."))
