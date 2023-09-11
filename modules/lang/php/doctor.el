;; -*- lexical-binding: t; no-byte-compile: t; -*-
;;; lang/php/doctor.el

(assert! (or (not (modulep! +lsp))
             (modulep! :tools lsp))
         "This module requires (:tools lsp)")

(assert! (or (not (modulep! +tree-sitter))
             (modulep! :tools tree-sitter))
         "This module requires (:tools tree-sitter)")

(unless (executable-find "php")
  (warn! "Couldn't find php in your PATH"))

(unless (executable-find "composer")
  (warn! "Couldn't find composer in your PATH"))
