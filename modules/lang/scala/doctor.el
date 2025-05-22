;;; lang/scala/doctor.el -*- lexical-binding: t; -*-

(assert! (or (not (modulep! +lsp))
             (modulep! :tools lsp))
         "This module requires (:tools lsp)")

(assert! (or (not (modulep! +tree-sitter))
             (modulep! :tools tree-sitter))
         "This module requires (:tools tree-sitter)")

(assert! (or (not (modulep! +tree-sitter))
             (version< emacs-version "29.1"))
         "Emacs 29.1+ is required for tree-sitter support")

(if (and (modulep! +lsp)
         (not (executable-find "metals")))
    (warn! "metals isn't installed"))

(when (modulep! :editor format)
  (unless (executable-find "scalafmt")
    (warn! "Couldn't find scalafmt. Formatting will be disabled.")))
