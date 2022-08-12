;;; lang/scala/doctor.el -*- lexical-binding: t; -*-

(assert! (or (not (modulep! +lsp))
             (modulep! :tools lsp))
         "This module requires (:tools lsp)")

(assert! (or (not (modulep! +tree-sitter))
             (modulep! :tools tree-sitter))
         "This module requires (:tools tree-sitter)")

(if (and (modulep! +lsp)
         (not (executable-find "metals-emacs")))
    (warn! "metals-emacs isn't installed"))
