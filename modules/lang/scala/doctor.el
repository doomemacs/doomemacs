;;; lang/scala/doctor.el -*- lexical-binding: t; -*-

(assert! (or (not (featurep! +lsp))
             (featurep! :tools lsp))
         "This module requires (:tools lsp)")

(assert! (or (not (featurep! +tree-sitter))
             (featurep! :tools tree-sitter))
         "This module requires (:tools tree-sitter)")

(if (and (featurep! +lsp)
         (not (executable-find "metals-emacs")))
    (warn! "metals-emacs isn't installed"))
