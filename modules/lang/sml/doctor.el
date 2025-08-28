;;; lang/sml/doctor.el -*- lexical-binding: t; -*-

(assert! (or (not (modulep! +tree-sitter))
             (modulep! :tools tree-sitter))
         "This module requires (:tools tree-sitter)")

(when (modulep! :editor format)
  (unless (executable-find "smlformat")
    (warn! "Couldn't find smlformat. Formatting will be disabled.")))
