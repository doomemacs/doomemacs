;;; lang/sh/doctor.el -*- lexical-binding: t; -*-

(when (featurep! :checkers syntax)
 (unless (executable-find "shellcheck")
  (warn! "Couldn't find shellcheck. Shell script linting will not work")))

(assert! (or (not (featurep! +tree-sitter))
             (featurep! :tools tree-sitter))
         "This module requires (:tools tree-sitter)")
