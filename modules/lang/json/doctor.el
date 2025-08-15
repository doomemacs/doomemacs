;;; lang/json/doctor.el -*- lexical-binding: t; -*-

(when (and (modulep! :completion ivy)
           (not (executable-find "jq")))
  (warn! "Couldn't find jq. counsel-jq won't work." ))

(assert! (or (modulep! -tree-sitter)
             (modulep! :tools tree-sitter))
         "This module requires (:tools tree-sitter)")
