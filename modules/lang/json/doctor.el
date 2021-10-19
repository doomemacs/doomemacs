;;; lang/json/doctor.el -*- lexical-binding: t; -*-

(when (and (featurep! :completion ivy)
           (not (executable-find "jq")))
  (warn! "Couldn't find jq. counsel-jq won't work." ))

(assert! (or (not (featurep! +tree-sitter))
             (featurep! :tools tree-sitter))
         "This module requires (:tools tree-sitter)")
