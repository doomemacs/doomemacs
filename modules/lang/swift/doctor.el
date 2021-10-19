;;; lang/swift/doctor.el -*- lexical-binding: t; -*-

(assert! (or (not (featurep! +tree-sitter))
             (featurep! :tools tree-sitter))
         "This module requires (:tools tree-sitter)")
