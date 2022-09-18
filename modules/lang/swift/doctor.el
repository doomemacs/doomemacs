;;; lang/swift/doctor.el -*- lexical-binding: t; -*-

(assert! (or (not (modulep! +tree-sitter))
             (modulep! :tools tree-sitter))
         "This module requires (:tools tree-sitter)")
