;;; lang/swift/doctor.el -*- lexical-binding: t; -*-

(assert! (or (not (modulep! +tree-sitter))
             (modulep! :tools tree-sitter))
         "This module requires (:tools tree-sitter)")

(when (modulep! :editor format)
  (unless (executable-find "swiftformat")
    (warn! "Couldn't find swiftformat. Formatting will be disabled.")))
