;;; lang/dart/doctor.el -*- lexical-binding: t; -*-

(assert! (or (not (featurep! +lsp))
             (featurep! :tools lsp))
         "This module requires (:tools lsp)")

(unless (executable-find "dart")
  (warn! "Dart isn't on PATH."))
