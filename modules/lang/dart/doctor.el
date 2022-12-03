;;; lang/dart/doctor.el -*- lexical-binding: t; -*-

(assert! (or (not (modulep! +lsp))
             (modulep! :tools lsp))
         "This module requires (:tools lsp)")

(unless (executable-find "dart")
  (warn! "Dart isn't on PATH."))
