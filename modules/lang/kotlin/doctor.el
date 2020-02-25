;;; lang/kotlin/doctor.el -*- lexical-binding: t; -*-

(unless (executable-find "ktlint")
  (warn! "ktlint not found. flycheck-kotlin won't work."))

(assert! (or (not (featurep! +lsp))
             (featurep! :tools lsp))
         "This module requires (:tools lsp)")
