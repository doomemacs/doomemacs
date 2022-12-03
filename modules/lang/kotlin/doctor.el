;;; lang/kotlin/doctor.el -*- lexical-binding: t; -*-

(unless (executable-find "ktlint")
  (warn! "ktlint not found. flycheck-kotlin won't work."))

(assert! (or (not (modulep! +lsp))
             (modulep! :tools lsp))
         "This module requires (:tools lsp)")
