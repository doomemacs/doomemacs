;;; lang/scala/doctor.el -*- lexical-binding: t; -*-

(assert! (or (not (featurep! +lsp))
             (featurep! :tools lsp))
         "This module requires (:tools lsp)")

(if (and (featurep! +lsp)
         (not (executable-find "metals-emacs")))
    (warn! "metals-emacs isn't installed"))
