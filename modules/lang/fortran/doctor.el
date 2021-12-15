;; -*- lexical-binding: t; no-byte-compile: t; -*-
;;; lang/fortran/doctor.el

(assert! (or (not (featurep! +lsp))
             (featurep! :tools lsp))
         "This module requires (:tools lsp)")

(when (not (executable-find "gfortran"))
  (warn! "Couldn't find gfortran - compilation will not work."))

(unless (executable-find "fpm")
  (warn! "Couldn't find fpm - project building/testing will not work."))

(when (featurep! +lsp)
  (unless (executable-find "fortls")
    (warn! "Couldn't find fortls."))
  (unless (executable-find "fprettify")
    (warn! "Couldn't find fprettify.")))
