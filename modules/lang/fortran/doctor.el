;; -*- lexical-binding: t; no-byte-compile: t; -*-
;;; lang/fortran/doctor.el

(assert! (or (not (modulep! +lsp))
             (modulep! :tools lsp))
         "This module requires (:tools lsp)")

(when (and (modulep! +intel)
           (not (executable-find "ifort")))
  (warn! "Couldn't find Intel ifort - compilation will not work."))

(when (and (not (modulep! +intel))
           (not (executable-find "gfortran")))
  (warn! "Couldn't find gfortran - compilation will not work."))

(unless (executable-find "fpm")
  (warn! "Couldn't find fpm - project building/testing will not work."))

(when (modulep! +lsp)
  (unless (executable-find "fortls")
    (warn! "Couldn't find fortls. Language features will be disabled.")))

(when (modulep! :editor format)
  (unless (executable-find "fprettify")
    (warn! "Couldn't find fprettify. Formatting will be disabled.")))
