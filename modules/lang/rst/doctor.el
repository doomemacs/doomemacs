;;; lang/rst/doctor.el -*- lexical-binding: t; -*-

(when (modulep! :editor format)
  (unless (executable-find "rstfmt")
    (warn! "Couldn't find rstfmt. Formatting will be disabled.")))
