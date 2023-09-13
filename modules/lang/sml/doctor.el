;;; lang/sml/doctor.el -*- lexical-binding: t; -*-

(when (modulep! :editor format)
  (unless (executable-find "smlformat")
    (warn! "Couldn't find smlformat. Formatting will be disabled.")))
