;;; lang/data/doctor.el -*- lexical-binding: t; -*-

(when (modulep! :editor format)
  (unless (executable-find "xmllint")
    (warn! "Couldn't find xmllint. Formatting will be disabled.")))
