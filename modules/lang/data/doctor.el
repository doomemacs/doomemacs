;;; lang/data/doctor.el -*- lexical-binding: t; -*-

(when (modulep! :editor format)
  (unless (executable-find +data-xmllint-binary)
    (warn! "Couldn't find xmllint. Formatting will be disabled.")))
