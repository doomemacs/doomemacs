;;; checkers/spell/doctor.el -*- lexical-binding: t; -*-

(unless (executable-find "aspell")
  (warn! "Couldn't find aspell executable; spell checker will not work"))

(when (featurep! +hunspell)
  (unless (executable-find "hunspell")
    (warn! "Couldn't find hunspell executable; spell checker will not work")))
