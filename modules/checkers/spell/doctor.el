;;; checkers/spell/doctor.el -*- lexical-binding: t; -*-

(when (or (modulep! -flyspell)
          (modulep! +aspell))
  (unless (executable-find "aspell")
    (warn! "Couldn't find aspell executable; spell checker will not work")))

(when (modulep! +hunspell)
  (unless (executable-find "hunspell")
    (warn! "Couldn't find hunspell executable; spell checker will not work")))
