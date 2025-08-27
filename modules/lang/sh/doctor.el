;;; lang/sh/doctor.el -*- lexical-binding: t; -*-

(when (modulep! :checkers syntax)
 (unless (executable-find "shellcheck")
  (warn! "Couldn't find shellcheck. Shell script linting will not work")))

(when (modulep! :editor format)
  (unless (executable-find "shfmt")
    (warn! "Couldn't find shfmt. Code formatting will not work.")))
