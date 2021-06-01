;;; lang/sh/doctor.el -*- lexical-binding: t; -*-

(when (featurep! :checkers syntax)
 (unless (executable-find "shellcheck")
  (warn! "Couldn't find shellcheck. Shell script linting will not work")))

(when (featurep! :editor format)
  (unless (executable-find "shfmt")
    (warn! "Couldn't find shfmt. Code formatting in sh buffers will not work.")))
