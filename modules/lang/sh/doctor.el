;;; lang/sh/doctor.el -*- lexical-binding: t; -*-

(when (featurep! :tools flycheck)
 (unless (executable-find "shellcheck")
  (warn! "Couldn't find shellcheck. Shell script linting will not work")))
