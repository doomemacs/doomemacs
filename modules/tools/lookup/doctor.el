;;; tools/lookup/doctor.el -*- lexical-binding: t; -*-

(when (modulep! +docsets)
  (unless (executable-find "sqlite3")
    (warn! "Couldn't find the sqlite3 executable. dash-docs will not work.")))
