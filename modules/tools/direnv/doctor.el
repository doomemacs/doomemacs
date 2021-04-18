;;; tools/direnv/doctor.el -*- lexical-binding: t; -*-

(unless (executable-find "direnv")
  (warn! "Couldn't find direnv executable"))
