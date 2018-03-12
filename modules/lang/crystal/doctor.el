;;; lang/crystal/doctor.el -*- lexical-binding: t; -*-

(unless (executable-find "icr")
  (warn! "Couldn't find icr. REPL will not work"))
