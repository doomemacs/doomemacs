;;; lang/fsharp/doctor.el -*- lexical-binding: t; -*-

(unless (-any #'fsharp-mode--executable-find '("fsharpc" "fsc"))
  (warn! "Cannot find the F# compiler. Most features will not work."))
