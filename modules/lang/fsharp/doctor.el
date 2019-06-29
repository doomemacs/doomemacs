;;; lang/fsharp/doctor.el -*- lexical-binding: t; -*-

(when (require 'fsharp-mode nil t)
  (unless (cl-some #'fsharp-mode--executable-find '("fsharpc" "fsc"))
    (warn! "Cannot find the F# compiler. Most features will not work.")))
