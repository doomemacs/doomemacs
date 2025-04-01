;;; lang/fsharp/doctor.el -*- lexical-binding: t; -*-

(when (require 'fsharp-mode nil t)
  (unless (or (cl-some #'fsharp-mode--executable-find '("fsharpc" "fsc"))
              (and (executable-find "dotnet")
                   (zerop (car (doom-call-process "dotnet" "fsi" "--version")))))
    (warn! "Cannot find the F# compiler. Most features will not work.")))
