;;; lang/julia/doctor.el -*- lexical-binding: t; -*-

(assert! (or (not (featurep! +lsp))
             (featurep! :tools lsp))
         "This module requires (:tools lsp)")

(if (featurep! +lsp)
    (when (require 'lsp-julia nil t)
      (unless (zerop (car (apply #'doom-call-process `(,lsp-julia-command ,@lsp-julia-flags "-e" "using LanguageServer"))))
        (warn! "Couldn't find LanguageServer.jl"))))
