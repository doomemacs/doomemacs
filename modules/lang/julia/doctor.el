;;; lang/julia/doctor.el -*- lexical-binding: t; -*-

(assert! (or (not (featurep! +lsp))
             (featurep! :tools lsp))
         "This module requires (:tools lsp)")

(when (featurep! +lsp)
  (let ((args
         (cond ((require 'eglot-jl nil t)
                `(,eglot-jl-julia-command
                  ,(concat "--project=" eglot-jl-language-server-project)
                  ,@eglot-jl-julia-flags
                  "-e" "empty!(LOAD_PATH); push!(LOAD_PATH, \"@\"); using LanguageServer, SymbolServer"))
               ((require 'lsp-julia nil t)
                `(,lsp-julia-command
                  ,@lsp-julia-flags
                  "-e" "using LanguageServer, SymbolServer")))))
    (unless (zerop (car (apply #'doom-call-process args)))
      (warn! "Couldn't find LanguageServer.jl and/or SymbolServer.jl"))))
