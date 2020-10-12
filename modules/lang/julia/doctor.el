;;; lang/julia/doctor.el -*- lexical-binding: t; -*-

(assert! (or (not (featurep! +lsp))
             (featurep! :tools lsp))
         "This module requires (:tools lsp)")

(if (featurep! +lsp)
    (if (featurep! :tools lsp +eglot)
        (when (require 'eglot-jl nil t)
          (unless (zerop (car (apply #'doom-call-process
                                     `(,eglot-jl-julia-command
                                       ,(concat "--project=" eglot-jl-language-server-project)
                                       ,@eglot-jl-julia-flags
                                       "-e" "empty!(LOAD_PATH); push!(LOAD_PATH, \"@\"); using LanguageServer, SymbolServer"))))
            (warn! "Couldn't find LanguageServer.jl and/or SymbolServer.jl")))
      (when (require 'lsp-julia nil t)
        (unless (zerop (car (apply #'doom-call-process `(,lsp-julia-command ,@lsp-julia-flags "-e" "using LanguageServer, SymbolServer"))))
          (warn! "Couldn't find LanguageServer.jl and/or SymbolServer.jl")))))
