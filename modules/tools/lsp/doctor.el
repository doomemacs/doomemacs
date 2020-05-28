;;; tools/lsp/doctor.el -*- lexical-binding: t; -*-

(assert! (not (and (featurep! +eglot) (featurep! +peek))) "+eglot and +peek flags are not compatible. Peek uses lsp-mode, while Eglot is another package altogether for LSP.")
