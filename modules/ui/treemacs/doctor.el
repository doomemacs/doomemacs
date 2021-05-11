;;; ui/treemacs/doctor.el -*- lexical-binding: t; -*-

(assert! (not (and (featurep! +lsp)
                   (featurep! :tools lsp +eglot)))
         "+lsp flag is not supported with eglot, only with lsp-mode.")
