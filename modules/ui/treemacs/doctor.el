;;; ui/treemacs/doctor.el -*- lexical-binding: t; -*-

(assert! (and (not (featurep! :tools lsp +eglot))
              (featurep! +lsp))
         "+lsp flag is not supported with eglot, only with lsp-mode.")
