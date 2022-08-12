;;; ui/treemacs/doctor.el -*- lexical-binding: t; -*-

(assert! (not (and (modulep! +lsp)
                   (modulep! :tools lsp +eglot)))
         "+lsp flag is not supported with eglot, only with lsp-mode.")
