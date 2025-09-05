;;; lang/scad/config.el -*- lexical-binding: t; -*-

(use-package! scad-mode
  :config
  (when (modulep! +lsp)
    (add-hook 'scad-mode-local-vars-hook #'lsp! 'append))
  (map! (:localleader
         (:map (scad-mode-map)
               "e" #'scad-export
               "o" #'scad-open
               "p" #'scad-preview))))
