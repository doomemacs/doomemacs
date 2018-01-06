;;; ui/vi-tilde-fringe/config.el -*- lexical-binding: t; -*-

;; indicators for empty lines past EOF
(def-package! vi-tilde-fringe
  :hook ((prog-mode text-mode conf-mode) . vi-tilde-fringe-mode))

