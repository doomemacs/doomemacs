;; -*- no-byte-compile: t; -*-
;;; ui/treemacs/packages.el

(package! treemacs)
(when (featurep! :feature evil +everywhere)
  (package! treemacs-evil))
(package! treemacs-projectile)
