;; -*- no-byte-compile: t; -*-
;;; ui/treemacs/packages.el

(package! treemacs)
(when (featurep! :feature evil)
  (package! treemacs-evil))
(package! treemacs-projectile)
