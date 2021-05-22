;; -*- no-byte-compile: t; -*-
;;; ui/treemacs/packages.el

(package! treemacs :pin "1a396fb66470736f3f76ae9342b20357426e371f")
;; These packages have no :pin because they're in the same repo
(when (featurep! :editor evil +everywhere)
  (package! treemacs-evil))
(package! treemacs-projectile)
(when (featurep! :tools magit)
  (package! treemacs-magit))
(when (featurep! :ui workspaces)
  (package! treemacs-persp))
(when (featurep! +lsp)
  (package! lsp-treemacs :pin "b07868740d6f7d364e496048cee00bce10a6ab33"))
