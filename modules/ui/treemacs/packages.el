;; -*- no-byte-compile: t; -*-
;;; ui/treemacs/packages.el

(package! treemacs :pin "b18a05b1f62074a40e6011d83cd4c92cbee040dd")
;; These packages have no :pin because they're in the same repo
(when (featurep! :editor evil +everywhere)
  (package! treemacs-evil))
(package! treemacs-projectile)
(when (featurep! :tools magit)
  (package! treemacs-magit))
(when (featurep! :ui workspaces)
  (package! treemacs-persp))
(when (featurep! +lsp)
  (package! lsp-treemacs :pin "355e468b7fa9887c616a8bfe873d8e456303b67b"))
