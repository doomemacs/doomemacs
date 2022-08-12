;; -*- no-byte-compile: t; -*-
;;; ui/treemacs/packages.el

(package! treemacs :pin "b18a05b1f62074a40e6011d83cd4c92cbee040dd")
;; These packages have no :pin because they're in the same repo
(when (modulep! :editor evil +everywhere)
  (package! treemacs-evil))
(package! treemacs-projectile)
(when (modulep! :tools magit)
  (package! treemacs-magit))
(when (modulep! :ui workspaces)
  (package! treemacs-persp))
(when (modulep! +lsp)
  (package! lsp-treemacs :pin "355e468b7fa9887c616a8bfe873d8e456303b67b"))
