;; -*- no-byte-compile: t; -*-
;;; ui/treemacs/packages.el

(package! treemacs :pin "9eaef270de9fde50ef029c12db6d8b89cc4580d8")
;; These packages have no :pin because they're in the same repo
(when (featurep! :editor evil +everywhere)
  (package! treemacs-evil))
(package! treemacs-projectile)
(when (featurep! :tools magit)
  (package! treemacs-magit))
(when (featurep! :ui workspaces)
  (package! treemacs-persp))
(when (featurep! +lsp)
  (package! lsp-treemacs :pin "4cfb46d7fe69cc537a8a86389c5d8d9fd3fbfabe"))
