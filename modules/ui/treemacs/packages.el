;; -*- no-byte-compile: t; -*-
;;; ui/treemacs/packages.el

(package! treemacs :pin "1820db42c6478455340a2bd962120b1e2b0bf3f6")
;; These packages have no :pin because they're in the same repo
(when (featurep! :editor evil +everywhere)
  (package! treemacs-evil))
(package! treemacs-projectile)
(when (featurep! :tools magit)
  (package! treemacs-magit))
(when (featurep! :ui workspaces)
  (package! treemacs-persp))
(when (featurep! +lsp)
  (package! lsp-treemacs :pin "c40a381730251039d33400cc14539c1e0729385f"))
