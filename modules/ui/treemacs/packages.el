;; -*- no-byte-compile: t; -*-
;;; ui/treemacs/packages.el

(package! treemacs :pin "fe471314f198f9aff268637a766a6ade0a5b5d96")
;; These packages have no :pin because they're in the same repo
(when (modulep! :editor evil +everywhere)
  (package! treemacs-evil))
(package! treemacs-projectile)
(when (modulep! :tools magit)
  (package! treemacs-magit))
(when (modulep! :ui workspaces)
  (package! treemacs-persp))
(when (modulep! +lsp)
  (package! lsp-treemacs :pin "e66ae2196503d4e84334519e56b4388feffa5060"))
