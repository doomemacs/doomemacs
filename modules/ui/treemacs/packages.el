;; -*- no-byte-compile: t; -*-
;;; ui/treemacs/packages.el

(package! treemacs :pin "202d7f01d61359bb4b2a775d8d8eeb726d91a086")
(package! treemacs-nerd-icons :pin "9876cb478145a0ec4e36f64ff6583f3de7126216")
;; These packages have no :pin because they're in the same repo
(when (modulep! :editor evil +everywhere)
  (package! treemacs-evil))
(package! treemacs-projectile)
(when (modulep! :tools magit)
  (package! treemacs-magit))
(when (modulep! :ui workspaces)
  (package! treemacs-persp))
(when (and (modulep! +lsp)
           (modulep! :tools lsp -eglot))
  (package! lsp-treemacs :pin "fb1a07ae0a3d781dea8ac78da2933e0173eb48a4"))
