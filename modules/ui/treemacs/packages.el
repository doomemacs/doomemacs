;; -*- no-byte-compile: t; -*-
;;; ui/treemacs/packages.el

(package! treemacs :pin "8c6df39f01a4d47fda2cc943645fa067f771b748")
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
           (not (modulep! :tools lsp +eglot)))
  (package! lsp-treemacs :pin "e54e74deb8150964e3c3024e1ec14295a34e2a3b"))
