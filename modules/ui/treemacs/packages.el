;; -*- no-byte-compile: t; -*-
;;; ui/treemacs/packages.el

(package! treemacs :pin "54ef590b7621032a76f8f3e307032fc13c802371")
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
  (package! lsp-treemacs :pin "1d43e9e0307f84496a4a7ddf9dba481000391dbd"))
