;; -*- no-byte-compile: t; -*-
;;; ui/treemacs/packages.el

(package! treemacs :pin "96a808f06760ec8c595d27eb5c991ea905c948f6")
(package! treemacs-nerd-icons :pin "eac9fb5d92b8b29e7c4fcf9f3baddb2cb0b04575")
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
  (package! lsp-treemacs :pin "312dee2b3ab776868c2b367d0ac15259689d981a"))
