;; -*- no-byte-compile: t; -*-
;;; ui/treemacs/packages.el

(package! treemacs :pin "5fa84199501fd43e5573b1277a2b1699c7473cc1")
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
  (package! lsp-treemacs :pin "3e5550f278db74f15ebe34add0138b138207ec08"))
