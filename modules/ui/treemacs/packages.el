;; -*- no-byte-compile: t; -*-
;;; ui/treemacs/packages.el

(package! treemacs :pin "05333cc23ca4349cd839cf1c18e1eaef1f6b70ec")
(package! treemacs-nerd-icons :pin "0c5ddcb978da639f01ddb023febc40fc755171e5")
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
