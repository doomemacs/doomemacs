;; -*- no-byte-compile: t; -*-
;;; ui/treemacs/packages.el

(package! treemacs :pin "2ab5a3c89fa01bbbd99de9b8986908b2bc5a7b49")
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
  (package! lsp-treemacs :pin "49df7292c521b4bac058985ceeaf006607b497dd"))
