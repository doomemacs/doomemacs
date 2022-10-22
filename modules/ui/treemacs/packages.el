;; -*- no-byte-compile: t; -*-
;;; ui/treemacs/packages.el

(package! treemacs :pin "76807bef3c128aca33064228a80c5e6937a67b8c")
;; These packages have no :pin because they're in the same repo
(when (modulep! :editor evil +everywhere)
  (package! treemacs-evil))
(package! treemacs-projectile)
(when (modulep! :tools magit)
  (package! treemacs-magit))
(when (modulep! :ui workspaces)
  (package! treemacs-persp))
(when (modulep! +lsp)
  (package! lsp-treemacs :pin "2894e6dec583eaa77037627e9d8c3bc89cf7273d"))
