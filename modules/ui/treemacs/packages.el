;; -*- no-byte-compile: t; -*-
;;; ui/treemacs/packages.el

(package! treemacs :pin "332d4e0f1f606c472dd083c9cdd4f143ee23020a")
;; These packages have no :pin because they're in the same repo
(when (featurep! :editor evil +everywhere)
  (package! treemacs-evil))
(package! treemacs-projectile)
(when (featurep! :tools magit)
  (package! treemacs-magit))
(when (featurep! :ui workspaces)
  (package! treemacs-persp))
