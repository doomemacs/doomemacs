;; -*- no-byte-compile: t; -*-
;;; ui/treemacs/packages.el

(package! treemacs :pin "4d4a955fcb")
(when (featurep! :editor evil +everywhere)
  (package! treemacs-evil :pin "4d4a955fcb"))
(package! treemacs-projectile :pin "4d4a955fcb")
(when (featurep! :tools magit)
  (package! treemacs-magit :pin "4d4a955fcb"))
(when (featurep! :ui workspaces)
  (package! treemacs-persp :pin "4d4a955fcb"))
